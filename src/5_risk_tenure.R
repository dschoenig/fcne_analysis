source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

# region <- "cam"
# n.threads <- 1

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.effects <- paste0(path.effects, region, ".risk.tenure.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

message("Aggregating observations …")
data.proc <- readRDS(file.data)

# data.proc <- data.proc[1:5e4,]

n.min <- 1
forest_types <- c(all = "TRUE", primary = "for_type == 'primary'")
groups <- list()

for(i in seq_along(forest_types)) {

  data.forest <- data.proc[eval(parse(text = forest_types[i]))]

  groups.bl <-
    data.forest[it_type == "none" & pa_type == "none"] |>
    ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
    subset(n >= n.min)
  groups.bl_adm <-
    data.forest[it_type == "none" & pa_type == "none"] |>
    ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type")) |>
    subset(n >= n.min)
  groups.it <-
    data.forest[it_type != "none"] |>
    ids_by_group(id.col = "id", group.vars = c("it_type")) |>
    subset(n >= n.min)
  groups.pa <-
    data.forest[pa_type != "none"] |>
    ids_by_group(id.col = "id", group.vars = c("pa_type")) |>
    subset(n >= n.min)
  groups.it_pa <-
    data.forest[it_type != "none" | pa_type != "none"] |>
    ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
    subset(n >= n.min)
  groups.adm_it <-
    data.forest[it_type != "none"] |>
    ids_by_group(id.col = "id", group.vars = c("adm0", "it_type")) |>
    subset(n >= n.min)
  groups.adm_pa <-
    data.forest[pa_type != "none"] |>
    ids_by_group(id.col = "id", group.vars = c("adm0", "pa_type")) |>
    subset(n >= n.min)
  groups.adm_it_pa <-
    data.forest[it_type != "none" | pa_type != "none"] |>
    ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type")) |>
    subset(n >= n.min)

  groups[[names(forest_types)[i]]] <-
    rbindlist(list(groups.bl,
                   groups.bl_adm,
                   groups.it,
                   groups.pa,
                   groups.it_pa,
                   groups.adm_it,
                   groups.adm_pa,
                   groups.adm_it_pa), fill = TRUE)

}

groups <- rbindlist(groups, fill = TRUE, idcol = "for_type")

groups[for_type == "all", for_type := NA]
groups[!is.na(for_type), group.label := paste0("for_type.", for_type, ":",
                                               group.label)]
groups$group.id <- 1:nrow(groups)

setcolorder(groups, c("group.id", "group.label", "for_type",
                      "adm0", "it_type", "pa_type"))

rm(data.proc)


id.list <- groups$ids
names(id.list) <- groups$group.label

effects.partial <- list()
name.par <- "full"

ds <- open_dataset(paste0(path.arrow, "partial=", name.par), format = "arrow")
message(paste0("Evaluating effects for region `", region,
               "`, using (partial) linear predictor `", name.par,
               "` (", length(draw.ids), " draws) …"))
effects.partial$r <- aggregate_variables(ds,
                                         agg.fun = E,
                                         trans.fun = inv_cloglog,
                                         ids = id.list,
                                         draw.ids = draw.ids,
                                         draw.chunk = 100,
                                         # draw.chunk = 1000,
                                         agg.size = 1e6,
                                         n.threads = n.threads,
                                         gc = TRUE
                                         )

effects.partial$groups <- groups

gc()

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects.partial, file.effects)

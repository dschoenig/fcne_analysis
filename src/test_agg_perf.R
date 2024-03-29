source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

region <- "cam"
n.threads <- 4

path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.effects <- paste0(path.effects, region, ".eff.tenure.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

marginals <- c("full","ten_loc")
draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

message("Aggregating observations …")
data.proc <- readRDS(file.data)

data.proc <- data.proc[(50*5e4+1):(51*5e4),]

n.min <- 1
groups.bl <-
  data.proc[it_type == "none" & pa_type == "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
  subset(n >= n.min)
groups.bl_adm <-
  data.proc[it_type == "none" & pa_type == "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type")) |>
  subset(n >= n.min)
groups.it <-
  data.proc[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type")) |>
  subset(n >= n.min)
groups.pa <-
  data.proc[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("pa_type")) |>
  subset(n >= n.min)
groups.it_pa <-
  data.proc[it_type != "none" | pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
  subset(n >= n.min)
groups.adm_it <-
  data.proc[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type")) |>
  subset(n >= n.min)
groups.adm_pa <-
  data.proc[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "pa_type")) |>
  subset(n >= n.min)
groups.adm_it_pa <-
  data.proc[it_type != "none" | pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type")) |>
  subset(n >= n.min)

groups <- rbindlist(list(groups.bl,
                         groups.bl_adm,
                         groups.it,
                         groups.pa,
                         groups.it_pa,
                         groups.adm_it,
                         groups.adm_pa,
                         groups.adm_it_pa), fill = TRUE)
groups$group.id <- 1:nrow(groups)
setcolorder(groups, c("group.id", "group.label", "adm0", "it_type", "pa_type"))

id.list <- groups$ids
names(id.list) <- groups$group.label

rm(data.proc)

# Define aggregation function: transform to response scale, then marginalize
# over geographic area (i.e. group identity).
agg.fun <- function(eta) mean(inv_logit(eta))

effects.mar <- list()
for(i in seq_along(marginals)) {
  ds <- open_dataset(paste0(path.arrow, "marginal=", marginals[i]), format = "arrow")
  message(paste0("Evaluating effects for region `", region,
                   "` over marginal `", marginals[i], "` (",
                   length(draw.ids), " draws) …"))
  system.time({
  effects.mar[[i]] <- aggregate_variables(ds,
                                          agg.fun = E,
                                          trans.fun = inv_logit,
                                          ids = id.list,
                                          draw.ids = draw.ids,
                                          draw.chunk = 100,
                                          # draw.chunk = 1000,
                                          agg.size = 1e4,
                                          n.threads = n.threads,
                                          gc = TRUE
                                          )
  })
}
names(effects.mar) <- marginals
effects.mar$groups <- groups

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects.mar, file.effects)

library(stringi)
source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

# region <- "amz"
# n.threads <- 1

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.effects <- paste0(path.effects, region, ".risk.tenure_cov.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

message("Aggregating observations …")
data.proc <- readRDS(file.data)

# data.proc <- data.proc[1:5e4]

cat_by_som <- 
  data.table(
    cat = c("bl",    # Baseline by forest type
            "all",   # All observations
            "for_p", # Primary forest only
            "it_c",  # All IT combined
            "it",    # IT by type
            "pa_c",  # ALL PA combined
            "pa",    # PA by type
            "ov"),   # Overlapping regions by type
    subset = c('it_type == "none" & pa_type =="none"',
               'TRUE',
               'for_type == "primary"',
               'it_type != "none"',
               'it_type != "none"',
               'pa_type != "none"',
               'pa_type != "none"',
               'it_type != "none" & pa_type != "none"'),
    group_vars = list(c("som_bmu", "som_x", "som_y",
                        "for_type", "it_type", "pa_type"), 
                      c("som_bmu", "som_x", "som_y"),
                      c("som_bmu", "som_x", "som_y",
                        "for_type"), 
                      c("som_bmu", "som_x", "som_y"), 
                      c("som_bmu", "som_x", "som_y",
                        "it_type"), 
                      c("som_bmu", "som_x", "som_y"), 
                      c("som_bmu", "som_x", "som_y",
                        "pa_type"), 
                      c("som_bmu", "som_x", "som_y",
                        "it_type", "pa_type", "overlap")))

som.units <- list()
for(i in 1:nrow(cat_by_som)) {
  som.units[[i]] <- 
    data.proc[eval(parse(text = cat_by_som$subset[[i]])),] |>
    ids_by_group(id.col = "id",
                 group.vars = cat_by_som$group_vars[[i]],
                 add.label = FALSE)
}
names(som.units) <- cat_by_som$cat
som.units <- rbindlist(som.units, fill = TRUE, idcol = "cat")
som.units[, `:=`(group.id = 1:nrow(som.units),
                 group.label = fifelse(is.na(for_type),
                                       as.character(som_bmu),
                                       paste0(som_bmu, ":", for_type)))]
som.units[, cat := factor(cat, levels = cat_by_som$cat)]
setcolorder(som.units, c("group.id", "cat", "group.label",
                         "for_type", "it_type", "pa_type", "overlap",
                         "som_bmu", "som_x", "som_y", "n", "ids"))

id.list <- som.units$ids
names(id.list) <- som.units$group.id

name.par <- "full"
ds <- open_dataset(paste0(path.arrow, "partial=", name.par), format = "arrow")
message(paste0("Evaluating effects for region `", region,
               "`, using (partial) linear predictor `", name.par,
               "` (", length(draw.ids), " draws) …"))
effects.combined <- aggregate_variables(ds,
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

# Disaggregate along tenure type combinations and rearrange output

disagg.groups <-
  ids_by_group(som.units,
               id.col = "group.id",
               group.vars = c("cat", "it_type", "pa_type", "overlap"))
disagg.groups[cat == "it_c", it_type := "combined"]
disagg.groups[cat == "pa_c", pa_type := "combined"]
disagg.groups[cat == "all", for_type := "all"]
disagg.groups[cat == "for_p", for_type := "primary"]
setorder(disagg.groups, cat, it_type, pa_type)

risk.cc <- list()
risk.cc$r <- list()
for(i in 1:nrow(disagg.groups)) {
  som.units.s <- som.units[group.id %in% disagg.groups$ids[[i]], ]
  sel <- as.character(som.units.s$group.id)
  var.names.new <- as.character(som.units.s$group.label)
  eff.sub <- effects.combined[,sel]
  colnames(eff.sub) <- var.names.new
  risk.cc$r[[i]] <- eff.sub
}

r.names <-
  disagg.groups[, paste0("for_type.", for_type, ":",
                                       "it_type.", it_type, ":",
                                       "pa_type.", pa_type)] |>
  stri_replace_all("", regex = "for_type.NA:") |>
  stri_replace_all("", regex = "it_type.NA:") |>
  stri_replace_all("", regex = ":pa_type.NA") |>
  stri_replace_all("baseline", regex = "it_type.none:pa_type.none")
names(risk.cc$r) <- r.names
risk.cc$som.units <- som.units

gc()

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(risk.cc, file.effects)

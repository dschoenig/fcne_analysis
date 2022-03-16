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
file.areas <- paste0(path.data.proc, region, ".areas.it_pa.rds")
file.effects <- paste0(path.effects, region, ".risk.tenure_areas.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

message("Aggregating observations …")
areas.it_pa <- readRDS(file.areas)
areas.it_pa[!is.na(it_type), area.label := paste0("it.", it.id)]
areas.it_pa[!is.na(pa_type), area.label := paste0("pa.", pa.id)]
setcolorder(areas.it_pa, c("area.id", "area.label"))

id.list <- areas.it_pa$ids
names(id.list) <- areas.it_pa$area.label


effects.areas <- list()
name.par <- "full"
ds <- open_dataset(paste0(path.arrow, "partial=", name.par), format = "arrow")
message(paste0("Evaluating effects for region `", region,
               "`, using (partial) linear predictor `", name.par,
               "` (", length(draw.ids), " draws) …"))
effects.areas$r <- aggregate_variables(ds,
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

effects.areas$areas.it_pa <- areas.it_pa

gc()

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects.areas, file.effects)

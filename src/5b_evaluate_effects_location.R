source("utilities.R")

# args <- commandArgs(trailingOnly = TRUE)
# region <- tolower(args[1])
# n.threads <- as.integer(args[2])

region <- "amz"
n.threads <- 4

path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.proc.rds")
file.effects <- paste0(path.effects, region, ".eff.location.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

marginals <- c("full","ten_loc")
draw.ids <- as.character(1:1000)
draw.ids <- as.character(1:100)

map_res <- switch(region,
                  amz = 5000,
                  cam = 2000)

message("Aggregating observations …")
data.proc <- readRDS(paste0(path.data.proc, region, ".data.proc.rds"))

coord_bins <- bin_cols(data.proc, c("ea_east", "ea_north"), c(map_res, map_res))
data.proc[, `:=`(ea_east.bin = coord_bins$ea_east.bin,
                 ea_north.bin = coord_bins$ea_north.bin)]
map.units <- ids_by_group(data.proc, id.col = "id",
                          group.vars = c("ea_east.bin", "ea_north.bin"),
                          expand.label = FALSE)

id.list <- map.units$ids
names(id.list) <- map.units$group.label

rm(data.proc)

effects <- list()
for(i in seq_along(marginals)) {
  ds <- open_dataset(paste0(path.arrow, "marginal=", marginals[i]), format = "arrow")
  message(paste0("Evaluating effects for region `", region,
                   "` over marginal `", marginals[i], "` (",
                   length(draw.ids), " draws) …"))
  effects[[i]] <- summarize_predictions(ds,
                                   ids = id.list,
                                   draw.ids = draw.ids,
                                   draw.chunk = 100,
                                   clamp = link_cll(c(.Machine$double.eps, 1-.Machine$double.eps)),
                                   n.threads = n.threads
                                   )
}
names(effects) <- marginals

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects, file.effects)

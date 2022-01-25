source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

# region <- "cam"
# n.threads <- 4

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.effects <- paste0(path.effects, region, ".eff.location.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

map_res <- switch(region,
                  amz = 5000,
                  cam = 1000)

marginals <- c("full","ten_loc")
draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

message("Aggregating observations …")
data.proc <- readRDS(file.data)

coord_bins <- bin_cols(data.proc, c("ea_east", "ea_north"), c(map_res, map_res))
data.proc[, `:=`(ea_east.bin = coord_bins$ea_east.bin,
                 ea_north.bin = coord_bins$ea_north.bin)]
map.units <- ids_by_group(data.proc, id.col = "id",
                          group.vars = c("ea_east.bin", "ea_north.bin"),
                          expand.label = FALSE)

id.list <- map.units$ids
names(id.list) <- map.units$group.label

rm(data.proc)

# Define aggregation function: transform to response scale, then marginalize
# over geographic area (i.e. group identity).
agg.fun <- function(x) mean(inv_logit(x))

effects.mar <- list()
for(i in seq_along(marginals)) {
  ds <- open_dataset(paste0(path.arrow, "marginal=", marginals[i]), format = "arrow")
  message(paste0("Evaluating effects for region `", region,
                   "` over marginal `", marginals[i], "` (",
                   length(draw.ids), " draws) …"))
  effects.mar[[i]] <- aggregate_variables(ds,
                                          fun = agg.fun,
                                          ids = id.list,
                                          draw.ids = draw.ids,
                                          draw.chunk = 100,
                                          # draw.chunk = 1000,
                                          agg.size = 1e6,
                                          n.threads = n.threads
                                          )
}
names(effects.mar) <- marginals
effects.mar$groups <- groups

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects.mar, file.effects)

source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
map.id <- as.integer(args[2])
n.threads <- as.integer(args[3])

# region <- "cam"
# map.id <- 1
# n.threads <- 4

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
prefix.file.effects <- paste0(region, ".risk.geo.")

map.res <- switch(region,
                  amz = 5000,
                  cam = 2500)

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

message("Aggregating observations …")

data.proc <- readRDS(file.data)
# data.proc <- data.proc[1:5e4,]
data.map <- data.proc[, .(id, it_type, pa_type, overlap, ea_east, ea_north)]
rm(data.proc)

map.anchor <- c(ea_east = floor(min(data.map$ea_east / map.res)) * map.res,
                ea_north = floor(min(data.map$ea_north / map.res)) * map.res)

maps <- 
  data.table(
    extent = c("all",
             "it_c",
             "it",
             "pa_c",
             "pa",
             "ov"),
    subset = c("TRUE",
               'it_type != "none"',
               'it_type != "none"',
               'pa_type != "none"',
               'pa_type != "none"',
               'it_type != "none" & pa_type != "none"'),
    group_vars = list(c("ea_east.bin", "ea_north.bin"), 
                      c("ea_east.bin", "ea_north.bin"), 
                      c("ea_east.bin", "ea_north.bin", "it_type"), 
                      c("ea_east.bin", "ea_north.bin"), 
                      c("ea_east.bin", "ea_north.bin", "pa_type"), 
                      c("ea_east.bin", "ea_north.bin", "it_type", "pa_type")),
    partial = rep("full", 6)
    # , partial.counterfactual = c("cov0", "it0", "it0", "pa0", "pa0", "ov0")
    )

map.units <- 
  data.map[eval(parse(text = maps$subset[[map.id]])),] |>
  bin_cols(columns = c("ea_east", "ea_north"), bin.res = rep(map.res, 2),
           bin.min = map.anchor, append = TRUE) |>
  ids_by_group(id.col = "id",
               group.vars = maps$group_vars[[map.id]],
               expand.label = FALSE)

id.list <- map.units$ids
names(id.list) <- map.units$group.label

rm(data.map)


effects.geo <- list()
name.par <- maps[map.id, partial]
ds <- open_dataset(paste0(path.arrow, "partial=", name.par),
                   format = "arrow")
message(paste0("Evaluating effects for region `", region,
               "`, map `", maps$extent[map.id],
               "`, using (partial) linear predictor `", name.par,
               "` (", length(draw.ids), " draws) …"))
effects.geo[[i]] <- aggregate_variables(ds,
                                        agg.fun = E,
                                        trans.fun = inv_cloglog,
                                        ids = id.list,
                                        draw.ids = draw.ids,
                                        draw.chunk = 100,
                                        # draw.chunk = 1000,
                                        agg.size = 5e6,
                                        n.threads = n.threads,
                                        gc = TRUE
                                        )

names(effects.geo) <- name.par
effects.geo$map.units <- map.units

file.effects <- paste0(path.effects, prefix.file.effects, maps$extent[map.id], ".rds")

gc()

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects.geo, file.effects)

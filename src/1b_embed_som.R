args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.som <- "../models/som/"
path.data <- "../data/"
path.data.int <- paste0(path.data, "intermediate/")

path.data.proc <- paste0(path.data, "processed/")
if(!dir.exists(path.data.proc)){
  dir.create(path.data, recursive = TRUE)
}


# region <- tolower(as.character(args[1]))
# region <- "cam"

## Map covariates to SOM #######################################################

datasets <- c("fit", "val")

file.som <- paste0(path.som, region, ".som.1e6.rds")
file.som <- paste0(path.som, region, ".som.1e4.rds")
som.fit <- readRDS(file.som)

for(i in seq_along(datasets)) {
  file.data.int <- paste0(path.data.int, region, ".data.", datasets[i], ".int.rds")
  file.data.proc <- paste0(path.data.proc, region, ".data.", datasets[i], ".proc.rds")

  message(paste0("Embedding observations from `", file.data.int, "` …"))

  data.int <- readRDS(file.data.int)

  embedded <-
    egp_embed(data.int[,
                       .(tri, dist_set, dist_roads,
                         dist_rivers, dens_pop, dens_roads)],
              som.fit,
              vars = c("tri", "dist_set", "dist_roads",
                       "dist_rivers", "dens_pop", "dens_roads"),
              scale = TRUE,
              bmu.name = "som_bmu",
              coord = TRUE,
              coord.names = c("som_x", "som_y"),
              list = TRUE)

  data.int[,
           `:=`(som_bmu = embedded$som_bmu,
                som_x = embedded$som_x,
                som_y = embedded$som_y)
           ]

  saveRDS(data.int, file.data.proc)
}

# Add variable to existing data set
# for(i in seq_along(datasets)) {

#   file.data.int <- paste0(path.data.int, region, ".data.", datasets[i], ".int.rds")
#   file.data.proc <- paste0(path.data.proc, region, ".data.", datasets[i], ".proc.rds")
  
#   data.int <- readRDS(file.data.int)
#   data.proc <- readRDS(file.data.proc)

#   all(data.int$id == data.proc$id)

#   merged <- merge(data.proc, data.int[, .(id, for_type)],
#                   by = "id", all = TRUE, sort = FALSE)

#   setcolorder(merged,
#               c("id", "adm0",
#                 "forestloss", "lossyear",
#                 "primary_forest", "for_type",
#                 "it", "it_type", "pa", "pa_type", "overlap",
#                 "tri", "dist_set", "dist_roads", "dist_rivers",
#                 "dens_roads", "dens_pop",
#                 "lon", "lat",
#                 "ed_east", "ed_north", "ea_east", "ea_north"))

#   saveRDS(merged, file.data.proc)
# }

variance_explained(som.fit)
quantization_error(som.fit)
topological_error(som.fit)
unit_diff(data.int, unit.var = "som_bmu", group.var = "adm0")

data.int



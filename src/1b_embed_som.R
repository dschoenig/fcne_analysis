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


region <- tolower(as.character(args[1]))
n.cores <- as.integer(args[2])

## Map covariates to SOM #######################################################

datasets <- c("fit", "val")

file.som <- paste0(path.som, region, ".som.1e6.rds")
som.fit <- readRDS(file.som)

for(i in seq_along(datasets)) {
  file.data.int <- paste0(path.data.int, region, ".data.", datasets[i], ".int.rds")
  file.data.proc <- paste0(path.data.proc, region, ".data.", datasets[i], ".proc.rds")
 

  message(paste0("Embedding observations from `", file.data.int, "` …"))

  data.int <- readRDS(file.data.int)
  
  mapped <- 
    data.int[, .(tri, dist_set, dist_roads, dist_rivers, dens_pop, dens_roads)] |>
    scale_data_som(som = som.fit) |>
    embed_som(som = som.fit,
              grid.coord = TRUE)

  data.int[,
           `:=`(som_bmu = mapped$bmu[,1],
                som_x = mapped$grid.coordinates$bmu.1[,"x"],
                som_y = mapped$grid.coordinates$bmu.1[,"y"])
           ]
  
  saveRDS(data.int, file.data.proc)
}

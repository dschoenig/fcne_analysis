library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.data <- "../data/intermediate/"
path.som <- "../models/som/"
dir.create(path.data, recursive = TRUE)
dir.create(path.som, recursive = TRUE)

## Variables
regions <- c("amz", "cam")
xdim <- 100
ydim <- 100
seed <- 19120623


## Fit SOMs and map covariates #################################################

for(i in 1:length(regions)) {

  file.data.int <- paste0(path.data, regions[i], ".data.int.rds")
  file.prefix.som <- paste0(path.som, regions[i], ".som.")
  file.som.mapped <- paste0(path.data, regions[i], ".som.mapped.rds")

  data <- readRDS(file.data.int)

  cov.z <- scale(data[, c("dist_set", "dist_roads", "dist_rivers", "slope")],
                     center = TRUE, scale = TRUE)
  rm(data)

  set.seed(seed + i)
  sam <- sample(1:nrow(cov.z), 1e6)
  train.1e4 <- cov.z[sam[1:1e4],]
  train.1e5 <- cov.z[sam[1:1e5],]
  train.1e6 <- cov.z[sam,]

  grid <- somgrid(xdim = xdim, ydim = ydim, 
                      topo = "rectangular", 
                      neighbourhood.fct = "gaussian")

  som.1e4 <- som(train.1e4, grid = grid, 
                     rlen = 1000, mode = "pbatch", 
                     init = init_som(train.1e4, xdim, ydim),
                     normalizeDataLayers = FALSE)
  saveRDS(som.1e4, paste0(file.prefix.som, "1e4.rds"))

  som.1e5 <- som(train.1e5, grid = grid, 
                     rlen = 1000, mode = "pbatch", 
                     init = som.1e4$codes,
                     normalizeDataLayers = FALSE)
  saveRDS(som.1e5, paste0(file.prefix.som, "1e5.rds"))

  som.1e6 <- som(train.1e6, grid = grid, 
                     rlen = 1000, mode = "pbatch", 
                     init = som.1e5$codes,
                     normalizeDataLayers = FALSE)
  saveRDS(som.1e6, paste0(file.prefix.som, "1e6.rds"))

  som.mapped <- map(som.1e6, cov.z)
  saveRDS(som.mapped, file.som.mapped)

  rm(cov.z,
     grid, train.1e4, train.1e5, train.1e6,
     som.1e4, som.1e5, som.1e6, som.mapped)

}

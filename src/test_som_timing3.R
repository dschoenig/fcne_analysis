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

i=2

file.data.fit.int <- paste0(path.data, regions[i], ".data.fit.int.rds")
file.prefix.som <- paste0(path.som, regions[i], ".som.")
file.som.mapped <- paste0(path.data, regions[i], ".som.mapped.rds")

data <- readRDS(file.data.fit.int)

cov.z <- scale(data[, c("tri", "dist_set", "dist_roads", "dist_rivers",
                        "dens_pop", "dens_roads")],
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
som.init <- init_som(train.1e4, xdim, ydim)



message("Large, 10 epochs, 16 cores")
a <- Sys.time()
som(train.1e6, grid = grid, 
    rlen = 2, mode = "pbatch", 
    init = som.init, cores = 16,
    normalizeDataLayers = FALSE)
b <- Sys.time()
print(b-a)

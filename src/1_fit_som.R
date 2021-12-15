args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.data <- "../data/intermediate/"
path.som <- "../models/som/"
if(!dir.exists(path.data)){
  dir.create(path.data, recursive = TRUE)
}
if(!dir.exists(path.som)){
  dir.create(path.som, recursive = TRUE)
}

region <- tolower(as.character(args[1]))
n.cores <- as.integer(args[2])

# SOM parameters
xdim <- 100
ydim <- 100
rlen <- 1000

seed <- 19120623

## Fit SOMs and map covariates #################################################

message(paste0("Fitting SOM (", region, ", ", n.cores, " cores) …"))

file.data.fit.int <- paste0(path.data, region, ".data.fit.int.rds")
file.prefix.som <- paste0(path.som, region, ".som.")

data <- readRDS(file.data.fit.int)

cov.z <- scale(data[, c("tri", "dist_set", "dist_roads", "dist_rivers",
                        "dens_pop", "dens_roads")],
               center = TRUE, scale = TRUE)
scale <- list(mean = attr(cov.z, "scaled:center"),
              sd = attr(cov.z, "scaled:scale"))
rm(data)

set.seed(seed)
sam <- sample(1:nrow(cov.z), 1e6)
train.1e4 <- cov.z[sam[1:1e4],]
train.1e5 <- cov.z[sam[1:1e5],]
train.1e6 <- cov.z[sam,]

grid <- somgrid(xdim = xdim, ydim = ydim, 
                topo = "rectangular", 
                neighbourhood.fct = "gaussian")
som.init <- init_som(train.1e4, xdim, ydim)

message("Small sample …")
a <- Sys.time()
som.1e4 <- som(train.1e4, grid = grid, 
                   rlen = rlen, mode = "pbatch", 
                   init = som.init, cores = n.cores,
                   normalizeDataLayers = FALSE)
b <- Sys.time()
print(b-a)
som.1e4$scale <- scale
saveRDS(som.1e4, paste0(file.prefix.som, "1e4.rds"))

message("Medium sample …")
a <- Sys.time()
som.1e5 <- som(train.1e5, grid = grid, 
                   rlen = rlen, mode = "pbatch", 
                   init = som.init, cores = n.cores,
                   normalizeDataLayers = FALSE)
b <- Sys.time()
print(b-a)
som.1e5$scale <- scale
saveRDS(som.1e5, paste0(file.prefix.som, "1e5.rds"))

message("Full sample …")
a <- Sys.time()
som.1e6 <- som(train.1e6, grid = grid, 
                   rlen = rlen, mode = "pbatch", 
                   init = som.init, cores = n.cores,
                   normalizeDataLayers = FALSE)
b <- Sys.time()
print(b-a)
som.1e6$scale <- scale
saveRDS(som.1e6, paste0(file.prefix.som, "1e6.rds"))

rm(cov.z, grid, train.1e4, train.1e5, train.1e6,
   som.1e4, som.1e5, som.1e6)

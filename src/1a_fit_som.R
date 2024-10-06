args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.data <- "../data/intermediate/"
path.som <- "../models/som/"
if(!dir.exists(path.som)){
  dir.create(path.som, recursive = TRUE)
}

region <- tolower(as.character(args[1]))
n.cores <- as.integer(args[2])
# region <- "amz"
# n.cores <- 4

# SOM parameters
x.dim <- 100
y.dim <- 100
epochs <- 1000
radius <- 100

seed <- 19120623

## Fit SOMs and map covariates #################################################

message(paste0("Fitting SOM (", region, ", ", n.cores, " cores) …"))

file.data.fit.int <- paste0(path.data, region, ".data.fit.int.rds")
file.prefix.som <- paste0(path.som, region, ".som.")

cov <-
  c("elevation", "slope", "sx", "cmi_min",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_pop", "dens_roads", "travel_time")

data <-
  readRDS(file.data.fit.int) |>
  _[, ..cov]


set.seed(seed)
sam <- sample(1:nrow(data), 1e6)
train.1e4 <- data[sam[1:1e4],]
train.1e5 <- data[sam[1:1e5],]
train.1e6 <- data[sam,]

# # TEST CONFIG
# x.dim <- 10
# y.dim <- 10
# epochs <- 100
# radius <- 10
# train.1e4 <- data[sam[1:10],]
# train.1e5 <- data[sam[1:10],]
# train.1e6 <- data[sam[1:10],]


message("Small sample …")
a <- Sys.time()

som.1e4 <-
  egp_som(train.1e4,
          topo = "rectangular",
          x.dim = x.dim,
          y.dim = y.dim,
          epochs = epochs,
          vars = cov,
          parallel = n.cores)

b <- Sys.time()
print(b-a)

saveRDS(som.1e4, paste0(file.prefix.som, "1e4.rds"))


message("Medium sample …")
a <- Sys.time()

som.1e5 <-
  egp_som(train.1e5,
          topo = "rectangular",
          x.dim = x.dim,
          y.dim = y.dim,
          epochs = epochs,
          vars = cov,
          parallel = n.cores)

b <- Sys.time()
print(b-a)

saveRDS(som.1e5, paste0(file.prefix.som, "1e5.rds"))


message("Full sample …")
a <- Sys.time()

som.1e6 <-
  egp_som(train.1e6,
          topo = "rectangular",
          x.dim = x.dim,
          y.dim = y.dim,
          epochs = epochs,
          vars = cov,
          parallel = n.cores)

b <- Sys.time()
print(b-a)

saveRDS(som.1e6, paste0(file.prefix.som, "1e6.rds"))

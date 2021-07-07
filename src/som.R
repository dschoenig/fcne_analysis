library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.data <- "../processed/data/"
path.som <- "../processed/som/"

xdim <- 100
ydim <- 100

## AMAZON ######################################################################

amz.data <- readRDS(paste0(path.data, "amz.data.rds"))

amz.cov.z <- scale(amz.data[, c("dist_set", "dist_roads", "dist_rivers", "slope")],
                   center = TRUE, scale = TRUE)
rm(amz.data)

set.seed(19120623)
sam <- sample(1:nrow(amz.cov.z), 1e6)
amz.train.1e4 <- amz.cov.z[sam[1:1e4],]
amz.train.1e5 <- amz.cov.z[sam[1:1e5],]
amz.train.1e6 <- amz.cov.z[sam,]

amz.grid <- somgrid(xdim = xdim, ydim = ydim, 
                    topo = "rectangular", 
                    neighbourhood.fct = "gaussian")

system.time({
amz.som.1e4 <- som(amz.train.1e4, grid = amz.grid, 
                   rlen = 1000, mode = "pbatch", 
                   init = init_som(amz.train.1e4, xdim, ydim),
                   normalizeDataLayers = FALSE)
saveRDS(amz.som.1e4, paste0(path.som, "amz.som.1e4.rds"))
})

system.time({
amz.som.1e5 <- som(amz.train.1e5, grid = amz.grid, 
                   rlen = 1000, mode = "pbatch", 
                   init = amz.som.1e4$codes,
                   normalizeDataLayers = FALSE)
saveRDS(amz.som.1e5, paste0(path.som, "amz.som.1e5.rds"))

system.time({
amz.som.1e6 <- som(amz.train.1e6, grid = amz.grid, 
                   rlen = 1000, mode = "pbatch", 
                   init = amz.som.1e5$codes,
                   normalizeDataLayers = FALSE)
saveRDS(amz.som.1e6, paste0(path.som, "amz.som.1e6"))

# amz.som.1e6.dir <- som(amz.train.1e6, grid = amz.grid, 
#                    rlen = 1000, mode = "pbatch", 
#                    init = init_som(amz.train.1e6, xdim, ydim),
#                    normalizeDataLayers = FALSE)
# saveRDS(amz.som.1e6.dir, paste0(path.som, "amz.som.1e6"))

rm(list = grep("amz", ls(), value = TRUE))

## CENTRAL AMERICA #############################################################

cam.data <- readRDS(paste0(path.data, "cam.data.rds"))

cam.cov.z <- scale(cam.data[, c("dist_set", "dist_roads", "dist_rivers", "slope")],
                   center = TRUE, scale = TRUE)
rm(cam.data)

set.seed(19120623)
sam <- sample(1:nrow(cam.cov.z), 1e6)
cam.train.1e4 <- cam.cov.z[sam[1:1e4],]
cam.train.1e5 <- cam.cov.z[sam[1:1e5],]
cam.train.1e6 <- cam.cov.z[sam,]

cam.grid <- somgrid(xdim = xdim, ydim = ydim, 
                    topo = "rectangular", 
                    neighbourhood.fct = "gaussian")

system.time({
cam.som.1e4 <- som(cam.train.1e4, grid = cam.grid, 
                   rlen = 1000, mode = "pbatch", 
                   init = init_som(cam.train.1e4, xdim, ydim),
                   normalizeDataLayers = FALSE)
saveRDS(cam.som.1e4, paste0(path.som, "cam.som.1e4.rds"))
})

system.time({
cam.som.1e5 <- som(cam.train.1e5, grid = cam.grid, 
                   rlen = 1000, mode = "pbatch", 
                   init = cam.som.1e4$codes,
                   normalizeDataLayers = FALSE)
saveRDS(cam.som.1e5, paste0(path.som, "cam.som.1e5.rds"))
})

system.time({
cam.som.1e6 <- som(cam.train.1e6, grid = cam.grid, 
                   rlen = 1000, mode = "pbatch", 
                   init = cam.som.1e5$codes,
                   normalizeDataLayers = FALSE)
saveRDS(cam.som.1e6, paste0(path.som, "cam.som.1e6"))
})

# cam.som.1e6.dir <- som(cam.train.1e6, grid = cam.grid, 
#                    rlen = 1000, mode = "pbatch", 
#                    init = init_som(cam.train.1e6, xdim, ydim),
#                    normalizeDataLayers = FALSE)
# saveRDS(cam.som.1e6.dir, paste0(path.som, "cam.som.1e6"))

rm(list = grep("cam", ls(), value = TRUE))


## QUALITY MEASURES ############################################################

# library(aweSOM)
# system.time({
#   amz.qual <- somQuality(amz.som.1e6)
# })

# system.time({
#   amz.qual <- somQuality(amz.som.1e6)
# })

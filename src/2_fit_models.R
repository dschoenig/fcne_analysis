# Version without parametric effects based on area type

library(data.table)
library(mgcv)
library(kohonen)
library(mvnfast)
source("utilities.R")

## Paths
path.data.int <- "../data/intermediate/"
path.data.proc <- "../data/processed/"
path.som <- "../models/som/"
path.gam <- "../models/gam/"
dir.create(path.gam, recursive = TRUE)


## AMAZON ######################################################################

amz.data <- readRDS(paste0(path.data.int, "amz.data.rds"))
amz.som <- readRDS(paste0(path.som, "amz.som.1e6.rds"))
amz.som.mapped <- readRDS(paste0(path.data.int, "amz.som.mapped"))

amz.som.xy <- data.table(amz.som$grid$pts[amz.som.mapped$unit.classif,])
amz.data[, `:=`(som_x = amz.som.xy$x, som_y = amz.som.xy$y)]
saveRDS(amz.data, paste0(path.data.proc, "amz.data.proc.rds"))
amz.mod <- as.data.frame(amz.data[, 
                                  .(id, forestloss,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
amz.mod$b0 <- model.matrix(~ 1, amz.mod)
rm(amz.data, amz.som, amz.som.mapped)

k = 2000
max.knots = 10000
# Penalized intercept
system.time({
amz.m2 <-
  bam(forestloss ~ -1 +
      b0 +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k/2, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k/2, xt = list(max.knots = max.knots)) +
      s(adm0, bs = 're'),
      family = binomial(link = "cloglog"),
      data = amz.mod,
      drop.intercept = FALSE,
      select = TRUE,
      paraPen = list(b0 = list(diag(1))),
      chunk.size = 5e3,
      discrete = TRUE,
      nthreads = c(2,1),
      gc.level = 0
      )
})
saveRDS(amz.m2, paste0(path.gam, "amz.m2.rds"))

rm(amz.m2, amz.mod)


## CENTRAL AMERICA #############################################################

cam.data <- readRDS(paste0(path.data.int, "cam.data.rds"))
cam.som <- readRDS(paste0(path.som, "cam.som.1e6.rds"))
cam.som.mapped <- readRDS(paste0(path.data.int, "cam.som.mapped"))
cam.som.xy <- data.table(cam.som$grid$pts[cam.som.mapped$unit.classif,])
cam.data[, `:=`(som_x = cam.som.xy$x, som_y = cam.som.xy$y)]
saveRDS(cam.data, paste0(path.data.proc, "cam.data.proc.rds"))
cam.mod <- as.data.frame(cam.data[, 
                                  .(id, forestloss,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
cam.mod$b0 <- model.matrix(~ 1, cam.mod)
rm(cam.data, cam.som, cam.som.mapped)

k = 2000
max.knots = 10000
# Penalized intercept
system.time({
cam.m2 <-
  bam(forestloss ~ -1 +
      b0 +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k/2, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k/2, xt = list(max.knots = max.knots)) +
      s(adm0, bs = 're'),
      family = binomial(link = "cloglog"),
      data = cam.mod,
      drop.intercept = FALSE,
      select = TRUE,
      paraPen = list(b0 = list(diag(1))),
      chunk.size = 5e3,
      discrete = TRUE,
      nthreads = c(2,1),
      gc.level = 0
      )
})
saveRDS(cam.m2, paste0(path.gam, "cam.m2.rds"))
rm(cam.m2, cam.mod)


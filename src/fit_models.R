library(data.table)
library(mgcv)
library(kohonen)
source("utilities.R")
options("ffbatchsize" = 1, "ffbatchbytes" = 2 * 2^30)

## Paths
path.data <- "../processed/data/"
path.som <- "../processed/som/"
path.results <- "../results/models/"
dir.create(path.results, recursive = TRUE)


## MODELS ######################################################################

models <- list(id = 0:2,
               response = rep("forestloss", 3),
               predictor = c(
                             "P",
                             paste("P",
                                   "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                                   sep = " + "),
                             # paste("P",
                             #       "s(som_x, som_y, bs = 'gp', k = 10, xt = list(max.knots = 10000))",
                             #       "s(som_x, som_y, bs = 'gp', by = adm0, k = 10, xt = list(max.knots = 10000))",
                             #       "s(adm0, bs = 're')",
                             #       "s(ed_east, ed_north, bs = 'gp', k = 20, xt = list(max.knots = 10000))",
                             #       sep = " + ")
                             paste("P",
                                   "s(som_x, som_y, bs = 'gp', k = 1000, xt = list(max.knots = 10000))",
                                   "s(som_x, som_y, bs = 'gp', by = adm0, k = 1000, xt = list(max.knots = 10000))",
                                   "s(adm0, bs = 're')",
                                   "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                                   sep = " + ")
                            ),
               paraPen = rep(list(P = list(diag(9))), 3),
               drop.intercept = rep(TRUE, 3),
               link = rep("cloglog", 3),
               select = rep(TRUE, 3)
               )


## AMAZON ######################################################################

amz.data <- readRDS(paste0(path.data, "amz.data.rds"))
amz.som <- readRDS(paste0(path.som, "amz.som.1e6.rds"))
amz.som.mapped <- readRDS(paste0(path.som, "amz.som.mapped"))

amz.som.xy <- data.table(amz.som$grid$pts[amz.som.mapped$unit.classif,])
amz.data[, `:=`(som_x = amz.som.xy$x, som_y = amz.som.xy$y)]
amz.mod <- as.data.frame(amz.data[, 
                                  .(id, forestloss, it_type, pa_type,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
amz.mod$P <- model.matrix(~ it_type * pa_type, amz.mod)
rm(amz.data, amz.som, amz.som.mapped)

amz.fitted <- fit_models(models, amz.mod, 5e3, subset = 2, path.results, "amz.m")



## CENTRAL AMERICA #############################################################

cam.data <- readRDS(paste0(path.data, "cam.data.rds"))
cam.som <- readRDS(paste0(path.som, "cam.som.1e6.rds"))
cam.som.mapped <- readRDS(paste0(path.som, "cam.som.mapped"))

cam.som.xy <- data.table(cam.som$grid$pts[cam.som.mapped$unit.classif,])
cam.data[, `:=`(som_x = cam.som.xy$x, som_y = cam.som.xy$y)]
cam.mod <- as.data.frame(cam.data[, 
                                  .(id, forestloss, it_type, pa_type,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
cam.mod$P <- model.matrix(~ it_type * pa_type, cam.mod)
rm(cam.data, cam.som, cam.som.mapped)

cam.fitted <- fit_models(models, cam.mod, 5e3, subset = 2, path.results, "cam.m")


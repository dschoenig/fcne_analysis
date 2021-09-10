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

models_cam <- 
  list(id = 0:2,
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
       paraPen = rep(list(P = list(diag(81))), 3),
       drop.intercept = rep(TRUE, 3),
       link = rep("cloglog", 3),
       select = rep(TRUE, 3)
       )

models_amz <- 
  list(id = 0:2,
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
       paraPen = rep(list(P = list(diag(90))), 3),
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

amz.data[order(it_type, adm0),.N,.(it_type, adm0)]

amz.data.red <- 
  amz.data[!(it_type == "recognized" & adm0 == "SUR") & 
           !(it_type == "not_recognized" & adm0 %in% c("BRA", "COL")) &
           !(pa_type == "indirect_use" & adm0 %in% c("ECU", "GUY")) &
           !(pa_type == "direct_use" & adm0 %in% c("BOL", "ECU", "GUY")) &
           !(pa_type == "none" & it_type == "not_recognized" & adm0 == "GUF") &
           !(pa_type == "indirect_use" & it_type == "recognized" & adm0 == "BOL") &
           !(pa_type == "direct_use" & it_type == "recognized" & adm0 == "COL") &
           !(pa_type == "direct_use" & it_type == "not_recognized" & adm0 == "SUR"),
           ]


amz.mod <- as.data.frame(amz.data.red[, 
                                  .(id, forestloss, it_type, pa_type,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])

contrasts <- list(it_type = contr.treatment(levels(amz.data$it_type)),
                  pa_type = contr.treatment(levels(amz.data$pa_type)),
                  adm0 = diag(9))
dimnames(contrasts$adm0) <- list(levels(amz.data$adm0), levels(amz.data$adm0))
amz.mod$P <- model.matrix(~ it_type + pa_type + it_type:pa_type + 
                          adm0 + it_type:adm0 + pa_type:adm0 + it_type:pa_type:adm0,
                          contrasts.arg = contrasts, data = amz.mod)

amz.mod$P <- 
  rename_para(amz.mod$P,
              patterns <- c("\\(Intercept\\)", "adm0",
                            "typenot", "typerec", "typeind", "typedir"),
              replacements <- c("intercept", "adm0-",
                                "type-not", "type-rec", "type-ind", "type-dir"))

rm(amz.data, amz.data.red, amz.som, amz.som.mapped)

amz.fitted <- fit_models(models_amz, amz.mod, 2.5e3, subset = 2, path.results, "amz.t")



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
contrasts <- list(it_type = contr.treatment(levels(cam.data$it_type)),
                  pa_type = contr.treatment(levels(cam.data$pa_type)),
                  adm0 = diag(8))
dimnames(contrasts$adm0) <- list(levels(cam.data$adm0), levels(cam.data$adm0))
cam.mod$P <- model.matrix(~ it_type + pa_type + 
                          it_type:pa_type + adm0 + it_type:adm0 + pa_type:adm0,
                          contrasts.arg = contrasts, data = cam.mod)
cam.mod$P <- 
  rename_para(cam.mod$P,
              patterns <- c("\\(Intercept\\)", "adm0",
                            "typenot", "typerec", "typeind", "typedir"),
              replacements <- c("intercept", "adm0-",
                                "type-not", "type-rec", "type-ind", "type-dir"))

rm(cam.data, cam.som, cam.som.mapped)

cam.fitted <- fit_models(models, cam.mod, 2.5e3, subset = 2, path.results, "cam.m")


##

load_models("amz.t2", summary = FALSE)
summary(amz.t2, re.test = FALSE)

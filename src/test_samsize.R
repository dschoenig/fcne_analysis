library(data.table)
library(mgcv)
source("utilities.R")
options("ffbatchsize" = 100, "ffbatchbytes" = 16 * 2^30)


res.path <- "../results/tests/samsize/"
dir.create(res.path, recursive = TRUE)

mod.sam.5e5 <- list(id = 0:2,
                    response = rep("forestloss", 3),
                    predictor = c(
                                  "P",
                                  paste("P",
                                        "s(ed_east, ed_north, bs = 'gp', k = 250)",
                                        sep = " + "),
                                  paste("P",
                                        "s(som.x, som.y, bs = 'gp', k = 250)",
                                        "s(som.x, som.y, bs = 'gp', by = adm0, k = 250)",
                                        "s(adm0, bs = 're')",
                                        "s(ed_east, ed_north, bs = 'gp', k = 250)",
                                        sep = " + ")
                                 ),
                    paraPen = rep(list(P = list(diag(9))), 3),
                    drop.intercept = rep(TRUE, 3),
                    link = rep("cloglog", 3),
                    select = rep(TRUE, 3)
                    )
mod.sam.1e6 <- list(id = 0:2,
                    response = rep("forestloss", 3),
                    predictor = c(
                                  "P",
                                  paste("P",
                                        "s(ed_east, ed_north, bs = 'gp', k = 500)",
                                        sep = " + "),
                                  paste("P",
                                        "s(som.x, som.y, bs = 'gp', k = 500)",
                                        "s(som.x, som.y, bs = 'gp', by = adm0, k = 500)",
                                        "s(adm0, bs = 're')",
                                        "s(ed_east, ed_north, bs = 'gp', k = 500)",
                                        sep = " + ")
                                 ),
                    paraPen = rep(list(P = list(diag(9))), 3),
                    drop.intercept = rep(TRUE, 3),
                    link = rep("cloglog", 3),
                    select = rep(TRUE, 3)
                    )
mod.sam.2e6 <- list(id = 0:2,
                    response = rep("forestloss", 3),
                    predictor = c(
                                  "P",
                                  paste("P",
                                        "s(ed_east, ed_north, bs = 'gp', k = 1000, xt = list(max.knots = 5000))",
                                        sep = " + "),
                                  paste("P",
                                        "s(som.x, som.y, bs = 'gp', k = 1000, xt = list(max.knots = 5000))",
                                        "s(som.x, som.y, bs = 'gp', by = adm0, k = 1000, xt = list(max.knots = 5000))",
                                        "s(adm0, bs = 're')",
                                        "s(ed_east, ed_north, bs = 'gp', k = 1000, xt = list(max.knots = 5000))",
                                        sep = " + ")
                                 ),
                    paraPen = rep(list(P = list(diag(9))), 3),
                    drop.intercept = rep(TRUE, 3),
                    link = rep("cloglog", 3),
                    select = rep(TRUE, 3)
                    )
mod.sam.5e6 <- list(id = 0:2,
                    response = rep("forestloss", 3),
                    predictor = c(
                                  "P",
                                  paste("P",
                                        "s(ed_east, ed_north, bs = 'gp', k = 1500, xt = list(max.knots = 5000))",
                                        sep = " + "),
                                  paste("P",
                                        "s(som.x, som.y, bs = 'gp', k = 1000, xt = list(max.knots = 5000))",
                                        "s(som.x, som.y, bs = 'gp', by = adm0, k = 1000, xt = list(max.knots = 5000))",
                                        "s(adm0, bs = 're')",
                                        "s(ed_east, ed_north, bs = 'gp', k = 1500, xt = list(max.knots = 5000))",
                                        sep = " + ")
                                 ),
                    paraPen = rep(list(P = list(diag(9))), 3),
                    drop.intercept = rep(TRUE, 3),
                    link = rep("cloglog", 3),
                    select = rep(TRUE, 3)
                    )
mod.sam.1e7 <- list(id = 0:2,
                    response = rep("forestloss", 3),
                    predictor = c(
                                  "P",
                                  paste("P",
                                        "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                                        sep = " + "),
                                  paste("P",
                                        "s(som.x, som.y, bs = 'gp', k = 1000, xt = list(max.knots = 5000))",
                                        "s(som.x, som.y, bs = 'gp', by = adm0, k = 1000, xt = list(max.knots = 5000))",
                                        "s(adm0, bs = 're')",
                                        "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                                        sep = " + ")
                                 ),
                    paraPen = rep(list(P = list(diag(9))), 3),
                    drop.intercept = rep(TRUE, 3),
                    link = rep("cloglog", 3),
                    select = rep(TRUE, 3)
                    )

## AMAZON

amz <- readRDS("../data/amz.rds")
amz[pa == FALSE, `:=`(parea = "none")] 
amz[pa == TRUE & pa_use_ind, `:=`(parea = "indirect")] 
amz[pa == TRUE & pa_use_dir, `:=`(parea = "direct")] 
amz[it == FALSE, `:=`(indter = "none")] 
amz[it == TRUE & it_rec, `:=`(indter = "recognized")] 
amz[it == TRUE & it_notrec, `:=`(indter = "notrecognized")] 
amz$parea <- factor(amz$parea, levels = c("none", "indirect", "direct"))
amz$indter <- factor(amz$indter, levels = c("none", "recognized", "notrecognized"))

amz.mod <- as.data.frame(na.omit(amz[, 
                                     .(id, forestloss, parea, indter,
                                     som.x, som.y, ed_east, ed_north, adm0)
                                     ]))
amz.mod$P <- model.matrix(~ parea * indter, amz.mod)
rm(amz)

set.seed(123)
# amz.mod.5e5 <- amz.mod[sample(1:nrow(amz.mod), 5e5),]
amz.mod.1e6 <- amz.mod[sample(1:nrow(amz.mod), 1e6),]
# amz.mod.2e6 <- amz.mod[sample(1:nrow(amz.mod), 2e6),]
amz.mod.5e6 <- amz.mod[sample(1:nrow(amz.mod), 5e6),]

# fm.amz.5e5 <- fit_models(mod.sam.5e5, amz.mod.5e5, 5e3, res.path, "amz.5e5.m")
fm.amz.1e6 <- fit_models(mod.sam.1e6, amz.mod.1e6, 5e3, res.path, "amz.1e6.m")
# fm.amz.2e6 <- fit_models(mod.sam.2e6, amz.mod.2e6, 5e3, res.path, "amz.2e6.m")
gc()
fm.amz.5e6 <- fit_models(mod.sam.5e6, amz.mod.5e6, 5e3, res.path, "amz.5e6.m")
# fm.amz.1e7 <- fit_models(mod.sam.1e7, amz.mod, 1e4, res.path, "amz.1e7.m")

# fm.amz.sam <- rbindlist(c(fm.amz.5e5, fm.amz.1e6, fm.amz.2e6, fm.amz.5e6, fm.amz.1e7))
# fm.amz.sam$sample_size <- rep(c(5e5, 1e6, 2e6, 5e5, 1e7), each = 3)
# saveRDS(fm.amz.sam, paste0(res.path, "fm.amz.sam.rds"))

rm(
   amz.mod, 
   # amz.mod.5e5
   amz.mod.1e6,
   # amz.mod.2e6
   amz.mod.5e6
  )
gc()

## CENTRAL AMERICA

cam <- readRDS("../data/cam.rds")
cam[pa == FALSE, `:=`(parea = "none")] 
cam[pa == TRUE & pa_use_ind, `:=`(parea = "indirect")] 
cam[pa == TRUE & pa_use_dir, `:=`(parea = "direct")] 
cam[it == FALSE, `:=`(indter = "none")] 
cam[it == TRUE & it_rec, `:=`(indter = "recognized")] 
cam[it == TRUE & it_notrec, `:=`(indter = "notrecognized")] 
cam$parea <- factor(cam$parea, levels = c("none", "indirect", "direct"))
cam$indter <- factor(cam$indter, levels = c("none", "recognized", "notrecognized"))

cam.mod <- as.data.frame(na.omit(cam[, 
                                     .(id, forestloss, parea, indter,
                                     som.x, som.y, ed_east, ed_north, adm0)
                                     ]))
cam.mod$P <- model.matrix(~ parea * indter, cam.mod)
rm(cam)

set.seed(123)
# cam.mod.5e5 <- cam.mod[sample(1:nrow(cam.mod), 5e5),]
cam.mod.1e6 <- cam.mod[sample(1:nrow(cam.mod), 1e6),]
# cam.mod.2e6 <- cam.mod[sample(1:nrow(cam.mod), 2e6),]
cam.mod.5e6 <- cam.mod[sample(1:nrow(cam.mod), 5e6),]

# fm.cam.5e5 <- fit_models(mod.sam.5e5, cam.mod.5e5, 5e3, res.path, "cam.5e5.m")
fm.cam.1e6 <- fit_models(mod.sam.1e6, cam.mod.1e6, 5e3, res.path, "cam.1e6.m")
# fm.cam.2e6 <- fit_models(mod.sam.2e6, cam.mod.2e6, 5e3, res.path, "cam.2e6.m")
fm.cam.5e6 <- fit_models(mod.sam.5e6, cam.mod.5e6, 5e3, res.path, "cam.5e6.m")
# fm.cam.1e7 <- fit_models(mod.sam.1e7, cam.mod, 1e4, res.path, "cam.1e7.m")

# fm.cam.sam <- rbindlist(c(fm.cam.5e5, fm.cam.1e6, fm.cam.2e6, fm.cam.5e6, fm.cam.1e7))
# fm.cam.sam$sample_size <- rep(c(5e5, 1e6, 2e6, 5e5, 1e7), each = 3)
# saveRDS(fm.cam.sam, paste0(res.path, "fm.cam.sam.rds"))

rm(
   cam.mod, 
   # cam.mod.5e5
   cam.mod.1e6,
   # cam.mod.2e6
   cam.mod.5e6
  )
gc()

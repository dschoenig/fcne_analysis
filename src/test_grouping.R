library(data.table)
library(mgcv)
source("utilities.R")
options("ffbatchsize" = 100, "ffbatchbytes" = 16 * 2^30)

## MODEL SETUP

## 1e5 OBSERVATIONS

res.path.1e5 <- "../results/tests/grouping/1e5/"
dir.create(res.path.1e5, recursive = TRUE)

res.path.1e6 <- "results/tests/grouping/1e6/"
dir.create(res.path.1e6, recursive = TRUE)

mod.1e5 <- list(id = 0:8,
               response = rep("forestloss", 9),
               predictor = c(
                             "P",
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 100)",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 100)",
                                   "s(group1, bs = 're')",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 100)",
                                   "s(som.x, som.y, bs = 'gp', by = adm0, k = 100)",
                                   "s(adm0, bs = 're')",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 100)",
                                   "s(som.x, som.y, bs = 'gp', by = adm0, k = 100)",
                                   "s(adm0, bs = 're')",
                                   "s(parea, pa_id, bs = 're')",
                                   "s(indter, it_id, bs = 're')",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 100)",
                                   "s(ed_east, ed_north, bs = 'gp', k = 100)",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 100)",
                                   "s(ed_east, ed_north, bs = 'gp', k = 100)",
                                   "s(parea, pa_id, bs = 're')",
                                   "s(indter, it_id, bs = 're')",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 100)",
                                   "s(som.x, som.y, bs = 'gp', by = adm0, k = 100)",
                                   "s(adm0, bs = 're')",
                                   "s(ed_east, ed_north, bs = 'gp', k = 100)",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 100)",
                                   "s(som.x, som.y, bs = 'gp', by = adm0, k = 100)",
                                   "s(adm0, bs = 're')",
                                   "s(ed_east, ed_north, bs = 'gp', k = 100)",
                                   "s(parea, pa_id, bs = 're')",
                                   "s(indter, it_id, bs = 're')",
                                   sep = " + ")
                            ),
               paraPen = rep(list(P = list(rank(9), diag(9))), 9),
               drop.intercept = rep(TRUE, 9),
               link = rep("cloglog", 9),
               select = rep(TRUE, 9)
               )

## 1e6 OBSERVATIONS

mod.1e6 <- list(id = 0:8,
               response = rep("forestloss", 9),
               predictor = c(
                             "P",
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 500)",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 500)",
                                   "s(group1, bs = 're')",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 500)",
                                   "s(som.x, som.y, bs = 'gp', by = adm0, k = 500)",
                                   "s(adm0, bs = 're')",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 500)",
                                   "s(som.x, som.y, bs = 'gp', by = adm0, k = 500)",
                                   "s(adm0, bs = 're')",
                                   "s(parea, pa_id, bs = 're')",
                                   "s(indter, it_id, bs = 're')",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 500)",
                                   "s(ed_east, ed_north, bs = 'gp', k = 500)",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 500)",
                                   "s(ed_east, ed_north, bs = 'gp', k = 500)",
                                   "s(parea, pa_id, bs = 're')",
                                   "s(indter, it_id, bs = 're')",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 500)",
                                   "s(som.x, som.y, bs = 'gp', by = adm0, k = 500)",
                                   "s(adm0, bs = 're')",
                                   "s(ed_east, ed_north, bs = 'gp', k = 500)",
                                   sep = " + "),
                             paste("P",
                                   "s(som.x, som.y, bs = 'gp', k = 500)",
                                   "s(som.x, som.y, bs = 'gp', by = adm0, k = 500)",
                                   "s(adm0, bs = 're')",
                                   "s(ed_east, ed_north, bs = 'gp', k = 500)",
                                   "s(parea, pa_id, bs = 're')",
                                   "s(indter, it_id, bs = 're')",
                                   sep = " + ")
                            ),
               paraPen = rep(list(P = list(rank(9), diag(9))), 9),
               drop.intercept = rep(TRUE, 9),
               link = rep("cloglog", 9),
               select = rep(TRUE, 9)
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
amz[pa == FALSE & it == FALSE, group0 := adm0]
amz[pa == TRUE | it == TRUE, group0 := paste(pa_id, it_id, sep = "_")]
amz$group0 <- factor(amz$group0)
amz[pa == FALSE & it == FALSE, group1 := adm1]
amz[pa == TRUE | it == TRUE, group1 := paste(pa_id, it_id, sep = "_")]
amz$group1 <- factor(amz$group1)

amz.mod <- as.data.frame(na.omit(amz[, 
                                     .(id, forestloss, parea, indter,
                                     som.x, som.y, ed_east, ed_north,
                                     pa_id, it_id, adm0, adm1, group0, group1)
                                     ]))
amz.mod$P <- model.matrix(~ parea * indter, amz.mod)
rm(amz)

set.seed(123)
amz.mod.1e5 <- amz.mod[sample(1:nrow(amz.mod), 1e5),]
fm.amz.1e5 <- fit_models(mod.1e5, amz.mod.1e5, 5e4, res.path.1e5, "amz.m")
saveRDS(rbindlist(fm.amz.1e5), paste0(res.path.1e5, "fm.amz.1e5.rds"))

set.seed(123)
amz.mod.1e6 <- amz.mod[sample(1:nrow(amz.mod), 1e6),]
fm.amz.1e6 <- fit_models(mod.1e6, amz.mod.1e6, 5e4, res.path.1e6, "amz.m")
saveRDS(rbindlist(fm.amz.1e6), "fm.amz.1e6.rds")

rm(amz.mod)


## CENTRAL AMERICA

cam <- readRDS("../data/cam.rds")
cam[pa == FALSE, `:=`(parea = "none")] 
cam[pa == TRUE & pa_use_ind, `:=`(parea = "indirect")] 
cam[pa == TRUE & pa_use_dir, `:=`(parea = "direct")] 
cam[it == FALSE, `:=`(indter = "none")] 
cam[it == TRUE & it_rec, `:=`(indter = "recognized")] 
cam[it == TRUE & it_notrec, `:=`(indter = "notrecognized")] 
cam$parea <- factor(cam$parea, levels = c("none", "indirect", "direct"))
cam$indter <- factor(cam$indter, levels = c("none", "recognized", "notrecognized"))
cam[pa == FALSE & it == FALSE, group0 := adm0]
cam[pa == TRUE | it == TRUE, group0 := paste(pa_id, it_id, sep = "_")]
cam$group0 <- factor(cam$group0)
cam[pa == FALSE & it == FALSE, group1 := adm1]
cam[pa == TRUE | it == TRUE, group1 := paste(pa_id, it_id, sep = "_")]
cam$group1 <- factor(cam$group1)

cam.mod <- as.data.frame(na.omit(cam[, 
                                     .(id, forestloss, parea, indter,
                                     som.x, som.y, ed_east, ed_north,
                                     pa_id, it_id, adm0, adm1, group0, group1)
                                     ]))
cam.mod$P <- model.matrix(~ parea * indter, cam.mod)
rm(cam)

set.seed(123)
cam.mod.1e5 <- cam.mod[sample(1:nrow(cam.mod), 1e5),]
fm.cam.1e5 <- fit_models(mod.1e5, cam.mod.1e5, 5e4, res.path.1e5, "cam.m")
saveRDS(fm.cam.1e5, paste0(res.path.1e5, "fm.cam.1e5.rds"))

set.seed(123)
cam.mod.1e6 <- cam.mod[sample(1:nrow(cam.mod), 1e6),]
fm.cam.1e6 <- fit_models(mod.1e6, cam.mod.1e6, 5e4, res.path.1e6, "cam.m")
saveRDS(rbindlist(fm.cam.1e6), "fm.cam.1e6.rds")

rm(cam.mod)


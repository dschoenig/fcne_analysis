library(mgcv)
source("utilities.R")


# Theoretical test

data <- gamSim(6, n = 1000)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) + s(fac, bs = "re"), data = data)
summary(mod)

post <- rmvn(n = 1000, mu = coef(mod), V = vcov(mod, unconditional = TRUE))
pred <- evaluate_posterior(mod, post)
pred_sum <- summarize_predictions_by(pred, data, fun = mean, group.vars = "fac")

summary(pred_sum - mean(with(data, f0+f1+f2+f3)))
# Means should be around 3, 6, 9, 12


# Test with Central America model

library(data.table)
library(mgcv)
library(kohonen)
library(mvnfast)
source("utilities.R")

path.data.int <- "../data/intermediate/"
path.data.proc <- "../data/processed/"
path.som <- "../models/som/"
path.gam <- "../models/gam/"
dir.create(path.gam, recursive = TRUE)


cam.data <- readRDS(paste0(path.data.int, "cam.data.rds"))
cam.som <- readRDS(paste0(path.som, "cam.som.1e6.rds"))
cam.som.mapped <- readRDS(paste0(path.data.int, "cam.som.mapped"))

cam.som.xy <- data.table(cam.som$grid$pts[cam.som.mapped$unit.classif,])
cam.data[, `:=`(som_x = cam.som.xy$x, som_y = cam.som.xy$y)]
# saveRDS(cam.data, paste0(path.data.proc, "cam.data.proc.rds"))

cam.data <- cam.data[sample(1:nrow(cam.data), 1e5),]

cam.mod <- as.data.frame(cam.data[, 
                                  .(id, forestloss, it_type, pa_type,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
cam.mod$b0 <- model.matrix(~ 1, cam.mod)
cam.mod$P <- model.matrix(~ it_type * pa_type, cam.mod)
cam.mod$P2 <- model.matrix(~ it_type * pa_type * adm0, cam.mod)
# rm(cam.data, cam.som, cam.som.mapped)

k = 500
max.knots = 2000

cam.m0a <-
  bam(forestloss ~ -1 +
      b0 +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)),
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

cam.m0b <-
  bam(forestloss ~ -1 +
      P +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)),
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

cam.m1 <-
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

cam.m2 <-
  bam(forestloss ~ -1 +
      P +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k/2, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k/2, xt = list(max.knots = max.knots)) +
      s(adm0, bs = 're'),
      family = binomial(link = "cloglog"),
      data = cam.mod,
      drop.intercept = FALSE,
      select = TRUE,
      paraPen = list(P = list(diag(9))),
      chunk.size = 5e3,
      discrete = TRUE,
      nthreads = c(2,1),
      gc.level = 0
      )

cam.m3 <-
  bam(forestloss ~ -1 +
      P2 +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k/2, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k/2, xt = list(max.knots = max.knots)),
      family = binomial(link = "cloglog"),
      data = cam.mod,
      drop.intercept = FALSE,
      select = TRUE,
      paraPen = list(P2 = list(diag(72))),
      chunk.size = 5e3,
      discrete = TRUE,
      nthreads = c(2,1),
      gc.level = 0
      )


models <-
  list(cam.m0a = cam.m0a,
       cam.m0b = cam.m0b,
       cam.m1 = cam.m1,
       cam.m2 = cam.m2,
       cam.m3 = cam.m3)

cam.effect_summaries <- list()
cam.effect_summaries2 <- list()
cam.effect_summaries.mar <- list()
cam.effect_summaries2.mar <- list()
cam.mar_dif <- list()
for(i in 1:length(models)) {
  mod.post <- mgcv::rmvn(n = 1000, mu = coef(models[[i]]),
                         V = vcov(models[[i]], unconditional = TRUE))
  pred <- evaluate_posterior(models[[i]], mod.post, cam.mod, id.col = "id")
  pred_sum <- summarize_predictions_by(pred, cam.data,
                                       id.col = "id", group.vars = c("pa_type", "it_type"))
  cam.effect_summaries[[i]] <- summary(pred_sum)
  cam.effect_summaries2[[i]] <- summary(pred_sum - extract_variable(pred_sum, "none.none"))
  mod.post.mar <- mod.post
  mod.post.mar[, grep("som", names(coef(models[[i]])))] <- 0
  pred.mar <- evaluate_posterior(models[[i]], mod.post.mar, cam.mod, id.col = "id")
  pred_sum.mar <- summarize_predictions_by(pred.mar, cam.data,
                                           id.col = "id", group.vars = c("pa_type", "it_type"))
  cam.effect_summaries.mar[[i]] <- summary(pred_sum.mar)
  cam.effect_summaries2.mar[[i]] <- summary(pred_sum.mar - extract_variable(pred_sum.mar, "none.none"))
  cam.mar_dif[[i]] <- summary(pred_sum.mar - pred_sum)
}

names(cam.effect_summaries) <- names(models)
names(cam.effect_summaries2) <- names(models)
names(cam.effect_summaries.mar) <- names(models)
names(cam.effect_summaries2.mar) <- names(models)
names(cam.mar_dif) <- names(models)

rbindlist(cam.effect_summaries2.mar, idcol = "id")
rbindlist(cam.mar_dif, idcol = "id")

AIC(cam.m0a, cam.m0b, cam.m1, cam.m2, cam.m3)

summary(cam.m3)

# k.check(cam.m0)
# k.check(cam.m1)
# k.check(cam.m2)

# coef(cam.m2)[1:9]

# summary(cam.m0a)
# summary(cam.m0b)
# summary(cam.m1)
# summary(cam.m2)

# Panama only

cam.pan.effect_summaries <- list()
cam.pan.effect_summaries2 <- list()
cam.pan.effect_summaries.mar <- list()
cam.pan.effect_summaries2.mar <- list()
cam.pan.mar_dif <- list()
for(i in 1:length(models)) {
  mod.post <- mgcv::rmvn(n = 1000, mu = coef(models[[i]]),
                         V = vcov(models[[i]], unconditional = TRUE))
  pred <- evaluate_posterior(models[[i]], mod.post, cam.mod, id.col = "id")
  pred_sum <- summarize_predictions_by(pred, cam.data[adm0 == "PAN",],
                                       id.col = "id", group.vars = c("pa_type", "it_type"))
  cam.pan.effect_summaries[[i]] <- summary(pred_sum)
  cam.pan.effect_summaries2[[i]] <- summary(pred_sum - extract_variable(pred_sum, "none.none"))
  mod.post.mar <- mod.post
  mod.post.mar[, grep("som", names(coef(models[[i]])))] <- 0
  pred.mar <- evaluate_posterior(models[[i]], mod.post.mar, cam.mod, id.col = "id")
  pred_sum.mar <- summarize_predictions_by(pred.mar, cam.data,
                                           id.col = "id", group.vars = c("pa_type", "it_type"))
  cam.pan.effect_summaries.mar[[i]] <- summary(pred_sum.mar)
  cam.pan.effect_summaries2.mar[[i]] <- summary(pred_sum.mar - extract_variable(pred_sum.mar, "none.none"))
  cam.pan.mar_dif[[i]] <- summary(pred_sum.mar - pred_sum)
}

names(cam.pan.effect_summaries) <- names(models)
names(cam.pan.effect_summaries2) <- names(models)
names(cam.pan.effect_summaries.mar) <- names(models)
names(cam.pan.effect_summaries2.mar) <- names(models)
names(cam.pan.mar_dif) <- names(models)

summarize_effects <- function(models, evaluate_data, summarize_data) {
  effects <- list()
  effects.bl <- list()
  effects.mar <- list()
  effects.bl.mar <- list()
  effects.mar.dif <- list()
  for(i in 1:length(models)) {
    mod.post <- mgcv::rmvn(n = 1000, mu = coef(models[[i]]),
                           V = vcov(models[[i]], unconditional = TRUE))
    pred <- evaluate_posterior(models[[i]], mod.post, evaluate_data, id.col = "id")
    pred_sum <- summarize_predictions_by(pred, summarize_data,
                                         id.col = "id", group.vars = c("pa_type", "it_type"))
    effects[[i]] <- summary(pred_sum)
    effects.bl[[i]] <- summary(pred_sum - extract_variable(pred_sum, "none.none"))
    mod.post.mar <- mod.post
    mod.post.mar[, grep("som", names(coef(models[[i]])))] <- 0
    pred.mar <- evaluate_posterior(models[[i]], mod.post.mar, evaluate_data, id.col = "id")
    pred_sum.mar <- summarize_predictions_by(pred.mar, summarize_data,
                                             id.col = "id", group.vars = c("pa_type", "it_type"))
    effects.mar[[i]] <- summary(pred_sum.mar)
    effects.bl.mar[[i]] <- summary(pred_sum.mar - extract_variable(pred_sum.mar, "none.none"))
    effects.mar.dif[[i]] <- summary(pred_sum.mar - pred_sum)
  }
  names(effects) <- names(models)
  names(effects.bl) <- names(models)
  names(effects.mar) <- names(models)
  names(effects.bl.mar) <- names(models)
  names(effects.mar.dif) <- names(models)
  effects_overview <- list()
  effects_overview[["effects"]] <- rbindlist(effects)
  effects_overview[["effects.bl"]] <- rbindlist(effects.bl)
  effects_overview[["effects.mar"]] <- rbindlist(effects.mar)
  effects_overview[["effects.bl.mar"]] <- rbindlist(effects.bl.mar)
  effects_overview[["effects.mar.dif"]] <- rbindlist(effects.mar.dif)
  return(effects_overview)
}

cam.pan.data <- cam.data[adm0 == "PAN",]
cam.pan.mod <- as.data.frame(cam.pan.data[, 
                                          .(id, forestloss, it_type, pa_type,
                                          som_x, som_y, ed_east, ed_north, adm0)
                                          ])
cam.pan.mod$b0 <- model.matrix(~ 1, cam.pan.mod)
cam.pan.mod$P <- model.matrix(~ it_type * pa_type, cam.pan.mod)
cam.pan.mod$P2 <- model.matrix(~ it_type * pa_type * adm0, cam.pan.mod)

effects.panama <- summarize_effects(models, cam.pan.mod, cam.pan.data)

effects.panama$effects.bl.mar
effects.panama$effects.mar.dif


# Test with Amazon model

library(data.table)
library(mgcv)
library(kohonen)
library(mvnfast)
source("utilities.R")

path.data.int <- "../data/intermediate/"
path.data.proc <- "../data/processed/"
path.som <- "../models/som/"
path.gam <- "../models/gam/"
dir.create(path.gam, recursive = TRUE)


amz.data <- readRDS(paste0(path.data.int, "amz.data.rds"))
amz.som <- readRDS(paste0(path.som, "amz.som.1e6.rds"))
amz.som.mapped <- readRDS(paste0(path.data.int, "amz.som.mapped"))

amz.som.xy <- data.table(amz.som$grid$pts[amz.som.mapped$unit.classif,])
amz.data[, `:=`(som_x = amz.som.xy$x, som_y = amz.som.xy$y)]
# saveRDS(amz.data, paste0(path.data.proc, "amz.data.proc.rds"))

amz.data <- amz.data[sample(1:nrow(amz.data), 1e5),]

amz.mod <- as.data.frame(amz.data[, 
                                  .(id, forestloss, it_type, pa_type,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
amz.mod$b0 <- model.matrix(~ 1, amz.mod)
amz.mod$P <- model.matrix(~ it_type * pa_type, amz.mod)
amz.mod$P2 <- model.matrix(~ it_type * pa_type * adm0, amz.mod)
# rm(amz.data, amz.som, amz.som.mapped)

k = 500
max.knots = 2000

amz.m0a <-
  bam(forestloss ~ -1 +
      b0 +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)),
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

amz.m0b <-
  bam(forestloss ~ -1 +
      P +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)),
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

amz.m1 <-
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

amz.m2 <-
  bam(forestloss ~ -1 +
      P +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k/2, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k/2, xt = list(max.knots = max.knots)) +
      s(adm0, bs = 're'),
      family = binomial(link = "cloglog"),
      data = amz.mod,
      drop.intercept = FALSE,
      select = TRUE,
      paraPen = list(P = list(diag(9))),
      chunk.size = 5e3,
      discrete = TRUE,
      nthreads = c(2,1),
      gc.level = 0
      )

amz.m3 <-
  bam(forestloss ~ -1 +
      P2 +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k/2, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k/2, xt = list(max.knots = max.knots)),
      family = binomial(link = "cloglog"),
      data = amz.mod,
      drop.intercept = FALSE,
      select = TRUE,
      paraPen = list(P2 = list(diag(81))),
      chunk.size = 5e3,
      discrete = TRUE,
      nthreads = c(2,1),
      gc.level = 0
      )


models <-
  list(amz.m0a = amz.m0a,
       amz.m0b = amz.m0b,
       amz.m1 = amz.m1,
       amz.m2 = amz.m2,
       amz.m3 = amz.m3)

amz.effect_summaries <- list()
amz.effect_summaries2 <- list()
amz.effect_summaries.mar <- list()
amz.effect_summaries2.mar <- list()
amz.mar_dif <- list()
for(i in 1:length(models)) {
  mod.post <- mgcv::rmvn(n = 1000, mu = coef(models[[i]]),
                         V = vcov(models[[i]], unconditional = TRUE))
  pred <- evaluate_posterior(models[[i]], mod.post, amz.mod, id.col = "id")
  pred_sum <- summarize_predictions_by(pred, amz.data,
                                       id.col = "id", group.vars = c("pa_type", "it_type"))
  amz.effect_summaries[[i]] <- summary(pred_sum)
  amz.effect_summaries2[[i]] <- summary(pred_sum - extract_variable(pred_sum, "none.none"))
  mod.post.mar <- mod.post
  mod.post.mar[, grep("som", names(coef(models[[i]])))] <- 0
  pred.mar <- evaluate_posterior(models[[i]], mod.post.mar, amz.mod, id.col = "id")
  pred_sum.mar <- summarize_predictions_by(pred.mar, amz.data,
                                           id.col = "id", group.vars = c("pa_type", "it_type"))
  amz.effect_summaries.mar[[i]] <- summary(pred_sum.mar)
  amz.effect_summaries2.mar[[i]] <- summary(pred_sum.mar - extract_variable(pred_sum.mar, "none.none"))
  amz.mar_dif[[i]] <- summary(pred_sum.mar - pred_sum)
}


names(amz.effect_summaries) <- names(models)
names(amz.effect_summaries2) <- names(models)
names(amz.effect_summaries.mar) <- names(models)
names(amz.effect_summaries2.mar) <- names(models)
names(amz.mar_dif) <- names(models)

rbindlist(amz.effect_summaries2.mar, idcol = "id")
rbindlist(amz.mar_dif, idcol = "id")

AIC(amz.m0a, amz.m0b, amz.m1, amz.m2, amz.m3)

# k.check(amz.m0)
# k.check(amz.m1)
# k.check(amz.m2)

# coef(amz.m2)[1:9]

# summary(amz.m0a)
# summary(amz.m0b)
# summary(amz.m1)
# summary(amz.m2)


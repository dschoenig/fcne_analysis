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
# cam.mod$P <- model.matrix(~ it_type * pa_type, cam.mod)
# cam.mod$P2 <- model.matrix(~ it_type * pa_type * adm0, cam.mod)
# rm(cam.data, cam.som, cam.som.mapped)

k = 100
max.knots = 2000

cam.m1 <-
  bam(forestloss ~ 
      -1 + b0 +
      s(ed_east, ed_north, bs = 'gp', k = 2*k, xt = list(max.knots = max.knots)),
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

cam.m2 <-
  bam(forestloss ~
      -1 + b0 +
      s(ed_east, ed_north, bs = 'gp', k = 2*k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k, xt = list(max.knots = max.knots)) +
      s(adm0, bs = "re"),
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

cam.data[pa_type != "none" & it_type != "none", overlap := paste(it_type, pa_type, sep = ":")]
cam.data[is.na(overlap), overlap := "none"]
cam.data[, overlap := factor(overlap,
                             levels = c("none",
                                        "recognized:indirect_use",
                                        "recognized:direct_use",
                                        "not_recognized:indirect_use",
                                        "not_recognized:direct_use"),
                             ordered = TRUE)]
cam.data[,
         `:=`(it_type = as.ordered(it_type),
              pa_type = as.ordered(pa_type))]
cam.mod2 <- as.data.frame(cam.data)
cam.mod2$b0 <- model.matrix(~ 1, cam.mod2)

cam.m3 <-
  bam(forestloss ~
      -1 + b0 +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(ed_east, ed_north, bs = 'gp', by = it_type, k = k, xt = list(max.knots = max.knots)) +
      s(ed_east, ed_north, bs = 'gp', by = pa_type, k = k, xt = list(max.knots = max.knots)) +
      s(ed_east, ed_north, bs = 'gp', by = overlap, k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k, xt = list(max.knots = max.knots)) +
      s(it_type, pa_type, adm0, bs = "re"),
      family = binomial(link = "cloglog"),
      data = cam.mod2,
      drop.intercept = FALSE,
      select = TRUE,
      paraPen = list(b0 = list(diag(1))),
      chunk.size = 5e3,
      discrete = TRUE,
      nthreads = c(2,1),
      gc.level = 0
      )

models <- list(cam.m1 = cam.m1,
               cam.m2 = cam.m2,
               cam.m3 = cam.m3)



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
    base <- summarize_predictions(pred, ids = summarize_data[it_type == "none" &
                                                             pa_type == "none",
                                                             id],
                                  draw.chunk = 250,
                                  n.cores = 4)
    pred_sum_it <- summarize_predictions_by(pred, summarize_data[it_type != "none",],
                                            id.col = "id",
                                            group.vars = c("it_type"))
    pred_sum_pa <- summarize_predictions_by(pred, summarize_data[pa_type != "none",],
                                            id.col = "id",
                                            group.vars = c("pa_type"))
    pred_sum_ov <- summarize_predictions_by(pred, summarize_data[overlap != "none",],
                                            id.col = "id",
                                            group.vars = c("overlap"))
    pred_sum_c <- bind_draws(as_draws_matrix(base), pred_sum_it, pred_sum_pa)
    colnames(pred_sum_c)[1] <- "control"
    effects[[i]] <- summary(pred_sum_c)
    effects.bl[[i]] <- summary(pred_sum_c - extract_variable(pred_sum_c, "control"))
    mod.post.mar <- mod.post
    mod.post.mar[, grep("som", names(coef(models[[i]])))] <- 0
    pred.mar <- evaluate_posterior(models[[i]], mod.post.mar, evaluate_data, id.col = "id")
    base.mar <- summarize_predictions(pred.mar, ids = summarize_data[it_type == "none" &
                                                             pa_type == "none",
                                                             id],
                                  draw.chunk = 250,
                                  n.cores = 4)
    pred_sum_it.mar <- summarize_predictions_by(pred.mar, summarize_data[it_type != "none",],
                                            id.col = "id",
                                            group.vars = c("it_type"))
    pred_sum_pa.mar <- summarize_predictions_by(pred.mar, summarize_data[pa_type != "none",],
                                            id.col = "id",
                                            group.vars = c("pa_type"))
    pred_sum_ov.mar <- summarize_predictions_by(pred.mar, summarize_data[overlap != "none",],
                                            id.col = "id",
                                            group.vars = c("overlap"))
    pred_sum_c.mar <- bind_draws(as_draws_matrix(base.mar), pred_sum_it.mar, pred_sum_pa.mar)
    colnames(pred_sum_c.mar)[1] <- "control"
    effects.mar[[i]] <- summary(pred_sum_c.mar)
    effects.bl.mar[[i]] <- summary(pred_sum_c.mar - extract_variable(pred_sum_c.mar, "control"))
    effects.mar.dif[[i]] <- summary(pred_sum_c.mar - pred_sum_c)
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

AIC(cam.m1, cam.m2, cam.m3)

evaluate_data <- cam.mod2
summarize_data <- cam.data

cam.eval <- as.data.frame(cam.data)
cam.eval$b0 <- model.matrix(~ 1, cam.eval)
# cam.eval$P2 <- model.matrix(~ it_type * pa_type * adm0, cam.eval)

effects <- summarize_effects(models, cam.eval, cam.data)
effects_pan <- summarize_effects(models, cam.eval[cam.eval$adm0 == "PAN",], cam.data[adm0 == "PAN",])
effects_gtm <- summarize_effects(models, cam.eval[cam.eval$adm0 == "GTM",], cam.data[adm0 == "GTM",])

effects
effects_pan
effects_gtm

summary(cam.m3, re.test = FALSE)
summary(cam.m4, re.test = FALSE)



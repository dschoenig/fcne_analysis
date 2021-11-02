# Test with Central America model

library(data.table)
library(mgcv)
library(mvnfast)
source("utilities.R")

path.data.proc <- "../data/processed/"
path.gam <- "../models/gam/"
n.threads <- c(2,1)

cam.data <- readRDS(paste0(path.data.proc, "cam.data.proc.rds"))

cam.data <- cam.data[sample(1:nrow(cam.data), 1e5),]

cam.mod <- as.data.frame(cam.data[, 
                                  .(id, forestloss, it_type, pa_type, overlap,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
cam.mod$b0 <- model.matrix(~ 1, cam.mod)

region <- "cam"
model.names <- c("gp", "gpf", "gplf", "gpn", "gpln")
models <- list()

for(i in 1:length(model.names)) {
  mod <- paste0(region, ".m3_", model.names[i])
  model.file <- paste0(path.gam, mod, ".rds")
  models[[i]] <- readRDS(model.file)
}
names(models) <- model.names

models

summary(models[["gp"]], re.test = FALSE)
summary(models[["gpf"]], re.test = FALSE)
summary(models[["gpn"]], re.test = FALSE)

for(i in 1:length(models)) print(summary(models[[i]], re.test = FALSE))

summarize_effects <- function(models, evaluate_data, summarize_data) {
  effects <- list()
  effects.bl <- list()
  effects.mar <- list()
  effects.bl.mar <- list()
  effects.mar.dif <- list()
  predictions <- list()
  for(i in 1:length(models)) {
    mod.post <- mgcv::rmvn(n = 1000, mu = coef(models[[i]]),
                           V = vcov(models[[i]], unconditional = TRUE))
    marginals <- list()
    marginals$full <- 1:length(coef(models[[i]]))
    marginals$ten_loc <- marginals$full[!marginals$full %in% grep("som", names(coef(models[[i]])))]
    pred <- evaluate_posterior(models[[i]], mod.post, evaluate_data,
                               id.col = "id", marginals = marginals,
                               predict.chunk = 10000, post.chunk = 250)
    predictions[[i]] <- pred
    base <- summarize_predictions(pred$full, ids = summarize_data[it_type == "none" &
                                                             pa_type == "none",
                                                             id],
                                  draw.chunk = 125,
                                  n.cores = 4)
    pred_sum_it <- summarize_predictions_by(pred$full, summarize_data[it_type != "none",],
                                            id.col = "id",
                                            group.vars = c("it_type"),
                                            draw.chunk = 125,
                                            n.cores = 4)
    pred_sum_pa <- summarize_predictions_by(pred$full, summarize_data[pa_type != "none",],
                                            id.col = "id",
                                            group.vars = c("pa_type"),
                                            draw.chunk = 125,
                                            n.cores = 4)
    pred_sum_ov <- summarize_predictions_by(pred$full, summarize_data[overlap != "none",],
                                            id.col = "id",
                                            group.vars = c("overlap"),
                                            draw.chunk = 125,
                                            n.cores = 4)
    pred_sum_c <- bind_draws(as_draws_matrix(base), pred_sum_it, pred_sum_pa)
    colnames(pred_sum_c)[1] <- "control"
    effects[[i]] <- summary(pred_sum_c)
    effects.bl[[i]] <- summary(pred_sum_c - extract_variable(pred_sum_c, "control"))
    base.mar <- summarize_predictions(pred$ten_loc, ids = summarize_data[it_type == "none" &
                                                             pa_type == "none",
                                                             id],
                                  draw.chunk = 250,
                                  n.cores = 4)
    pred_sum_it.mar <- summarize_predictions_by(pred$ten_loc, summarize_data[it_type != "none",],
                                            id.col = "id",
                                            group.vars = c("it_type"),
                                            draw.chunk = 125,
                                            n.cores = 4)
    pred_sum_pa.mar <- summarize_predictions_by(pred$ten_loc, summarize_data[pa_type != "none",],
                                            id.col = "id",
                                            group.vars = c("pa_type"),
                                            draw.chunk = 125,
                                            n.cores = 4)
    pred_sum_ov.mar <- summarize_predictions_by(pred$ten_loc, summarize_data[overlap != "none",],
                                            id.col = "id",
                                            group.vars = c("overlap"),
                                            draw.chunk = 125,
                                            n.cores = 4)
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
  names(predictions) <- names(models)
  effects_overview <- list()
  effects_overview[["effects"]] <- rbindlist(effects)
  effects_overview[["effects.bl"]] <- rbindlist(effects.bl)
  effects_overview[["effects.mar"]] <- rbindlist(effects.mar)
  effects_overview[["effects.bl.mar"]] <- rbindlist(effects.bl.mar)
  effects_overview[["effects.mar.dif"]] <- rbindlist(effects.mar.dif)
  return(list(effects_overview, predictions))
}

mod.aic <- lapply(models, AIC)

effects <- summarize_effects(models, cam.mod[1:1e4,], cam.data[1:1e4,])

effects_pan <- summarize_effects(models, cam.mod[cam.mod$adm0 == "PAN",], cam.data[adm0 == "PAN",])
effects_gtm <- summarize_effects(models, cam.mod[cam.mod$adm0 == "GTM",], cam.data[adm0 == "GTM",])

effects[[1]]
effects[[2]]

effects_pan
effects_gtm

summary(cam.m3, re.test = FALSE)
summary(cam.m4, re.test = FALSE)



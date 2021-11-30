# Test with Central America model

library(data.table)
library(mgcv)
library(mvnfast)
source("utilities.R")

path.data.proc <- "../data/processed/"
path.gam <- "../models/gam/"
n.threads <- c(2,1)

cam.data <- readRDS(paste0(path.data.proc, "cam.data.proc.rds"))
it.adm.rec <- c("NIC", "PAN", "MEX", "CRI")
it.adm.nor <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")
pa.adm.ind <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")
pa.adm.dir <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")
itpa.adm.rec.ind <- c("NIC", "PAN", "MEX", "CRI")
itpa.adm.rec.dir <- c("PAN", "MEX", "CRI")
itpa.adm.nor.ind <- c("BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV")
itpa.adm.nor.dir <- c("SLV", "BLZ", "PAN", "GTM", "HND", "CRI")
cam.data <-
  cam.data[(it_type == "none" & pa_type == "none") |
           (it_type == "recognized" & pa_type == "none" & adm0 %in% it.adm.rec) |
           (it_type == "not_recognized" & pa_type == "none" & adm0 %in% it.adm.nor) |
           (pa_type == "indirect_use" & it_type == "none" & adm0 %in% pa.adm.ind) |
           (pa_type == "direct_use" & it_type == "none" & adm0 %in% pa.adm.dir) |
           (it_type == "recognized" & pa_type == "indirect_use" & adm0 %in% itpa.adm.rec.ind) |
           (it_type == "recognized" & pa_type == "direct_use" & adm0 %in% itpa.adm.rec.dir) |
           (it_type == "not_recognized" & pa_type == "indirect_use" & adm0 %in% itpa.adm.nor.ind) |
           (it_type == "not_recognized" & pa_type == "direct_use" & adm0 %in% itpa.adm.nor.dir)]
set.seed(1234)
sam <- sample(1:nrow(cam.data), 1e5)
cam.data <- cam.data[sam]

cam.mod <- as.data.frame(cam.data[, 
                                  .(id, forestloss, it_type, pa_type, overlap,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
cam.mod$b0 <- model.matrix(~ 1, cam.mod)

region <- "cam"
model.names <- c("l0", "l1", "l2")
models <- list()

for(i in 1:length(model.names)) {
  mod <- paste0(region, ".m3_", model.names[i])
  model.file <- paste0(path.gam, mod, ".rds")
  models[[i]] <- readRDS(model.file)
}
names(models) <- model.names


mod.aic <- lapply(models, AIC)
mod.aic
for(i in 1:length(models)) print(summary(models[[i]], re.test = FALSE))

summarize_effects <- function(models, evaluate_data, summarize_data) {
  effects <- list()
  effects.bl <- list()
  effects.mar <- list()
  effects.bl.mar <- list()
  effects.mar.dif <- list()
  predictions <- list()
  for(i in 1:length(models)) {
    message(names(models)[i])
    linkinv <- models[[i]]$family$linkinv
    mod.post <- mgcv::rmvn(n = 1000, mu = coef(models[[i]]),
                           V = vcov(models[[i]], unconditional = TRUE))
    marginals <- list()
    marginals$full <- 1:length(coef(models[[i]]))
    marginals$ten_loc <- marginals$full[!marginals$full %in% grep("som", names(coef(models[[i]])))]
    pred <- evaluate_posterior(models[[i]], mod.post, evaluate_data,
                               id.col = "id", marginals = marginals,
                               predict.chunk = 10000, post.chunk = 250)
    predictions[[i]] <- pred
    groups.bl <-
      summarize_data[it_type == "none" & pa_type == "none"] |>
      ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type"))
    groups.it <-
      summarize_data[it_type != "none"] |>
      ids_by_group(id.col = "id", group.vars = c("it_type"))
    groups.pa <-
      summarize_data[pa_type != "none"] |>
      ids_by_group(id.col = "id", group.vars = c("pa_type"))
    groups.it_pa <-
      summarize_data[it_type != "none" & pa_type != "none"] |>
      ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type"))
    groups <- rbindlist(list(groups.bl,
                             groups.it,
                             groups.pa,
                             groups.it_pa), fill = TRUE)
    groups$group.id <- 1:nrow(groups)
    setcolorder(groups, c("group.id", "group.label","it_type", "pa_type"))
    id.list <- groups$ids
    names(id.list) <- groups$group.label
    pred_sum_c <- summarize_predictions(pred$full,
                                      ids = id.list,
                                      id.col = "id",
                                      draw.chunk = 125,
                                      n.cores = 4)
    colnames(pred_sum_c)[1] <- "control"
    effects[[i]] <- summary(linkinv(pred_sum_c),
                             mean, median, mode=ggdist::Mode, sd, mad, quantile2)
    effects.bl[[i]] <- summary(rrc(pred_sum_c, bl = "control", linkinv = linkinv),
                               mean, median, mode=ggdist::Mode, sd, mad, quantile2)
    pred_sum_c.mar <- summarize_predictions(pred$ten_loc,
                                            ids = id.list,
                                            id.col = "id",
                                            draw.chunk = 125,
                                            n.cores = 4)
    colnames(pred_sum_c.mar)[1] <- "control"
    effects.mar[[i]] <- summary(linkinv(pred_sum_c.mar),
                                mean, median, mode=ggdist::Mode, sd, mad, quantile2)
    effects.bl.mar[[i]] <- summary(rrc(pred_sum_c.mar, bl = "control", linkinv = linkinv),
                                   mean, median, mode=ggdist::Mode, sd, mad, quantile2)
    effects.mar.dif[[i]] <- summary(rrc(pred_sum_c.mar, bl = "control", linkinv = linkinv) -
                                    rrc(pred_sum_c, bl = "control", linkinv = linkinv),
                                        mean, median, mode=ggdist::Mode, sd, mad, quantile2)
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

predictions <- pred$full
           fun = mean
           ids = id.list
           draw.ids = NULL
           draw.chunk = NULL
           agg.size = NULL
           clamp = NULL
           n.threads = NULL
           progress = TRUE


evaluate_data <- cam.mod[1:1e4,]
summarize_data <- cam.data[1:1e4,]

effects <- summarize_effects(models, cam.mod[1:1e4,], cam.data[1:1e4,])

effects[[1]]
effects[[2]]

effects_pan <- summarize_effects(models, cam.mod[cam.mod$adm0 == "PAN",], cam.data[adm0 == "PAN",])
effects_gtm <- summarize_effects(models, cam.mod[cam.mod$adm0 == "GTM",], cam.data[adm0 == "GTM",])

invlink_cll(effects_pan[[1]][[3]]$mean[1:9])
plogis(effects_pan[[1]][[3]]$mean[10:18])
pcauchy(effects_pan[[1]][[3]]$mean[19:27])

effects_pan[[1]]
effects_pan[[2]]

effects_gtm[[1]]
effects_gtm[[2]]

library(pROC)
library(ggdist)

par(mfrow = c(2,3))
for(i in 1:length(models)) {
  lfun <- models[[1]]$family$linkinv
  predicted <- lfun(summarise_draws(effects[[2]][[i]]$full, mean)$mean)
  response <- as.integer(cam.data$forestloss[1:1e4])
  mod.roc <- roc(response, predicted)
  plot(mod.roc)
  print(mod.roc)
}

par(mfrow = c(2,3))
for(i in 1:length(models)) {
  lfun <- models[[1]]$family$linkinv
  predicted <- lfun(summarise_draws(effects[[2]][[i]]$full, Mode)$Mode)
  response <- as.integer(cam.data$forestloss[1:1e4])
  mod.roc <- roc(response, predicted)
  plot(mod.roc)
  print(mod.roc)
}

par(mfrow = c(2,3))
for(i in 1:length(models)) {
  lfun <- models[[1]]$family$linkinv
  predicted <- lfun(summarise_draws(effects_pan[[2]][[i]]$full, mean)$mean)
  response <- as.integer(cam.data[adm0 == "PAN", forestloss])
  mod.roc <- roc(response, predicted)
  plot(mod.roc)
  print(mod.roc)
}

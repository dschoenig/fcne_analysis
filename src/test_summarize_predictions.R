library(mgcv)
library(data.table)
library(posterior)
library(foreach)
library(doParallel)

source("utilities.R")

# Mock model
data <- gamSim(n = 1e4)
data$group1 <- as.factor(letters[sample(1:4, nrow(data), replace = TRUE)])
data$group2 <- as.factor(letters[sample(23:24, nrow(data), replace = TRUE)])
data$id <- sample(1:nrow(data), nrow(data))
model <- gam(y ~ s(x0) +s(x1) +s(x2) +s(x3), data = data)
post <- rmvn(n = 1000, mu = coef(model), V = vcov(model))

predictions <- evaluate_posterior(model, post, data, id.col = "id")

summarize_predictions <- function(x, ...) {
  UseMethod("summarize_predictions", x)
}

summarize_predictions.draws_matrix <- 
  function(predictions,
           fun = mean,
           ids = NULL,
           draw.chunk = NULL,
           n.cores = 1,
           ...) {
  if(is.null(ids)) {
    ids <- 1:ncol(predictions)
  }
  idx <- which(colnames(predictions) %in% ids)
  if(is.null(draw.chunk)) {
    draw.chunk <- nrow(predictions)
  }
  draw.chunks <- chunk_seq(1, nrow(predictions), draw.chunk)
  registerDoParallel(cores = n.cores)
  pred_summarized <-
    foreach(i = 1:length(draw.chunks$from),
            .combine = c) %dopar% {
      pred_summarized_chunk <- 
        apply(predictions[draw.chunks$from[i]:draw.chunks$to[i],idx], 1, fun)
    }
  stopImplicitCluster()
  return(rvar(pred_summarized))
}


system.time({
rr <- summarize_predictions(predictions, ids = 10, draw.chunk = 25, n.cores = 4)
})
rr


library(arrow)
predictions <- open_dataset("../models/gam/lp/cam.lp.mar")

summarize_predictions.FileSystemDataset <- 
  function(predictions,
           fun = mean,
           ids = NULL,
           id.col = NULL,
           draw.prefix = "draw",
           draw.chunk = NULL,
           n.cores = 1,
           ...) {
  if(is.null(ids)) {
    ids <- 1:nrow(predictions)
  }
  if(is.null(id.col)) {
    source.ids <- 1:nrow(predictions)
  } else {
    source.ids <- as.data.frame(predictions[, id.col])[[id.col]]
  }
  idx <- which(source.ids %in% ids)
  if(is.null(draw.prefix)) {
    draw.cols <- 1:ncol(predictions)
  } else {
    draw.cols <- grep(draw.prefix, names(predictions))
  }
  if(is.null(draw.chunk)) {
    draw.chunk <- length(draw.cols)
  }
  draw.chunks <- chunk_seq(1, length(draw.cols), draw.chunk)
  registerDoParallel(cores = n.cores)
  predictions.summarized <-
    foreach(i = 1:length(draw.chunks$from),
            .combine = c) %dopar% {
      predictions.pulled <- 
        as.data.frame(predictions[idx,
                                  draw.cols[draw.chunks$from[i]:draw.chunks$to[i]]])
      predictions.summarized.chunk <- 
        apply(t(as.matrix(predictions.pulled)), 1, fun)
      return(predictions.summarized.chunk)
    }
  stopImplicitCluster()
  return(rvar(predictions.summarized))
}

system.time({
  rr <- summarize_predictions(predictions, id.col = "id", draw.chunk = 250, n.cores = 4)
})

draws_of(rr)


summarize_predictions_by <- function(x, ...) {
  UseMethod("summarize_predictions_by", x)
}

summarize_predictions_by <- 
  function(predictions,
           data = NULL,
           fun = mean,
           id.col = NULL,
           group.vars = NULL,
           group.labels = NULL,
           draw.prefix = "draw",
           draw.chunk = NULL,
           n.cores = 1,
           ...) {
  setDT(data)
  if(is.null(id.col)) {
    groups <- data[, .(n = .N, ids = list(.I)), group.vars]
  } else {
    groups <- data[, .(n = .N, ids = list(data$id[.I])), group.vars]
  }
  if(!is.null(group.labels)) {
    for(i in 1:length(group.vars)) {
      old.labels <- as.character(groups[[group.vars[i]]])
      new.labels <- factor(group.labels[[i]][old.labels], 
                           levels = group.labels[[i]])
      groups[[group.vars[i]]] <- new.labels
    }
  }
  summary.labels <- do.call(paste, c(groups[, 1:length(group.vars)] , sep = "."))
  groups.summarized <- list()
  for(i in 1:nrow(groups)) {
    group.summary <- 
      summarize_predictions(predictions,
                            fun = fun,
                            ids = unlist(groups[i, ids]),
                            id.col = id.col,
                            draw.prefix = draw.prefix,
                            draw.chunk = draw.chunk,
                            n.cores = n.cores
                            # , ...
                            )
    groups.summarized[[summary.labels[i]]] <- group.summary
  }
  return(as_draws_matrix(groups.summarized))
}


cam.data <- readRDS("../data/processed/cam.data.proc.rds")
cam.data <- cam.data[1:1e5,]


library(arrow)
predictions <- open_dataset("../models/gam/lp/cam.lp.mar")



bl.cam <- summarize_predictions(predictions, 
                                ids = cam.data[pa_type == "none" & it_type == "none", id],
                                fun = mean, id.col = "id",
                                draw.chunk = 250, n.cores = 4)
bl.cam

bl.pan <- summarize_predictions(predictions, 
                                ids = cam.data[pa_type == "none" &
                                               it_type == "none" &
                                               adm0 == "PAN",
                                               id],
                                fun = mean, id.col = "id",
                                draw.chunk = 250, n.cores = 4)
bl.pan



sr <- summarize_predictions_by(predictions,
                               data = cam.data[it_type != "none" & adm0 == "PAN",], 
                               fun = mean, id.col = "id",
                               group.vars = "it_type",
                               draw.chunk = 250, n.cores = 4)

summary(expm1(sr - as.numeric(draws_of(bl.pan))))


sr <- summarize_predictions_by(predictions,
                               data = cam.data[pa_type != "none",], 
                               fun = mean, id.col = "id",
                               group.vars = "pa_type",
                               draw.chunk = 250, n.cores = 4)
sr - as.numeric(draws_of(bl.cam))


cam.data[it_type == "none", sum(forestloss)/.N, c("pa_type", "it_type")]

cam.data
cam.data[1:1e5, sum(forestloss)/.N, c("pa_type", "it_type")]
cam.data[, sum(forestloss)/.N, c("pa_type", "it_type")]


amz.data <- readRDS("../data/processed/cam.data.proc.rds")
amz.data <- cam.data[1:1e5,]
amz.data[1:1e5, sum(forestloss)/.N, c("pa_type", "it_type")]
amz.data[, sum(as.integer(forestloss))/.N, c("pa_type", "it_type")]


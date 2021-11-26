args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(mvnfast)
library(posterior)

source("utilities.R")

n.threads <- ifelse(length(args) < 1, 1, as.integer(args[1]))

path.gam <- "../models/gam/test_effects/"

regions <- c("cam")
# regions <- c("amz")
seed <- 18980605


## SIMULATION FROM MULTIVARIATE APPROXIMATION OF MODEL POSTERIOR ###############

for(i in 1:length(regions)) {
  file.model <- paste0(path.gam, regions[i], ".t1.rds")
  file.post <- paste0(path.gam, regions[i], ".t1.post.rds")

  # Load model
  model <- readRDS(file.model)

  # Posterior draws
  set.seed(seed + i)
  # post <- mvnfast::rmvn(1000,
  #                       mu = coef(model),
  #                       sigma = vcov(model, unconditional = TRUE),
  #                       ncores = n.threads)
  post <- mgcv::rmvn(1000,
                     mu = coef(model),
                     V = vcov(model, unconditional = TRUE))
  colnames(post) <- names(coef(model))
  post <- as_draws_matrix(post)
  saveRDS(post, file.post)

  rm(model, post)
  gc()
}


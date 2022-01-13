args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(mvnfast)
library(posterior)

source("utilities.R")

region <- tolower(as.character(args[1]))
n.threads <- ifelse(length(args) < 2, 1, as.integer(args[2]))

path.gam <- "../models/gam/"

seed <- 18980605


## SIMULATION FROM MULTIVARIATE NORMAL APPROXIMATION OF MODEL POSTERIOR ########

file.model <- paste0(path.gam, region, ".m3.rds")
file.post <- paste0(path.gam, region, ".m3.post.rds")

# Load model
model <- readRDS(file.model)


# Posterior draws
set.seed(seed)
post <- rmvn(1000,
             mu = coef(model),
             sigma = vcov(model, unconditional = TRUE),
             ncores = n.threads)
colnames(post) <- names(coef(model))
post <- as_draws_matrix(post)
saveRDS(post, file.post)

# # Posterior draws
# set.seed(seed)
# post <- mgcv::rmvn(1000,
#                    mu = coef(model),
#                    V = vcov(model, unconditional = TRUE))
# colnames(post) <- names(coef(model))
# post <- as_draws_matrix(post)
# saveRDS(post, file.post)

# rm(model, post)
# gc()

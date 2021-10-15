library(mgcv)
library(mvnfast)
library(posterior)

source("utilities.R")

path.gam <- "../models/gam/"

seed <- 18980605

## AMAZON ######################################################################

# Load model
amz.m2 <- readRDS(paste0(path.gam, "amz.m2.rds"))

# Posterior draws
set.seed(seed)
amz.post <- mvnfast::rmvn(1000,
                          mu = coef(amz.m2),
                          sigma = vcov(amz.m2, unconditional = TRUE),
                          ncores = 8)
colnames(amz.post) <- names(coef(amz.m2))
amz.post <- as_draws_matrix(amz.post)
saveRDS(amz.post, paste0(path.gam, "amz.m2.post.rds"))

rm(amz.m2, amz.post)
gc()

## CENTRAL AMERICA #############################################################

# Load model
cam.m2 <- readRDS(paste0(path.gam, "cam.m2.rds"))

# Posterior draws
set.seed(seed+1)
cam.post <- mvnfast::rmvn(1000,
                          mu = coef(cam.m2),
                          sigma = vcov(cam.m2, unconditional = TRUE),
                          ncores = 8)
colnames(cam.post) <- names(coef(cam.m2))
cam.post <- as_draws_matrix(cam.post)
saveRDS(cam.post, paste0(path.gam, "cam.m2.post.rds"))

rm(cam.m2, cam.post)

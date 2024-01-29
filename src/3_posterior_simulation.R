args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(mvnfast)
library(posterior)

source("utilities.R")

region <- tolower(as.character(args[1]))
n.threads <- ifelse(length(args) < 2, 1, as.integer(args[2]))
# region <- "cam"
# n.threads <- 4

path.gam <- "../models/gam/"

seed <- 18980605


## SIMULATION FROM MULTIVARIATE NORMAL APPROXIMATION OF MODEL POSTERIOR ########

file.model <- paste0(path.gam, region, ".m7.rds")
file.post <- paste0(path.gam, region, ".m7.post.rds")

paste0("Simulating 1000 draws from the approximate posterior distribution\n",
       "of model ", file.model, " …") |>
message()

# Load model
model <- readRDS(file.model)


post <- 
  egp_posterior_draw(model,
                     n = 1000,
                     unconditional = TRUE,
                     package = "mvnfast",
                     parallel = 4)


# Posterior draws
set.seed(seed)
post <-
  mvnfast::rmvn(1000,
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

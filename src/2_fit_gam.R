args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(mgcv)
library(kohonen)
library(mvnfast)
source("utilities.R")

## Paths
path.data.proc <- "../data/processed/"
path.som <- "../models/som/"
path.gam <- "../models/gam/"

model.reg <- tolower(as.character(args[1]))
model.id <- as.integer(args[2])
if(length(args) < 3) {
  n.threads <- c(2,1)
} else {
  n.threads <- as.integer(args[3])
}
# model.reg <- "cam"
# model.id <- 6
# n.threads <- c(2,1)


if(!dir.exists(path.gam))
  dir.create(path.gam, recursive = TRUE)

file.data.proc <- paste0(path.data.proc, model.reg, ".data.fit.proc.rds")
model.name <- paste0(model.reg, ".m", model.id)

k.reg <- list(cam = c(
                      # ten_loc.bl = 5000,
                      ten_loc.bl = 2500,
                      ten_loc.forp = 2500,
                      ten_loc.itpa = 1000,
                      ten_loc.ov = 500,
                      som = 1000),
              amz = c(
                      # ten_loc.bl = 5000,
                      ten_loc.bl = 2500,
                      ten_loc.forp = 2500,
                      ten_loc.itpa = 1000,
                      ten_loc.ov = 500,
                      som = 1000))
# Increase number of maximum knots 10-fold (default: 2000)
max.knots.reg <- list(cam = c(
                              ten_loc.bl = 2e4,
                              ten_loc.forp = 2e4,
                              ten_loc.itpa = 2e4,
                              ten_loc.ov = 2e4,
                              som = 2e4),
                      amz = c(
                              ten_loc.bl = 2e4,
                              ten_loc.forp = 2e4,
                              ten_loc.itpa = 2e4,
                              ten_loc.ov = 2e4,
                              som = 2e4))
# max.knots.reg <- list(cam = c(k.reg$cam[1:3] * 10, som = 10000),
#                       amz = c(k.reg$amz[1:3] * 10, som = 10000))

# Fitting parameters
conv.eps <- 1e-7 # Default is 1e-7
max.discrete.bins <- 1e5 # Default for bivariate smooths is effectively 1e4
# max.discrete.bins <- 1e4 # Default for bivariate smooths is effectively 1e4

## FIT MODELS ##################################################################

data.proc <- readRDS(file.data.proc)
data.mod <- 
  as.data.frame(data.proc[, .(forestloss,
                          for_type, it_type, pa_type, overlap, ed_east, ed_north,
                          adm0, som_x, som_y)])
data.mod$b0 <- model.matrix(~ 1, data.mod)
rm(data.proc)

k.def <- k.reg[[model.reg]]
max.knots.def <- max.knots.reg[[model.reg]]

# # FOR TESTING ONLY:
# k.def = k.def / 100
# max.knots.def = max.knots.def / 10
# data.mod <- data.mod[1:1e5,]

message(paste0("Fitting model `", model.name, "` …"))

a <- Sys.time()

if(model.id == 0) {
  k.loc <- sum(k.def["ten_loc.bl"], 4 * k.def["ten_loc.itpa"], 4 * k.def["ten_loc.ov"]) 
  max.knots.loc <- max.knots.def["ten_loc.bl"]
  model <-
    bam(forestloss ~
        s(ed_east, ed_north, bs = 'gp',
          k = k.loc,
          xt = list(max.knots = max.knots.loc)),
        family = binomial(link = "logit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

if(model.id == 1) {
  model <-
    bam(forestloss ~
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "logit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

if(model.id == 2) {
  k.loc <- sum(k.def["ten_loc.bl"], 4 * k.def["ten_loc.itpa"], 4 * k.def["ten_loc.ov"]) 
  max.knots.loc <- 10 * k.loc
  model <-
    bam(forestloss ~
        s(ed_east, ed_north, bs = 'gp',
          k = k.loc,
          xt = list(max.knots = max.knots.loc)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "logit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

if(model.id == 3) {
  model <-
    bam(forestloss ~
        # Tenure effects, continuous variation over geographic location
        s(ed_east, ed_north, bs = 'gp',
          k = k.def["ten_loc.bl"],
          xt = list(max.knots = max.knots.def["ten_loc.bl"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def["ten_loc.ov"],
          xt = list(max.knots = max.knots.def["ten_loc.ov"])) +
        # Tenure effects, discontinuous variation between countries
        s(adm0, bs = "re") +
        s(adm0, it_type, bs = "re") +
        s(adm0, pa_type, bs = "re") +
        s(adm0, it_type, pa_type, bs = "re") +
        # Covariates
        s(som_x, som_y, bs = 'gp', k = 5*k.def["som"],
          xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

if(model.id == 4) {
  # Vary covariate effects by country
  model <-
    bam(forestloss ~
        # Tenure effects, continuous variation over geographic location
        s(ed_east, ed_north, bs = 'gp',
          k = k.def["ten_loc.bl"],
          xt = list(max.knots = max.knots.def["ten_loc.bl"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def["ten_loc.ov"],
          xt = list(max.knots = max.knots.def["ten_loc.ov"])) +
        # Tenure effects, discontinuous variation between countries
        s(adm0, it_type, bs = "re") +
        s(adm0, pa_type, bs = "re") +
        s(adm0, it_type, pa_type, bs = "re") +
        # Covariates
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"],
          xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "logit"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

if(model.id == 5) {
  # Same as `4`, but with cloglog link
  model <-
    bam(forestloss ~
        # Tenure effects, continuous variation over geographic location
        s(ed_east, ed_north, bs = 'gp',
          k = k.def["ten_loc.bl"],
          xt = list(max.knots = max.knots.def["ten_loc.bl"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def["ten_loc.ov"],
          xt = list(max.knots = max.knots.def["ten_loc.ov"])) +
        # Tenure effects, discontinuous variation between countries
        s(adm0, it_type, bs = "re") +
        s(adm0, pa_type, bs = "re") +
        s(adm0, it_type, pa_type, bs = "re") +
        # Covariates
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"],
          xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}


if(model.id == 6) {
  # Same as `3`, but vary baseline risk by forest type
  model <-
    bam(forestloss ~
        # Tenure effects, continuous variation over geographic location
        s(ed_east, ed_north, bs = 'gp',
          k = k.def["ten_loc.bl"],
          xt = list(max.knots = max.knots.def["ten_loc.bl"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = for_type, k = k.def["ten_loc.forp"],
          xt = list(max.knots = max.knots.def["ten_loc.forp"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def["ten_loc.ov"],
          xt = list(max.knots = max.knots.def["ten_loc.ov"])) +
        # Tenure effects, discontinuous variation between countries
        s(adm0, bs = "re") +
        s(adm0, it_type, bs = "re") +
        s(adm0, pa_type, bs = "re") +
        s(adm0, it_type, pa_type, bs = "re") +
        # Covariates
        s(som_x, som_y, bs = 'gp', k = 5*k.def["som"],
          xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

if(model.id == 7) {
  # Same as `3`, but vary baseline risk by forest type, doubled k
  model <-
    bam(forestloss ~
        # Tenure effects, continuous variation over geographic location
        s(ed_east, ed_north, bs = 'gp',
          k = k.def["ten_loc.bl"] * 2,
          xt = list(max.knots = max.knots.def["ten_loc.bl"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = for_type, k = k.def["ten_loc.forp"] * 2,
          xt = list(max.knots = max.knots.def["ten_loc.forp"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def["ten_loc.itpa"],
          xt = list(max.knots = max.knots.def["ten_loc.itpa"])) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def["ten_loc.ov"],
          xt = list(max.knots = max.knots.def["ten_loc.ov"])) +
        # Tenure effects, discontinuous variation between countries
        s(adm0, bs = "re") +
        s(adm0, it_type, bs = "re") +
        s(adm0, pa_type, bs = "re") +
        s(adm0, it_type, pa_type, bs = "re") +
        # Covariates
        s(som_x, som_y, bs = 'gp', k = 5*k.def["som"],
          xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}


b <- Sys.time()
b - a

model
summary(model, re.test = FALSE)
k.check(model)
AIC(model)

print("Saving fitted model …")
saveRDS(model, paste0(path.gam, model.name, ".rds"))


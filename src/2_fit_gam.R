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
n.threads <- ifelse(length(args) < 3, c(2,1), as.integer(args[3]))

if(!dir.exists(path.gam))
  dir.create(path.gam, recursive = TRUE)

file.data.proc <- paste0(path.data.proc, model.reg, ".data.fit.proc.rds")
model.name <- paste0(model.reg, ".m", model.id)

k.def = 1000
max.knots.def = 10000

## FIT MODELS ##################################################################


data.proc <- readRDS(file.data.proc)

# # FOR TESTING ONLY:
# k.def = 100
# max.knots.def = 1000
# data.proc <- data.proc[1:1e5,]

data.mod <- 
  as.data.frame(data.proc[, .(forestloss,
                          it_type, pa_type, overlap, ed_east, ed_north,
                          adm0, som_x, som_y)])
data.mod$b0 <- model.matrix(~ 1, data.mod)
rm(data.proc)

print(paste0("Fitting model `", model.name, "` …"))

if(model.id == 0) {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(ed_east, ed_north, bs = 'gp',
          k = 2*k.def, xt = list(max.knots = max.knots.def)),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == 1) {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(som_x, som_y, bs = 'gp',
          k = k.def, xt = list(max.knots = max.knots.def)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def, xt = list(max.knots = max.knots.def)),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == 2) {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(ed_east, ed_north, bs = 'gp',
          k = 2*k.def, xt = list(max.knots = max.knots.def)) +
        s(som_x, som_y, bs = 'gp',
          k = k.def, xt = list(max.knots = max.knots.def)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def, xt = list(max.knots = max.knots.def)),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == 3) {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(ed_east, ed_north, bs = 'gp',
          k = k.def, xt = list(max.knots = max.knots.def)) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def, xt = list(max.knots = max.knots.def)) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def, xt = list(max.knots = max.knots.def)) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def, xt = list(max.knots = max.knots.def)) +
        s(som_x, som_y, bs = 'gp',
          k = k.def, xt = list(max.knots = max.knots.def)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def, xt = list(max.knots = max.knots.def)),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

warnings()

print("Saving fitted model ...")
saveRDS(model, paste0(path.gam, model.name, ".rds"))



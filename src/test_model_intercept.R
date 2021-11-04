args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(mgcv)
library(kohonen)
library(mvnfast)
source("utilities.R")

## Paths
path.data.int <- "../data/intermediate/"
path.data.proc <- "../data/processed/"
path.som <- "../models/som/"
path.gam <- "../models/gam/"

model.reg <- tolower(as.character(args[1]))
model.id <- as.character(args[2])
n.threads <- ifelse(length(args) < 3, c(2,1), as.integer(args[3]))

dir.create(path.gam, recursive = TRUE)
dir.create(path.data.proc, recursive = TRUE)

file.data.int <- paste0(path.data.int, model.reg, ".data.int.rds")
file.data.proc <- paste0(path.data.proc, model.reg, ".data.proc.rds")
file.som <- paste0(path.som, model.reg, ".som.1e6.rds")
file.som.mapped <- paste0(path.data.int, model.reg, ".som.mapped")
model.name <- paste0(model.reg, ".m3_", model.id)



## FIT MODELS ##################################################################


print("Processing data ...")
if(file.exists(file.data.proc)) {
  data.proc <- readRDS(file.data.proc)
} else {
  data.proc <- readRDS(file.data.int)
  som.fit <- readRDS(file.som)
  som.mapped <- readRDS(file.som.mapped)

  som.xy <- data.table(som.fit$grid$pts[som.mapped$unit.classif,])
  data.proc[, `:=`(som_x = som.xy$x, som_y = som.xy$y)]
  saveRDS(data.proc, file.data.proc)
}


k.def = 1000
max.knots.def = 10000

# k.def = 100
# max.knots.def = 1000
# data.proc <- data.proc[1:1e5,]

data.mod <- as.data.frame(data.proc)
rm(data.proc)

print(paste0("Fitting model `", model.name, "` ..."))

if(model.id == "flat") {
  model <-
    bam(forestloss ~
        s(ed_east, ed_north, bs = 'gp',
          k = k.def, xt = list(max.knots = max.knots.def)) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def, xt = list(max.knots = max.knots.def)) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def, xt = list(max.knots = max.knots.def)) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def, xt = list(max.knots = max.knots.def)) +
        s(it_type, bs = "re") +
        s(pa_type, bs = "re") +
        s(it_type, pa_type, bs = "re") +
        s(som_x, som_y, bs = 'gp',
          k = k.def, xt = list(max.knots = max.knots.def)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def, xt = list(max.knots = max.knots.def)) +
        s(adm0, bs = "re"),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}


if(model.id == "normal") {
  data.mod$b0 <- model.matrix(~ 1, data.mod)
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
        s(it_type, bs = "re") +
        s(pa_type, bs = "re") +
        s(it_type, pa_type, bs = "re") +
        s(som_x, som_y, bs = 'gp',
          k = k.def, xt = list(max.knots = max.knots.def)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def, xt = list(max.knots = max.knots.def)) +
        s(adm0, bs = "re"),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "mpf") {
  data.mod$beta <- model.matrix(~ it_type * pa_type + adm0, data = data.mod,
                                contrasts.arg = list(it_type = contr.treatment,
                                                     pa_type = contr.treatment,
                                                     adm0 = contr.treatment))[,-1]
  colnames(data.mod$beta) <-
    with(data.mod, c(paste0("it_type-", levels(it_type)[-1]),
                     paste0("pa_type-", levels(pa_type)[-1]),
                     paste0("adm0-", levels(adm0)[-1]),
                     paste0("it_type-", rep(levels(it_type)[-1],
                                           length(levels(pa_type))-1), ":",
                            "pa_type-", rep(levels(pa_type)[-1],
                                          each = length(levels(it_type)) -1), ":")))
  model <-
    bam(forestloss ~
        1 + beta + 
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
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        paraPen = list(beta = list(diag(15))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "mpn") {
  data.mod$beta <- model.matrix(~ it_type * pa_type + adm0, data = data.mod,
                                contrasts.arg = list(it_type = contr.treatment,
                                                     pa_type = contr.treatment,
                                                     adm0 = contr.treatment))
  colnames(data.mod$beta) <-
    with(data.mod, c("b0",
                     paste0("it_type-", levels(it_type)[-1]),
                     paste0("pa_type-", levels(pa_type)[-1]),
                     paste0("adm0-", levels(adm0)[-1]),
                     paste0("it_type-", rep(levels(it_type)[-1],
                                           length(levels(pa_type))-1), ":",
                            "pa_type-", rep(levels(pa_type)[-1],
                                          each = length(levels(it_type)) -1), ":")))
  model <-
    bam(forestloss ~
        -1 + beta + 
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
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        paraPen = list(beta = list(diag(16))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}


if(model.id == "gp") {
  model <-
    bam(forestloss ~
        -1 +
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
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "gpf") {
  model <-
    bam(forestloss ~
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
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "gplf") {
  model <-
    bam(forestloss ~
        s(it_type, bs = "re") +
        s(pa_type, bs = "re") +
        s(it_type, pa_type, bs = "re") +
        s(adm0, bs = "re") +
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
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "gpn") {
  data.mod$b0 <- model.matrix(~ 1, data.mod)
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
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "gpln") {
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(it_type, bs = "re") +
        s(pa_type, bs = "re") +
        s(it_type, pa_type, bs = "re") +
        s(adm0, bs = "re") +
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
        family = binomial(link = "cloglog"),
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



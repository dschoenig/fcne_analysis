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
# model.reg <- "cam"
# model.id <- "l1"

dir.create(path.gam, recursive = TRUE)
dir.create(path.data.proc, recursive = TRUE)

file.data.int <- paste0(path.data.int, model.reg, ".data.int.rds")
file.data.proc <- paste0(path.data.proc, model.reg, ".data.proc.rds")
file.som <- paste0(path.som, model.reg, ".som.1e6.rds")
file.som.mapped <- paste0(path.data.int, model.reg, ".som.mapped")
model.name <- paste0(model.reg, ".m3_", model.id)



## FIT MODELS ##################################################################


message("Processing data …")
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

if(model.reg == "cam") {
  it.adm.rec <- c("NIC", "PAN", "MEX", "CRI")
  it.adm.nor <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")
  pa.adm.ind <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")
  pa.adm.dir <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")
  itpa.adm.rec.ind <- c("NIC", "PAN", "MEX", "CRI")
  itpa.adm.rec.dir <- c("PAN", "MEX", "CRI")
  itpa.adm.nor.ind <- c("BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV")
  itpa.adm.nor.dir <- c("SLV", "BLZ", "PAN", "GTM", "HND", "CRI")
  data.proc <-
    data.proc[(it_type == "none" & pa_type == "none") |
              (it_type == "recognized" & pa_type == "none" & adm0 %in% it.adm.rec) |
              (it_type == "not_recognized" & pa_type == "none" & adm0 %in% it.adm.nor) |
              (pa_type == "indirect_use" & it_type == "none" & adm0 %in% pa.adm.ind) |
              (pa_type == "direct_use" & it_type == "none" & adm0 %in% pa.adm.dir) |
              (it_type == "recognized" & pa_type == "indirect_use" & adm0 %in% itpa.adm.rec.ind) |
              (it_type == "recognized" & pa_type == "direct_use" & adm0 %in% itpa.adm.rec.dir) |
              (it_type == "not_recognized" & pa_type == "indirect_use" & adm0 %in% itpa.adm.nor.ind) |
              (it_type == "not_recognized" & pa_type == "direct_use" & adm0 %in% itpa.adm.nor.dir)]
}

# Basis dimension and maximum knots considered for eigen decomposition
k.def <- c(ten_loc.bl = 750,
           ten_loc.itpa = 500,
           ten_loc.ov = 250,
           som = 750)
max.knots.def <- c(k.def[1:3] * 10, som = 10000)
# max.knots.def <- c(ten_loc.bl = 10000,
#                    ten_loc.itpa = 10000,
#                    ten_loc.ov = 10000,
#                    som = 10000)

# # For testing:
# k.def = k.def / 10
# max.knots.def = max.knots.def / 10
# set.seed(1234)
# sam <- sample(1:nrow(data.proc), 1e5)
# data.proc <- data.proc[sam,]

# Prepare model fit
k.def <- as.list(k.def)
max.knots.def <- as.list(max.knots.def)
data.mod <- as.data.frame(data.proc)
data.mod$b0 <- model.matrix(~ 1, data.mod)
rm(data.proc)

message(paste0("Fitting model `", model.name, "` …"))

if(model.id == "l0") {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(ed_east, ed_north, bs = 'gp',
          k = k.def$ten_loc.bl, xt = list(max.knots = max.knots.def$ten_loc.bl)) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def$ten_loc.ov, xt = list(max.knots = max.knots.def$ten_loc.ov)) +
        s(som_x, som_y, bs = 'gp',
          k = k.def$som, xt = list(max.knots = max.knots.def$som)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def$som, xt = list(max.knots = max.knots.def$som)),
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

if(model.id == "l1") {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(ed_east, ed_north, bs = 'gp',
          k = k.def$ten_loc.bl, xt = list(max.knots = max.knots.def$ten_loc.bl)) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def$ten_loc.ov, xt = list(max.knots = max.knots.def$ten_loc.ov)) +
        s(som_x, som_y, bs = 'gp',
          k = k.def$som, xt = list(max.knots = max.knots.def$som)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def$som, xt = list(max.knots = max.knots.def$som)),
        family = binomial(link = "logit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "l2") {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(ed_east, ed_north, bs = 'gp',
          k = k.def$ten_loc.bl, xt = list(max.knots = max.knots.def$ten_loc.bl)) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def$ten_loc.ov, xt = list(max.knots = max.knots.def$ten_loc.ov)) +
        s(som_x, som_y, bs = 'gp',
          k = k.def$som, xt = list(max.knots = max.knots.def$som)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def$som, xt = list(max.knots = max.knots.def$som)),
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

if(model.id == "l1f") {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(ed_east, ed_north, bs = 'gp',
          k = k.def$ten_loc.bl, xt = list(max.knots = max.knots.def$ten_loc.bl)) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(it_type, bs = "re") +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(pa_type, bs = "re") +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def$ten_loc.ov, xt = list(max.knots = max.knots.def$ten_loc.ov)) +
        s(overlap, bs = "re") +
        s(som_x, som_y, bs = 'gp',
          k = k.def$som, xt = list(max.knots = max.knots.def$som)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def$som, xt = list(max.knots = max.knots.def$som)) +
        s(adm0, bs = "re"),
        family = binomial(link = "logit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "l2f") {
  model <-
    bam(forestloss ~
        -1 + b0 +
        s(ed_east, ed_north, bs = 'gp',
          k = k.def$ten_loc.bl, xt = list(max.knots = max.knots.def$ten_loc.bl)) +
        s(ed_east, ed_north, bs = 'gp',
          by = it_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(it_type, bs = "re") +
        s(ed_east, ed_north, bs = 'gp',
          by = pa_type, k = k.def$ten_loc.itpa, xt = list(max.knots = max.knots.def$ten_loc.itpa)) +
        s(pa_type, bs = "re") +
        s(ed_east, ed_north, bs = 'gp',
          by = overlap, k = k.def$ten_loc.ov, xt = list(max.knots = max.knots.def$ten_loc.ov)) +
        s(overlap, bs = "re") +
        s(som_x, som_y, bs = 'gp',
          k = k.def$som, xt = list(max.knots = max.knots.def$som)) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def$som, xt = list(max.knots = max.knots.def$som)) +
        s(adm0, bs = "re"),
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

model
summary(model, re.test = FALSE)
AIC(model)

message("Saving fitted model …")
saveRDS(model, paste0(path.gam, model.name, ".rds"))



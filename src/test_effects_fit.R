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
path.gam <- "../models/gam/test_effects/"

model.reg <- tolower(as.character(args[1]))
model.id <- tolower(args[2])
n.threads <- ifelse(length(args) < 3, c(2,1), as.integer(args[3]))

# model.reg <- "cam"
# model.id <- "t1"
# n.threads <- c(2,1)


dir.create(path.gam, recursive = TRUE)
dir.create(path.data.proc, recursive = TRUE)

file.data.int <- paste0(path.data.int, model.reg, ".data.int.rds")
file.data.proc <- paste0(path.data.proc, model.reg, ".data.proc.rds")
file.som <- paste0(path.som, model.reg, ".som.1e6.rds")
file.som.mapped <- paste0(path.data.int, model.reg, ".som.mapped")
model.name <- paste0(model.reg, ".", model.id)

k.def = 750
max.knots.def = 10000

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

# FOR TESTING ONLY:
# k.def = 100
# max.knots.def = 1000
# data.proc <- data.proc[1:1e5,]

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

print(data.proc[order(it_type, pa_type, adm0), .N, c("it_type", "pa_type", "adm0")])

data.mod <- as.data.frame(data.proc)
data.mod$b0 <- model.matrix(~ 1, data.mod)
rm(data.proc)

print(paste0("Fitting model `", model.name, "` ..."))

if(model.id == "t1") {
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

warnings()
summary(model, re.test = FALSE)

print("Saving fitted model ...")
saveRDS(model, paste0(path.gam, model.name, ".rds"))



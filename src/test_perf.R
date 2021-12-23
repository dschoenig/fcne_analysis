args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(mgcv)
library(kohonen)
library(mvnfast)
source("utilities.R")

## Paths
path.data.proc <- "../data/processed/"
path.gam <- "../models/gam/"

model.reg <- tolower(as.character(args[1]))
model.id <- as.character(args[2])
n.threads <- ifelse(length(args) < 3, c(2,1), as.integer(args[3]))

if(!dir.exists(path.gam))
  dir.create(path.gam, recursive = TRUE)

file.data.proc <- paste0(path.data.proc, model.reg, ".data.fit.proc.rds")
model.name <- paste0(model.reg, ".m", model.id)


## FIT MODELS ##################################################################


data.proc <- readRDS(file.data.proc)


print(paste0("Fitting model `", model.name, "` …"))


if(model.id == "t0") {
  
  # Model size
  k.def <- c(ten_loc.bl = 75,
             ten_loc.itpa = 50,
             ten_loc.ov = 25,
             som = 75)
  max.knots.def <- c(k.def[1:3] * 10, som = 2000)
  data.proc <- data.proc[1:1e5,]

  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          k = k.def["som"], xt = list(max.knots = max.knots.def["som"])) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "logit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = 1,
        gc.level = 0
        )
}

if(model.id == "t1") {
  
  # Model size
  k.def <- c(ten_loc.bl = 75,
             ten_loc.itpa = 50,
             ten_loc.ov = 25,
             som = 75)
  max.knots.def <- c(k.def[1:3] * 10, som = 2000)
  data.proc <- data.proc[1:5e5,]

  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          k = k.def["som"], xt = list(max.knots = max.knots.def["som"])) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
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


if(model.id == "t2") {
  
  # Model size
  k.def <- c(ten_loc.bl = 75,
             ten_loc.itpa = 50,
             ten_loc.ov = 25,
             som = 75)
  max.knots.def <- c(k.def[1:3] * 10, som = 2000)
  data.proc <- data.proc[5:5e5,]

  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          k = k.def["som"], xt = list(max.knots = max.knots.def["som"])) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
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


if(model.id == "t3") {
  
  # Model size
  k.def <- c(ten_loc.bl = 250,
             ten_loc.itpa = 150,
             ten_loc.ov = 50,
             som = 250)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.proc <- data.proc[1:1e6,]

  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          k = k.def["som"], xt = list(max.knots = max.knots.def["som"])) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
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


if(model.id == "t4") {
  
  # Model size
  k.def <- c(ten_loc.bl = 250,
             ten_loc.itpa = 250,
             ten_loc.ov = 250,
             som = 250)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.proc <- data.proc[1:1e6,]

  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          k = k.def["som"], xt = list(max.knots = max.knots.def["som"])) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
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

if(model.id == "t5") {
  
  # Model size
  k.def <- c(ten_loc.bl = 250,
             ten_loc.itpa = 250,
             ten_loc.ov = 250,
             som = 250)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)

  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          k = k.def["som"], xt = list(max.knots = max.knots.def["som"])) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
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

if(model.id == "t6") {
  
  # Model size
  k.def <- c(ten_loc.bl = 750,
             ten_loc.itpa = 500,
             ten_loc.ov = 250,
             som = 750)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.proc <- data.proc[1:1e6,]
  
  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          k = k.def["som"], xt = list(max.knots = max.knots.def["som"])) +
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e5,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}


if(model.id == "t7") {
  
  # Model size
  k.def <- c(ten_loc.bl = 750,
             ten_loc.itpa = 500,
             ten_loc.ov = 250,
             som = 750)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.proc <- data.proc[1:2e6,]
  
  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 1e5,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}


if(model.id == "t8") {
  
  # Model size
  k.def <- c(ten_loc.bl = 750,
             ten_loc.itpa = 500,
             ten_loc.ov = 250,
             som = 750)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.proc <- data.proc[1:4e6,]
  
  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 1e5,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}


if(model.id == "t9") {
  
  # Model size
  k.def <- c(ten_loc.bl = 750,
             ten_loc.itpa = 500,
             ten_loc.ov = 250,
             som = 750)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.proc <- data.proc[1:6e6,]
  
  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)

  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 1e5,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "t9b") {
  # Model size
  k.def <- c(ten_loc.bl = 750,
             ten_loc.itpa = 500,
             ten_loc.ov = 250,
             som = 750)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.proc <- data.proc[1:6e6,]
  data.mod <- 
    as.data.frame(data.proc[, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)
  model <-
    bam(forestloss ~
        -1 +
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
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        chunk.size = 1e5,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "t10") {
  # Model size
  k.def <- c(ten_loc.bl = 750,
             ten_loc.itpa = 500,
             ten_loc.ov = 250,
             som = 750)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.mod <- 
    as.data.frame(data.proc[1:5e6, .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)
  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 1e5,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}


if(model.id == "t11") {
  # Model size
  k.def <- c(ten_loc.bl = 750,
             ten_loc.itpa = 500,
             ten_loc.ov = 250,
             som = 750)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.mod <- 
    as.data.frame(data.proc[(6e6+1):1e7,
                            .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)
  model <-
    bam(forestloss ~
        -1 + 
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
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 1e5,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0
        )
}

if(model.id == "t12") {
  # Model size
  k.def <- c(ten_loc.bl = 750,
             ten_loc.itpa = 500,
             ten_loc.ov = 250,
             som = 750)
  max.knots.def <- c(k.def[1:3] * 10, som = 10000)
  data.mod <- 
    as.data.frame(data.proc[,
                            .(forestloss,
                            it_type, pa_type, overlap, ed_east, ed_north,
                            adm0, som_x, som_y)])
  data.mod$b0 <- model.matrix(~ 1, data.mod)
  rm(data.proc)
  model <-
    bam(forestloss ~
        -1 + b0 +
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
        s(som_x, som_y, bs = 'gp',
          by = adm0, k = k.def["som"], xt = list(max.knots = max.knots.def["som"])),
        family = binomial(link = "cauchit"),
        data = data.mod,
        select = TRUE,
        paraPen = list(b0 = list(diag(1))),
        chunk.size = 5e3,
        discrete = TRUE,
        nthreads = n.threads,
        gc.level = 0,
        control = gam.control(trace = TRUE),
        )
}


warnings()

print("Saving fitted model …")
saveRDS(model, paste0(path.gam, model.name, ".rds"))



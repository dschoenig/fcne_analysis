library(data.table)
library(mgcv)
library(ff)
library(posterior)
library(parallel)
library(doParallel)
library(foreach)

source("utilities.R")
options("ffbatchsize" = 1, "ffbatchbytes" = 2 * 2^30)


evaluate_posterior_par <- 
  function(
           # model, 
           # posterior,
           # newdata = NULL,
           # id.col = NULL,
           # type = "link",
           # obs = NULL,
           # coef = NULL,
           n = 1,
           m = 1,
           row.chunk = NULL,
           predict.chunk = NULL,
           post.chunk = NULL,
           on.disk = FALSE,
           cluster,
           # n.cores = 1,
           storage.mode = "double",
           storage.path = NULL,
           ff.finalizer = "delete"
           # gc = TRUE
           ) {
  # if(is.null(dim(posterior))) {
  #   posterior <- matrix(posterior, ncol = length(posterior))
  # }
  # if(is.null(newdata)) {
  #   data <- model.frame(model)
  # } else {
  #   data <- newdata
  # }
  # if(is.numeric(obs)) {
  #   data <- data[obs,]
  # }
  # n <- nrow(data)
  # m <- nrow(posterior)
  if(is.null(row.chunk)) row.chunk <- n
  if(is.null(predict.chunk)) predict.chunk <- row.chunk
  if(is.null(post.chunk)) post.chunk <- m
  # weights <- model$prior.weights
  # scale <- model$sig2
  row.from <- seq(1, n, row.chunk)
  if(length(row.from) > 1) {
    row.to <- c(row.from[2:length(row.from)]-1, n)
  } else {
    row.to <- n
  }
  row.chunks <- row.to - c(0, row.to[-length(row.to)])
  post.from <- seq(1, m, post.chunk)
  if(length(post.from) > 1) {
    post.to <- c(post.from[2:length(post.from)]-1, m)
  } else {
    post.to <- m
  }
  post.chunks <- post.to - c(0, post.to[-length(post.to)])
  # Set excluded coefficients to 0
  # if(is.numeric(coef)) {
  #   posterior[,-coef] <- 0
  # }
  evaluated <- list()
  # if(!is.null(id.col)) {
  #   row.ids <- data[[id.col]]
  # }
  if(is.null(storage.path)) {
    storage.path <- getOption("fftempdir")
  } 
  registerDoParallel(cl = cluster)
  # # Reduce data transfer
  # model$model <- NA
  evaluated.l <- 
    foreach(i = 1:length(row.from), .packages = "ff") %dopar% {
      print(paste0(Sys.time(),
                   " Processing row chunk ", i, " of ", length(row.from), "."))
      if(on.disk) {
        evaluated <- ff(dim = c(row.chunks[i], m), vmode = storage.mode, 
                                finalizer = ff.finalizer,
                                pattern = storage.path)
      } else {
        evaluated <- matrix(nrow = row.chunks[i], ncol = m)
      }
      if(!is.null(id.col)) { 
        rownames(evaluated) <- row.ids[row.from[i]:row.to[i]]
      }
      predict.from <- seq(row.from[i], row.to[i], predict.chunk)
      if(length(predict.from) > 1) {
        predict.to <- c(predict.from[2:length(predict.from)]-1, row.to[i])
      } else {
        predict.to <- row.to[i]
      }
      predict.chunks <- predict.to - c(row.from[i]-1, predict.to[-length(predict.to)])
      for(j in 1:length(predict.chunks)) {
        print(paste0(Sys.time(),
                     " Prediction chunk ", j, " of ", length(predict.chunks),
                     " (row chunk ", i, ")."))
        m.predict.chunk <- matrix(nrow = predict.to[j] - (predict.from[j] - 1),
                                  ncol = m)
        for(k in 1:length(post.from)) {
          lp <- matrix(rnorm(predict.chunks[j] * length(post.from[k]:post.to[k])),
                 nrow = predict.chunks[j],
                 ncol = length(post.from[k]:post.to[k]))
          m.predict.chunk[, post.from[k]:post.to[k]] <- lp
          rm(lp)
        }
        evaluated[(predict.from[j]:predict.to[j]) - (row.from[i] - 1), ] <- 
          m.predict.chunk
        rm(m.predict.chunk)
      }
      gc()
      return(evaluated)
    }
  if(on.disk) {
    return(fflist(evaluated.l, join_by = "row"))
    # return(evaluated.l)
  } else {
    return(evaluated.l)
  }
}


nodeslist <- unlist(strsplit(Sys.getenv("NODESLIST"), split=" "))
cl <- makeCluster(nodeslist, type = "PSOCK", outfile = "/home/schoed/scratch/foreach.log") 

cl <- makeCluster(2) 

ffl <- evaluate_posterior_par(n = 4000,
                         m = 1000,
                         row.chunk = 1000,
                         predict.chunk = 500,
                         post.chunk = NULL,
                         on.disk = TRUE,
                         cluster = cl,
                         storage.mode = "double",
                         storage.path = "/home/schoed/scratch/ff/epos",
                         ff.finalizer = "close"
                         ) 

ffl


dir.create("/home/schoed/scratch/ff")

fflist_delete(ffl)
rm(ffl)

stopCluster(cl)

        evaluated <- ff(dim = c(1000, 1000), vmode = storage.mode, 
                                finalizer = ff.finalizer,
                                pattern = storage.path)


fflist(list(evaluated))

storage.path = "/home/schoed/project/


evaluated

n = 4000
m = 1000
row.chunk = 1000
predict.chunk = 500
post.chunk = NULL
on.disk = TRUE
cluster = cl
storage.mode = "double"
storage.path = tempdir()
ff.finalizer = "close"

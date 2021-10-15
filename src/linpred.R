library(data.table)
library(mgcv)
library(ff)
library(posterior)
library(parallel)
library(doParallel)
library(foreach)

source("utilities.R")
options("ffbatchsize" = 1, "ffbatchbytes" = 2 * 2^30)

path.gam <- "../models/gam/"
path.data.proc <- "../data/processed/"
path.ff.tmp <- getOption("fftempdir")

## AMAZON ######################################################################

# Load model, posterior draws, and data
amz.gam <- readRDS(paste0(path.gam, "amz.m2.rds"))
amz.post <- readRDS(paste0(path.gam, "amz.m2.post.rds"))
amz.data <- readRDS(paste0(path.data.proc, "amz.data.proc.rds"))

# Data for predict function
amz.pred <- as.data.frame(amz.data[, 
                                   .(id, forestloss, it_type, pa_type,
                                   som_x, som_y, ed_east, ed_north, adm0)
                                   ])
amz.pred$b0 <- model.matrix(~ 1, amz.pred)

# set.seed(1234)
# sam <- sample(1:nrow(amz.pred), 5e4)

cl <- makeCluster(2, outfile = "../log/linpred.log")

system.time({
ffl <-
  evaluate_posterior_par(
                         model = amz.gam, 
                         posterior = amz.post, 
                         newdata = amz.pred[sample(1:1e6, 2000),],
                         id.col = "id", 
                         row.chunk = 500,
                         predict.chunk = 250,
                         # post.chunk = 200,
                         on.disk = TRUE, 
                         type = "link",
                         storage.path = NULL,
                         cluster = cl,
                         # n.cores = 2,
                         ff.finalizer = "close"
  )
})

stopCluster(cl)

ffl

fflist_save(ffl, "ffl")

fflist_delete(ffl)
rm(ffl)




fam <- fix.family.rd(amz.gam$family)
plot(density(fam$linkinv(colMeans(ffl[,]))))



cl <- makeCluster(2, outfile = "log.txt")
registerDoParallel(cl, cores = 2)
row.chunk = 500 
n <- nrow(amz.pred[1:1000,])
row.from <- seq(1, n, row.chunk)
row.to <- c(row.from[2:length(row.from)]-1, n)

system.time({
lpp.chunks <-
  foreach(i = 1:length(row.from), .packages = "ff") %dopar% {
    post_eval <-
      evaluate_posterior(amz.gam, amz.post, 
                         newdata = amz.pred[1:1000,][row.from[i]:row.to[i],],
                         id.col = "id", post.chunk = 200,
                         on.disk = TRUE, type = "link",
                         storage.path = ".",
                         ff.finalizer = "close")
    return(post_eval)
  }
})
lapply(lpp.chunks, fflist_delete)
rm(lpp.chunks)
stopCluster(cl)
gc()
# 133s


cl <- makeCluster(2, outfile = "log.txt")
registerDoParallel(cl, cores = 2)
row.chunk = 5000 
n <- nrow(amz.pred[1:10000,])
row.from <- seq(1, n, row.chunk)
row.to <- c(row.from[2:length(row.from)]-1, n)
system.time({
lpp.chunks <-
  foreach(i = 1:length(row.from), .packages = "ff") %dopar% {
    post_eval <-
      evaluate_posterior(amz.gam, amz.post, 
                         newdata = amz.pred[1:10000,][row.from[i]:row.to[i],],
                         id.col = "id", post.chunk = 200, row.chunk = 1000,
                         on.disk = TRUE, type = "link",
                         storage.path = ".",
                         ff.finalizer = "close")
    return(post_eval)
  }
})
lapply(lpp.chunks, fflist_delete)
rm(lpp.chunks)
stopCluster(cl)
gc()
#895s

cl <- makeCluster(1, outfile = "log.txt")
registerDoParallel(cl, cores = 1)
row.chunk = 5000 
n <- nrow(amz.pred[1:10000,])
row.from <- seq(1, n, row.chunk)
row.to <- c(row.from[2:length(row.from)]-1, n)
system.time({
lpp.chunks <-
  foreach(i = 1:length(row.from), .packages = "ff") %dopar% {
    post_eval <-
      evaluate_posterior(amz.gam, amz.post, 
                         newdata = amz.pred[1:10000,][row.from[i]:row.to[i],],
                         id.col = "id", post.chunk = 200,
                         on.disk = TRUE, type = "link",
                         storage.path = ".",
                         ff.finalizer = "close")
    return(post_eval)
  }
})
lapply(lpp.chunks, fflist_delete)
rm(lpp.chunks)
stopCluster(cl)
gc()

cl <- makeCluster(2, outfile = "log.txt")
registerDoParallel(cl, cores = 2)
row.chunk = 5000 
n <- nrow(amz.pred[1:10000,])
row.from <- seq(1, n, row.chunk)
row.to <- c(row.from[2:length(row.from)]-1, n)
system.time({
lpp.chunks <-
  foreach(i = 1:length(row.from), .packages = "ff") %dopar% {
    post_eval <-
      evaluate_posterior(amz.gam, amz.post, 
                         newdata = amz.pred[1:10000,][row.from[i]:row.to[i],],
                         id.col = "id", post.chunk = 200, row.chunk = 5000,
                         on.disk = TRUE, type = "link",
                         storage.path = ".",
                         ff.finalizer = "close")
    return(post_eval)
  }
})
lapply(lpp.chunks, fflist_delete)
rm(lpp.chunks)
stopCluster(cl)
gc()








1e7/4e3*400/3600

?foreach

A <- matrix(rnorm(400), 20, 20)
x <- matrix(rnorm(200), 10, 20)

foreach(A = iter(A, by = "row", chunksize = 7)) %dopar%
{m <- A %*% t(x)}

system.time({
      evaluate_posterior_par(amz.gam, amz.post, 
                         newdata = amz.pred[1:1000,],
                         id.col = "id", post.chunk = 200,
                         on.disk = TRUE, type = "link",
                         storage.path = ".",
                         cluster = cl,
                         cores = 4,
                         ff.finalizer = "close")
})

Xp <- predict(amz.gam, 
              amz.pred[1:10000,],
              type = "lpmatrix",
              block.size = 100,
              newdata.guaranteed = TRUE,
              cluster = cl)

?bam

lpp.chunks[[1]][[1]][1:10,1:10]

?registerDoParallel

lapply(lpp.chunks, fflist_delete)
rm(lpp.chunks)
stopCluster(cl)
gc()

t
?ff

dof <- function(row.chunk){
  n <- 100
  p <- 200
  ft <- ff(as.double(0), dim = c(n,p), vmode = "double")
  row.from <- seq(1, n, row.chunk)
  if(length(row.from) > 1) {
    row.to <- c(row.from[2:length(row.from)]-1, n)
  } else {
    row.to <- n
  }
  foreach(i = 1:length(row.from), .packages = "ff") %dopar% {
    vals <- rnorm(row.chunk*200)
    ft[row.from[i]:row.to[i],] <- vals
    return(TRUE)
  }
  return(ft)
}

s <- dof(10)
t <- dof(10)

i=1

?chunk

obs <- c(9:8e3, 9000:9008)
chunk.size <- 1000
chunks <- data.frame(row = obs,
                     chunk = cut(obs, ceiling(length(obs)/chunk.size), labels = FALSE))
chunk.ids <- unique(chunks$chunk)

foreach(chunks.ids)

amz.lpp.geo <-
  evaluate_posterior(amz.gam, amz.post, newdata = amz.pred, id.col = "id",
                     obs = 1:10, post.chunk = 200,
                     on.disk = FALSE, type = "link", cluster = NULL)


Sys.setenv("OPENBLAS_NUM_THREADS" = "1")

fflist_save(amz.lpp, paste0(path.gam, "amz.lpp"))



fflist_delete(amz.lpp)
rm(amz.lpp)

amz.lpp <- fflist_load(paste0(path.gam, "amz.lpp"))


fflist_colmeans(amz.lpp)

id.col <- NULL

if(!is.null(id.col) & id.col %in% names(amz.pred)) print("yes")
?rownames

?predict.bam


rownames(amz.lpp[[1]]) <- 1:10
str(amz.lpp)


# Obtain (partial) linear predictor based on marginal posterior for all observations
cam.lpp <-
  evaluate_posterior(cam.m2, cam.post, obs = 1:10, post.chunk = 200, on.disk = TRUE, type = "response")

fflist_save(cam.lpp, paste0(path.models, "cam.lpp"))
cam.lpp <- fflist_load(paste0(path.models, "cam.lpp"))

fitted(cam.m2)[1:10]

summary(cam.m2, re.test = FALSE)

fam <- fix.family.rd(cam.m2$family)
ffrowapply(colSums(x[i1:i2,,drop=FALSE]), X=x, RETURN=TRUE, CFUN="csum", BATCHSIZE=2)


sum(x[,200])


library(parallel)
cl <- makeCluster(4)
clusterExport(cl, c("ffrowapply", 

lplist <- lapply(cam.lpp,
          \(x) {
            ffrowapply(colSums(x[i1:i2,,drop=FALSE]), X=x, RETURN=TRUE, CFUN="cmean", BATCHSIZE=2)})

fsum <- do.call(c, lplist)

fam$linkinv(fsum)

?do.call

?cbind


fflist_delete(cam.lpp)
rm(cam.lpp)

fflist_mean

ffapply(cam.lpp
?csum

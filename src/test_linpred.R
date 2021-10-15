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

set.seed(1234)
sam <- sample(1:nrow(amz.pred), 1e4)

nodeslist <- unlist(strsplit(Sys.getenv("NODESLIST"), split=" "))
cl <- makeCluster(nodeslist, type = "PSOCK", outfile = "../log/linpred.log") 

a <- Sys.time()
ffl <-
  evaluate_posterior_par(
                         model = amz.gam, 
                         posterior = amz.post, 
                         newdata = amz.pred[sam,],
                         id.col = "id", 
                         row.chunk = 2500,
                         predict.chunk = 500,
                         # post.chunk = 200,
                         on.disk = TRUE, 
                         type = "link",
                         storage.path = path.ff.tmp,
                         cluster = cl,
                         ff.finalizer = "close"
  )
b <- Sys.time()
b-a

fflist_save(ffl, paste0(path.gam, "amz.lp.nc"), rootpath = path.ff.tmp)



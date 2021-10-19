args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(data.table)
library(posterior)
library(arrow)

source("utilities.R")

path.gam <- "../models/gam/"
path.data.proc <- "../data/processed/"
path.lp <- "/home/schoed/scratch/lp/"
# path.lp <- "../tmp/lp/"

task_id <- as.integer(args[1])
task_count <- as.integer(args[2])

## CENTRAL AMERICA #############################################################

# Load model, posterior draws, and data
cam.gam <- readRDS(paste0(path.gam, "cam.m2.rds"))
cam.post <- readRDS(paste0(path.gam, "cam.m2.post.rds"))
cam.data <- readRDS(paste0(path.data.proc, "cam.data.proc.rds"))

# Data for predict function
cam.pred <- as.data.frame(cam.data[, 
                                   .(id, forestloss, som_x, som_y, ed_east, ed_north, adm0)
                                   ])
cam.pred$b0 <- model.matrix(~ 1, cam.pred)

# Reduce data for test
cam.pred <- cam.pred[1:5e4,]

# Construct chunk overview
row.chunks <- chunk_seq(1, nrow(cam.pred), ceiling(nrow(cam.pred) / task_count))

print(paste0("Processing rows ", row.chunks$from[task_id],
             " to ", row.chunks$to[task_id],
             " (chunk ", task_id, " / ", task_count, ")"))

# Marginalize posterior over covariate effects
cam.post.mar <- cam.post
cam.post.mar[, grep("som", colnames(cam.post.mar))] <- 0
rm(cam.post)

# Evaluate posterior, calculate linear predictor
a <- Sys.time()
lpe.mar <-
  evaluate_posterior(model = cam.gam,
                     posterior = cam.post.mar,
                     newdata = cam.pred,
                     obs = row.chunks$from[task_id]:row.chunks$to[task_id],
                     id.col = "id",
                     predict.chunk = 1000,
                     post.chunk = 200,
                     type = "link",
                     progress = TRUE)
b <- Sys.time()
b-a

# Prepare export
lpe.mar.dt <- as.data.table(t(lpe.mar))
colnames(lpe.mar.dt) <- paste0("draw.", colnames(lpe.mar.dt))
lpe.mar.dt$id <- as.integer(colnames(lpe.mar))
setcolorder(lpe.mar.dt, "id")

# Export
if(!dir.exists(paste0(path.lp, "cam.lp.mar"))) {
  dir.create(paste0(path.lp, "cam.lp.mar"))
}
write_parquet(lpe.mar.dt, paste0(path.lp, "cam.lp.mar/cam.lp.mar-", task_id, ".parquet"))


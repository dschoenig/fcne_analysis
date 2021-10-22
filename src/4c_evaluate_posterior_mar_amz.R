args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(data.table)
library(posterior)
library(arrow)

source("utilities.R")

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.gam <- paste0(path.base, "models/gam/")
path.data.proc <- paste0(path.base, "data/processed/")
path.lp <- paste0(path.base, "models/gam/lp/")

task_id <- as.integer(args[1])
task_count <- as.integer(args[2])

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
amz.pred$P <- model.matrix(~ it_type * pa_type * adm0, amz.pred)

# # Reduce data for test
# amz.pred <- amz.pred[1:5e4,]

# Construct chunk overview
row.chunks <- chunk_seq(1, nrow(amz.pred), ceiling(nrow(amz.pred) / task_count))

print(paste0("Processing rows ", row.chunks$from[task_id],
             " to ", row.chunks$to[task_id],
             " (chunk ", task_id, " / ", task_count, ")"))

# Marginalize posterior over covariate effects
amz.post.mar <- amz.post
amz.post.mar[, grep("som", colnames(amz.post.mar))] <- 0
rm(amz.post)

# Evaluate posterior, calculate linear predictor
a <- Sys.time()
lpe.mar <-
  evaluate_posterior(model = amz.gam,
                     posterior = amz.post.mar,
                     newdata = amz.pred,
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
if(!dir.exists(paste0(path.lp, "amz.lp.mar"))) {
  dir.create(paste0(path.lp, "amz.lp.mar"))
}
write_parquet(lpe.mar.dt, paste0(path.lp, "amz.lp.mar/amz.lp.mar-", task_id, ".parquet"))


library(arrow)
library(mgcv)
library(data.table)


path.gam <- "../models/gam/"
path.data.proc <- "../data/processed/"

path.lp <- "../tmp/lp/amz.lp/"
gam <- readRDS(paste0(path.gam, "amz.m2.rds"))
data <- readRDS(paste0(path.data.proc, "amz.data.proc.rds"))
post <- readRDS(paste0(path.gam, "amz.m2.post.rds"))

post[, grep("som", colnames(post))] <- 0
lpm <- predict(gam, post, newdata = gam$model[3998,], newdata.guaranteed = TRUE, type = "lpmatrix")
pred <- lpm %*% t(post)

ds <- open_dataset(path.lp)
e.lp <- as.numeric(as.data.frame(ds[1998,2:1001]))

hist(pred - e.lp)
max(pred - e.lp)


path.gam <- "../models/gam/"
path.data.proc <- "../data/processed/"

path.lp <- "../tmp/lp/cam.lp/"
gam <- readRDS(paste0(path.gam, "cam.m2.rds"))
data <- readRDS(paste0(path.data.proc, "cam.data.proc.rds"))
post <- readRDS(paste0(path.gam, "cam.m2.post.rds"))

# post[, grep("som", colnames(post))] <- 0
lpm <- predict(gam, post, newdata = gam$model[3998,], newdata.guaranteed = TRUE, type = "lpmatrix")
pred <- lpm %*% t(post)

ds <- open_dataset(path.lp)
e.lp <- as.numeric(as.data.frame(ds[1998,2:1001]))

hist(pred - e.lp)
max(pred - e.lp)

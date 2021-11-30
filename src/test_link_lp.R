args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(data.table)
library(posterior)
library(arrow)

source("utilities.R")

# path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.gam <- paste0(path.base, "models/gam/")
path.data.proc <- paste0(path.base, "data/processed/")
path.lp <- paste0(path.base, "models/gam/test_link/lp/")

region <- tolower(as.character(args[1]))
task_id <- as.integer(args[2])
task_count <- as.integer(args[3])

file.gam <- paste0(path.gam, region, ".m3_l2.rds")
file.post <- paste0(path.gam, region,  ".m3_l2.post.rds")
file.data <- paste0(path.data.proc, region, ".data.proc.rds")

path.out.full <-  paste0(path.lp, region, ".lp/marginal=full/")
path.out.ten_loc <-  paste0(path.lp, region, ".lp/marginal=ten_loc/")

file.out.full <- paste0(path.out.full, region, ".lp-", task_id, ".arrow")
file.out.ten_loc <- paste0(path.out.ten_loc, region, ".lp-", task_id, ".arrow")


## EVALUATE LINEAR PREDICTOR BASED ON DRAWS FROM MODEL POSTERIOR ###############

# Load model, posterior draws, and data
gam <- readRDS(file.gam)
post <- readRDS(file.post)
data <- readRDS(file.data)


if(region == "cam") {
  it.adm.rec <- c("NIC", "PAN", "MEX", "CRI")
  it.adm.nor <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")

  pa.adm.ind <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")
  pa.adm.dir <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")

  itpa.adm.rec.ind <- c("NIC", "PAN", "MEX", "CRI")
  itpa.adm.rec.dir <- c("PAN", "MEX", "CRI")
  itpa.adm.nor.ind <- c("BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV")
  itpa.adm.nor.dir <- c("SLV", "BLZ", "PAN", "GTM", "HND", "CRI")

  data <-
    data[(it_type == "none" & pa_type == "none") |
              (it_type == "recognized" & pa_type == "none" & adm0 %in% it.adm.rec) |
              (it_type == "not_recognized" & pa_type == "none" & adm0 %in% it.adm.nor) |
              (pa_type == "indirect_use" & it_type == "none" & adm0 %in% pa.adm.ind) |
              (pa_type == "direct_use" & it_type == "none" & adm0 %in% pa.adm.dir) |
              (it_type == "recognized" & pa_type == "indirect_use" & adm0 %in% itpa.adm.rec.ind) |
              (it_type == "recognized" & pa_type == "direct_use" & adm0 %in% itpa.adm.rec.dir) |
              (it_type == "not_recognized" & pa_type == "indirect_use" & adm0 %in% itpa.adm.nor.ind) |
              (it_type == "not_recognized" & pa_type == "direct_use" & adm0 %in% itpa.adm.nor.dir)]
}

set.seed(1234)
sam <- sample(1:nrow(data), 1.5e5)
data <- data[sam,]

# Data for prediction
data.pred <- as.data.frame(data[, 
                                .(id, forestloss, it_type, pa_type, overlap,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                ])
data.pred$b0 <- model.matrix(~ 1, data.pred)
rm(data)

# Construct chunk overview
row.chunks <- chunk_seq(1, nrow(data.pred), ceiling(nrow(data.pred) / task_count))

# Subset data
data.pred <- data.pred[row.chunks$from[task_id]:row.chunks$to[task_id],]
silence <- gc()

# Marginalize posterior over covariate effects
b.names <- names(coef(gam))
b.full <- 1:length(b.names)
b.cov <- grep("s(som_x,som_y)", b.names, fixed = TRUE)
post.marginals <- list(full = b.full,
                       ten_loc = b.full[!b.full %in% b.cov])


cat("Evaluating the linear predictor for model ", region, ".t1, ",
     "using draws from the posterior distribution.\n", sep = "")
cat("Processing rows ", row.chunks$from[task_id],
    " to ", row.chunks$to[task_id],
    " (chunk ", task_id, " / ", task_count, "):\n",
    sep = "")

# Evaluate posterior, calculate linear predictor
a <- Sys.time()
lp <-
  evaluate_posterior(model = gam,
                     posterior = post,
                     newdata = data.pred,
                     id.col = "id",
                     marginals = post.marginals,
                     predict.chunk = 500,
                     post.chunk = 200,
                     type = "link",
                     progress = TRUE)
b <- Sys.time()
b-a

# Prepare export
lp.dt <-
  lapply(lp,
         FUN = \(x) {y <- as.data.table(t(x))
                     y$id <- colnames(x)
                     return(y)}) |>
  rbindlist(idcol = "marginal") |>
  # setcolorder(c("id", "marginal")) |>
  melt(id.vars = c("id", "marginal"),
       variable.name = "draw",
       value.name = "eta")

# Export

if(!dir.exists(path.out.full)){
  dir.create(path.out.full, recursive = TRUE)
}
if(!dir.exists(path.out.ten_loc)){
  dir.create(path.out.ten_loc, recursive = TRUE)
}

write_feather(lp.dt[marginal == "full"], file.out.full)
write_feather(lp.dt[marginal == "ten_loc"], file.out.ten_loc)

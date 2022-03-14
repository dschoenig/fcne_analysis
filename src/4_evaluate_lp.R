args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(data.table)
library(posterior)
library(stringi)
library(arrow)

source("utilities.R")

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.gam <- paste0(path.base, "models/gam/")
path.data.proc <- paste0(path.base, "data/processed/")
path.lp <- paste0(path.base, "models/gam/lp/")

region <- tolower(as.character(args[1]))
task_id <- as.integer(args[2])
task_count <- as.integer(args[3])

# region <- "amz"
# task_id <- 1
# task_count <- 200

task_count <- 200
# AMAZON refit because of timeout
chunks.refit <-
  c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
  27, 28, 29, 30, 31, 32, 33, 34, 35, 40, 41, 60,
  78, 79, 80, 81, 82, 83, 84, 85, 86, 114, 115, 127,
  128, 135, 156, 157, 158, 159, 161, 162, 163, 175,
  176, 177, 178, 179, 180, 181, 182)
task_id <- chunks.refit[task_id]


file.gam <- paste0(path.gam, region, ".m3.rds")
file.post <- paste0(path.gam, region,  ".m3.post.rds")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")

## EVALUATE LINEAR PREDICTOR BASED ON DRAWS FROM MODEL POSTERIOR ###############

# Load model, posterior draws, and data
gam <- readRDS(file.gam)
post <- readRDS(file.post)
data <- readRDS(file.data)

# Data for prediction
data.pred <- data[,.(id, forestloss, it_type, pa_type, overlap,
                     som_x, som_y, ed_east, ed_north, adm0)]
rm(data)

# Construct chunk overview
row.chunks <- chunk_seq(1, nrow(data.pred), ceiling(nrow(data.pred) / task_count))

# Subset data
data.pred <- data.pred[row.chunks$from[task_id]:row.chunks$to[task_id],]
silence <- gc()

# Marginalize posterior over covariate effects

# Lookup table for marginals

smooth.lu <- lookup_smooths(gam)

para.cov <- 
  smooth.lu[grepl("s(som_x,som_y)", label, fixed = TRUE),
            unlist(para)]
para.it <- 
  smooth.lu[grepl("it_type", label, fixed = TRUE) |
            grepl("overlap", label, fixed = TRUE),
            unlist(para)]
para.pa <- 
  smooth.lu[grepl("pa_type", label, fixed = TRUE) |
            grepl("overlap", label, fixed = TRUE),
            unlist(para)]
para.ov <- 
  smooth.lu[grepl("it_type", label, fixed = TRUE) |
            grepl("pa_type", label, fixed = TRUE) |
            grepl("overlap", label, fixed = TRUE),
            unlist(para)]
para.cov_it <- 
  smooth.lu[
            grepl("s(som_x,som_y)", label, fixed = TRUE) |
            grepl("it_type", label, fixed = TRUE) |
            grepl("overlap", label, fixed = TRUE),
            unlist(para)]
para.cov_pa <- 
  smooth.lu[
            grepl("s(som_x,som_y)", label, fixed = TRUE) |
            grepl("pa_type", label, fixed = TRUE) |
            grepl("overlap", label, fixed = TRUE),
            unlist(para)]
para.cov_ov <- 
  smooth.lu[
            grepl("s(som_x,som_y)", label, fixed = TRUE) |
            grepl("it_type", label, fixed = TRUE) |
            grepl("pa_type", label, fixed = TRUE) |
            grepl("overlap", label, fixed = TRUE),
            unlist(para)]


# Define marginals

b.full <- 1:length(coef(gam))
post.marginals <- list(full = b.full,
                       cov0 = b.full[-para.cov],
                       it0 = b.full[-para.it],
                       pa0 = b.full[-para.pa],
                       ov0 = b.full[-para.ov],
                       cov0_it0 = b.full[-para.cov_it],
                       cov0_pa0 = b.full[-para.cov_pa],
                       cov0_ov0 = b.full[-para.cov_ov])

marginal.ids <- list(full = data.pred[,id],
                     cov0 = data.pred[,id],
                     it0 = data.pred[it_type != "none", id],
                     pa0 = data.pred[pa_type != "none", id],
                     ov0 = data.pred[overlap != "none", id],
                     cov0_it0 = data.pred[it_type != "none", id],
                     cov0_pa0 = data.pred[pa_type != "none", id],
                     cov0_ov0 = data.pred[overlap != "none", id])

message(paste0("Evaluating the linear predictor for model ", region, ".m3, ",
        "using draws from the posterior distribution.\n"))
message(paste0("Processing rows ", row.chunks$from[task_id],
        " to ", row.chunks$to[task_id],
        " (chunk ", task_id, " / ", task_count, "):\n"))


# Evaluate posterior, calculate linear predictor
a <- Sys.time()
lp <-
  evaluate_posterior(model = gam,
                     posterior = post,
                     newdata = data.pred,
                     id.col = "id",
                     marginals = post.marginals,
                     # predict.chunk = 1000,
                     predict.chunk = 500,
                     post.chunk = 200,
                     type = "link",
                     marginal.ids = marginal.ids,
                     progress = TRUE)
b <- Sys.time()
b-a

# Prepare export
lp.dt <-
  lapply(lp,
         FUN = \(x) {y <- as.data.table(t(x))
                     y$id <- colnames(x)
                     return(y)}) |>
  rbindlist(idcol = "partial") |>
  # setcolorder(c("id", "marginal")) |>
  melt(id.vars = c("id", "partial"),
       variable.name = "draw",
       value.name = "eta")

# Export

paths.partial <- paste0(path.lp, region, ".lp/partial=", names(post.marginals), "/")

for(i in seq_along(paths.partial)) {
  if(!dir.exists(paths.partial[i])) {
    dir.create(paths.partial[i], recursive = TRUE)
  }
}

for(i in seq_along(paths.partial)) {
  name.partial <- names(post.marginals)[i]
  file.out <- paste0(paths.partial[i], region, ".lp-",
                     stri_pad_left(task_id, 3, 0) , ".arrow")
  message(paste0("Writing output for (partial) linear predictor `", name.partial,
                 "` to `", file.out, "` …"))
  write_feather(lp.dt[partial == name.partial, .(id, draw, eta)],
                file.out)
}

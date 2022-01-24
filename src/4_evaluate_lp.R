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

region <- tolower(as.character(args[1]))
task_id <- as.integer(args[2])
task_count <- as.integer(args[3])

file.gam <- paste0(path.gam, region, ".m3.rds")
file.post <- paste0(path.gam, region,  ".m3.post.rds")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")

## EVALUATE LINEAR PREDICTOR BASED ON DRAWS FROM MODEL POSTERIOR ###############

# Load model, posterior draws, and data
gam <- readRDS(file.gam)
post <- readRDS(file.post)
data <- readRDS(file.data)

# Recalculate failed chunks
task_id.change <- c(136, 148, 150, 152, 190, 191)
task_id <- task_id.change[task_id]

# Data for prediction
data.pred <- as.data.frame(data[, 
                                .(id, forestloss, it_type, pa_type, overlap,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                ])
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
                     predict.chunk = 500,
                     # predict.chunk = 50000,
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

paths.marginals <- paste0(path.lp, region, ".lp/marginal=", names(post.marginals), "/")

for(i in seq_along(paths.marginals)) {
  if(!dir.exists(paths.marginals[i])) {
    dir.create(paths.marginals[i], recursive = TRUE)
  }
}

for(i in seq_along(paths.marginals)) {
  name.marginal <- names(post.marginals)[i]
  file.out <- paste0(paths.marginals[i], region, ".lp-", task_id, ".arrow")
  message(paste0("Writing output for marginal `", name.marginal,
                 "` to `", file.out, "` …"))
  write_feather(lp.dt[marginal == name.marginal, .(id, draw, eta)],
                file.out)
}

args <- commandArgs(trailingOnly = TRUE)
library(mgcv)
library(data.table)
library(posterior)
library(stringi)
library(arrow)

source("utilities.R")

path.base <- "../"
path.gam <- paste0(path.base, "models/gam/")
path.data.proc <- paste0(path.base, "data/processed/")
path.pred <- paste0(path.base, "models/gam/pred/")

region <- tolower(as.character(args[1]))
task_id <- as.integer(args[2])
task_count <- as.integer(args[3])

# region <- "amz"
# task_id <- 1
# task_count <- 100

file.gam <- paste0(path.gam, region, ".m7.rds")
file.post <- paste0(path.gam, region,  ".m7.post.rds")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")

path.out <- paste0(path.pred, region, "/")
if(!dir.exists(path.out))
  dir.create(path.out, recursive = TRUE)
file.out <-
  paste0(path.out, region, ".", stri_pad_left(task_id, 3, 0) , ".arrow")

## EVALUATE LINEAR PREDICTOR BASED ON DRAWS FROM MODEL POSTERIOR ###############

# Load model, posterior draws, and data
gam <- readRDS(file.gam)
post <- readRDS(file.post)
data <- readRDS(file.data)

# Data for prediction
data.pred <- data[,.(id, forestloss, for_type, it_type, pa_type, overlap,
                     som_x, som_y, ed_east, ed_north, adm0)]
rm(data)

# Construct chunk overview
row.chunks <- chunk_seq(1, nrow(data.pred), ceiling(nrow(data.pred) / task_count))

# Recalculate missing parts
# change_task_id <- c(44, 45, 54)
# task_id <- change_task_id[task_id]
# task_count <- 200
# row.chunks <- chunk_seq(1, nrow(data.pred), ceiling(nrow(data.pred) / task_count))


# Subset data
data.pred <- data.pred[row.chunks$from[task_id]:row.chunks$to[task_id],]
silence <- gc()


message(paste0("Generating predictions for model ", region, ".m7, ",
        "using draws from the posterior distribution.\n"))
message(paste0("Processing rows ", row.chunks$from[task_id],
        " to ", row.chunks$to[task_id],
        " (chunk ", task_id, " / ", task_count, "):\n"))


# Evaluate posterior, calculate linear predictor
a <- Sys.time()

pred <-
  egp_posterior_predict(model = gam,
                        posterior = post,
                        data = data.pred,
                        id.var = "id",
                        type = "response",
                        epred = FALSE,
                        pred.name = "forestloss",
                        predict.chunk = 500,
                        post.chunk = 200,
                        progress = TRUE
                        )

b <- Sys.time()
b-a

silence <- gc()


# Prepare export

pred[, forestloss := as.logical(forestloss)]
pred[, .draw.chunk := factor(ceiling(.draw/100), levels = as.character(1:10))]
setcolorder(pred, c(".draw.chunk", ".draw", "id", "forestloss"))

setorder(pred, .draw.chunk, .draw, id)

message(paste0("Writing output to `", file.out, "` …"))
write_feather(pred, file.out, version = 2, chunk_size = 1e7, compression = "uncompressed")

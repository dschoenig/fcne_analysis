args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(arrow)
library(DHARMa)

source("utilities.R")

region <- tolower(as.character(args[1]))
# region <- "cam"

path.base <- "../"
path.data.proc <- paste0(path.base, "data/processed/")
path.mod <- paste0(path.base, "models/gam/")
path.pred <- paste0(path.mod, "pred/")
path.arrow <- paste0(path.pred, region)
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
files.pred <- list.files(path.arrow, ".arrow", full.names = TRUE)

file.res <- paste0(path.mod, region, ".m7.res.rds")

data <- readRDS(file.data)
setkey(data, id)

set.seed(123)
seeds <- sample(1:1e8, length(files.pred))

res.l <- list()
for(i in seq_along(files.pred)) {

  message(paste0("Processing file '", files.pred[i], "' (", i, "/", length(files.pred), ") …"))

  pred <- read_feather(files.pred[i], col_select = c(".draw", "id", "forestloss"))
  pred[, forestloss := as.numeric(forestloss)]
  pred.t <- dcast(pred, id ~ .draw, value.var = "forestloss")

  pred.fit <-
    pred[,
         .(fitted = mean(forestloss)),
         by = "id"
         ][.(pred.t$id),
           on = "id"]
  data.obs <-
    data[.(pred.t$id), .(id, observed = as.numeric(forestloss)), on = "id"]

  pred.t <-
    merge(pred.t, pred.fit, by = "id") |>
    merge(data.obs, by = "id")

  pred.mat <- as.matrix(pred.t[, -c("id", "fitted", "observed")])

  dim(pred.mat)

  qres <-
    createDHARMa(simulatedResponse = pred.mat,
                 observedResponse = pred.t$observed,
                 fittedPredictedResponse = pred.t$fitted,
                 integerResponse = TRUE,
                 seed = seeds[i],
                 method = "PIT") |>
    residuals()

  res.l[[i]] <-
    data.table(id = pred.t$id,
               fitted = pred.t$fitted,
               residual = qres)

  rm(pred, pred.t, pred.fit, data.obs, pred.mat, qres)

}

res <- rbindlist(res.l)

message("Saving output …")

saveRDS(res, file.res)




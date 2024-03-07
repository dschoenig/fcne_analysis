args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(arrow)
library(dplyr)

source("utilities.R")

n.threads <- as.integer(args[1])
cf_type <- tolower(as.character(args[2]))
region <- tolower(as.character(args[3]))
for_type <- tolower(as.character(args[4]))
area_type <- tolower(as.character(args[5]))
ov_type <- tolower(as.character(args[6]))
hurr_type <- tolower(as.character(args[7]))

# n.threads <- 4
# cf_type <- "geo"
# region <- "amz"
# for_type <- "af"
# area_type <- "pa"
# ov_type <- "na"
# hurr_type <- NA

if(cf_type == "geo") {
  ov_suf <- ""
} else {
  ov_suf <- paste0(".", ov_type)
}

setDTthreads(n.threads)

if(is.na(hurr_type)) {
  hurr_type <- "otto"
}
if(hurr_type == "no_otto" & region == "cam") {
  hurr_suf <- ".no_otto"
} else {
  hurr_suf <- ""
}

paste0("Settings: ", paste(cf_type, for_type, area_type, ov_type, hurr_type, sep = ", ")) |>
message()

path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.pred <- paste0(path.base, "models/gam/pred/")
path.cf <- paste0(path.base, "models/cf/", region, "/")
path.mar <- paste0(path.base, "models/marginal/", region, "/")
if(!dir.exists(path.mar))
  dir.create(path.mar, recursive = TRUE)
file.data.areas <- paste0(path.data.proc, region, ".data.areas.rds")
file.cf <- paste0(path.cf,
                  region, ".",
                  cf_type, ".",
                  for_type, ".",
                  area_type, ov_suf, hurr_suf, ".rds")
file.mar <- paste0(path.mar,
                   region, ".",
                   cf_type, ".",
                   for_type, ".",
                   area_type, ov_suf, hurr_suf, ".rds")
path.arrow <- paste0(path.pred, region, "/")


cf <- readRDS(file.cf)
pred.ds <- open_dataset(path.arrow, format = "arrow")

if(cf_type == "ten.areas") {
  dict.idx <-
    readRDS(file.data.areas) |>
    _[comp == area_type, .(uid, id)]
  setkey(dict.idx, id)
}

draw.chunks <- chunk_seq(1, 1000, 25)
draw.chunks <- chunk_seq(1, 50, 25)
group.chunks <- chunk_seq(1, nrow(cf$groups), 2500)

eval.mar.l <- list()

for(i in seq_along(draw.chunks$from)) {

  paste0("Evaluating draws ", draw.chunks$from[i],
         " to ", draw.chunks$to[i],
         " …") |>
  message()

  message("  Load predictions …")

  a <- Sys.time()

  pred.draw <-
    pred.ds |>
    filter(.draw >= draw.chunks$from[i] & .draw <= draw.chunks$to[i]) |>
    select(.draw, id, forestloss) |>
    collect()

  if(cf_type == "ten.areas") {
    pred.draw <-
      merge(pred.draw, dict.idx, all.x = TRUE, all.y = FALSE)
    setkey(pred.draw, uid)
    pred.draw <- pred.draw[uid %in% cf$data$uid]
  } else {
    setkey(pred.draw, id)
    pred.draw <- pred.draw[id %in% cf$data$id]
  }


  b <- Sys.time()
  print(b-a)


  message("  Evaluate marginal …")

  a <- Sys.time()

  eval.fac <-
    egp_evaluate_factual(predictions = pred.draw,
                         cf.def = cf,
                         name = "factual",
                         pred.var = "forestloss",
                         draw.chunk = NULL,
                         agg.size = 1e6,
                         parallel = n.threads,
                         progress = TRUE)
  
  silence <- gc()

  eval.cf.l <- list()

  for(j in seq_along(group.chunks$from)) {

    if(length(group.chunks$from) > 1) {
      paste0("  Groups ", group.chunks$from[j],
         " to ", group.chunks$to[j],
         " …") |>
      message()
    }

    eval.cf.l[[j]] <-
      egp_evaluate_counterfactual(predictions = pred.draw,
                                  cf.def = cf,
                                  name = "counterfactual",
                                  group.eval = group.chunks$from[j]:group.chunks$to[j],
                                  pred.var = "forestloss",
                                  draw.chunk = NULL,
                                  agg.size = 1e6,
                                  parallel = n.threads,
                                  progress = TRUE)

  }

  eval.cf <- rbindlist(eval.cf.l)

  rm(eval.cf.l)
  gc()

  eval.mar.l[[i]] <-
    egp_marginal(factual = eval.fac,
                 counterfactual = eval.cf,
                 marginal.name = "marginal")

  rm(eval.fac, eval.cf)
  gc()

  b <- Sys.time()
  print(b-a)

}

eval.mar <- rbindlist(eval.mar.l)

saveRDS(eval.mar, file.mar)

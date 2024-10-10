args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(arrow)
library(dplyr)

source("utilities.R")

n.threads <- as.integer(args[1])
cf_type <- tolower(as.character(args[2]))
cf_pwid <- as.integer(args[3])
region <- tolower(as.character(args[4]))
resp_type <- tolower(as.character(args[5]))
area_type <- tolower(as.character(args[6]))
ov_type <- tolower(as.character(args[7]))
hurr_type <- tolower(as.character(args[8]))

draws.max <- 1000
draws.load.chunk <- 100
draws.eval.chunk <- 10

# n.threads <- 1
# cf_type <- "ten_pw_"
# cf_pwid <- 3
# region <- "amz"
# resp_type <- "def"
# area_type <- "itpa"
# ov_type <- "all"
# hurr_type <- NA


setDTthreads(n.threads)
set_cpu_count(n.threads)


if(cf_type == "geo") {
  ov_suf <- ""
} else {
  ov_suf <- paste0(".", ov_type)
}

if(is.na(hurr_type)) {
  hurr_type <- "hurr"
}
if(hurr_type == "no_hurr" & region == "cam") {
  hurr_suf <- ".no_hurr"
} else {
  hurr_suf <- ""
}

paste0("Settings: ", paste(resp_type, cf_type, cf_pwid, area_type, ov_type, hurr_type, sep = ", ")) |>
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
                  cf_type, cf_pwid, ".",
                  area_type, ov_suf, hurr_suf, ".rds")
file.mar <- paste0(path.mar,
                   region, ".",
                   resp_type, ".",
                   cf_type, cf_pwid, ".",
                   area_type, ov_suf, hurr_suf, ".rds")
path.arrow <- paste0(path.pred, region, "/", resp_type, "/")


cf <- readRDS(file.cf)
pred.ds <- open_dataset(path.arrow, format = "arrow")

if(cf_type == "ten.areas") {
  dict.idx <-
    readRDS(file.data.areas) |>
    _[comp == area_type, .(uid, id)]
  setkey(dict.idx, id)
}

draw.chunks.load <- chunk_seq(1, draws.max, draws.load.chunk)
if(cf_type == "geo") {
  group.chunks <- chunk_seq(1, nrow(cf$groups), 10000)
} else {
  group.chunks <- chunk_seq(1, nrow(cf$groups), 250)
}

resp.var <-
  switch(resp_type,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

select.var <- c(".draw", "id", resp.var)


eval.mar.i <- list()

for(i in seq_along(draw.chunks.load$from)) {

  a <- Sys.time()

  paste0("Loading predictions ", draw.chunks.load$from[i],
         " to ", draw.chunks.load$to[i],
         " …") |>
  message()

  pred.draw <-
    pred.ds |>
    filter(.draw >= draw.chunks.load$from[i] & .draw <= draw.chunks.load$to[i]) |>
    select(all_of(select.var)) |>
    collect()

  pred.draw[, resp.col := as.numeric(resp.col), env = list(resp.col = resp.var)]

  if(cf_type == "ten.areas") {
    pred.draw <-
      merge(pred.draw, dict.idx, all.x = TRUE, all.y = FALSE,
            allow.cartesian = TRUE)
    setkey(pred.draw, uid)
    pred.draw <- pred.draw[uid %in% cf$data$uid]
  } else {
    setkey(pred.draw, id)
    pred.draw <- pred.draw[id %in% cf$data$id]
  }

  draw.chunks.eval <-
    chunk_seq(draw.chunks.load$from[i],
              draw.chunks.load$to[i],
              draws.eval.chunk)

  eval.mar.j <- list()

  b <- Sys.time()
  print(b-a)

  for(j in seq_along(draw.chunks.eval$from)) {

    paste0("Evaluating draws ", draw.chunks.eval$from[j],
           " to ", draw.chunks.eval$to[j],
           " …") |>
    message()

    pred.draw.j <- pred.draw[.draw %in% draw.chunks.eval$from[j]:draw.chunks.eval$to[j]]

    message("  Evaluating marginals …")

    a <- Sys.time()

    eval.fac <-
      egp_evaluate_factual(predictions = pred.draw.j,
                           cf.def = cf,
                           name = "factual",
                           pred.var = resp.var,
                           draw.chunk = NULL,
                           agg.size = 1e6,
                           parallel = n.threads,
                           progress = TRUE)
    
    silence <- gc()

    eval.cf.l <- list()

    for(k in seq_along(group.chunks$from)) {

      if(length(group.chunks$from) > 1) {
        paste0("  Groups ", group.chunks$from[k],
           " to ", group.chunks$to[k],
           " …") |>
        message()
      }

      eval.cf.l[[k]] <-
        egp_evaluate_counterfactual(predictions = pred.draw.j,
                                    cf.def = cf,
                                    name = "counterfactual",
                                    group.eval = group.chunks$from[k]:group.chunks$to[k],
                                    pred.var = resp.var,
                                    draw.chunk = NULL,
                                    agg.size = 1e6,
                                    parallel = n.threads,
                                    progress = TRUE)

    }

    eval.cf <- rbindlist(eval.cf.l)

    rm(eval.cf.l)
    gc()

    eval.mar.j[[j]] <-
      egp_marginal(factual = eval.fac,
                   counterfactual = eval.cf,
                   marginal.name = "marginal")

    rm(eval.fac, eval.cf, pred.draw.j)
    # gc()

    b <- Sys.time()
    print(b-a)

  }

  eval.mar.i[[i]] <- rbindlist(eval.mar.j)

  rm(eval.mar.j, pred.draw)
  gc()

}

eval.mar <- rbindlist(eval.mar.i)

rm(eval.mar.i)
gc()

paste0("Saving marginal as ", file.mar, " …") |>
message()

saveRDS(eval.mar, file.mar)

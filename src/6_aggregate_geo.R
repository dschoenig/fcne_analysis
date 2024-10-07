args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(arrow)
library(dplyr)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
resp_type <- tolower(as.character(args[3]))
hurr_type <- tolower(as.character(args[4]))

# n.threads <- 4
# region <- "cam"
# hurr_type <- NA

draws.max <- 1000
draws.load.chunk <- 100
draws.eval.chunk <- 1

setDTthreads(n.threads)
set_cpu_count(n.threads)

if(is.na(hurr_type)) {
  hurr_type <- "hurr"
}
if(hurr_type == "no_hurr" & region == "cam") {
  hurr_suf <- ".no_hurr"
} else {
  hurr_suf <- ""
}

map.res <- switch(region,
                  amz = 1e4,
                  cam = 5e3)

paste0("Settings: ", paste(resp_type, map.res, hurr_type, sep = ", ")) |>
message()

path.base <- "../"
path.data.proc <- paste0(path.base, "data/processed/")
path.pred <- paste0(path.base, "models/gam/pred/")
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")
if(!dir.exists(path.agg))
  dir.create(path.agg, recursive = TRUE)

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
path.arrow <- paste0(path.pred, region, "/", resp_type, "/")

file.agg <- paste0(path.agg, region, ".", resp_type, ".geo", hurr_suf, ".rds")

id.var <- "id"
data <- readRDS(file.data)


if(region == "cam" & hurr_type == "no_hurr") {
  data <- data[hurr_lf == FALSE]
}


map.anchor <- c(ea_east = floor(min(data$ea_east / map.res)) * map.res,
                ea_north = floor(min(data$ea_north / map.res)) * map.res)

data <-
  bin_cols(data,
           columns = c("ea_east", "ea_north"), bin.res = rep(map.res, 2),
           bin.min = map.anchor, append = TRUE)

group.by <- list(c("ea_east.bin", "ea_north.bin"))


paste0("No. of data: ", nrow(data)) |>
message()

paste0("Aggregated predictions will be saved as ", file.agg) |>
message()

message("Aggregating predictions …")


idx.geo <-
  .ids_by_group(data[, .(id, ea_east.bin, ea_north.bin)],
                id.var = id.var,
                group.vars = c("ea_east.bin", "ea_north.bin"),
                add.label = FALSE)
idx.geo[, group.id := 1:.N]


pred.ds <- open_dataset(path.arrow, format = "arrow")

draw.chunks.load <- chunk_seq(1, draws.max, draws.load.chunk)

resp.var <-
  switch(resp_type,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

select.var <- c(".draw", id.var, resp.var)

eval.agg.l <- list()

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

  b <- Sys.time()
  print(b-a)

  eval.agg.l[[i]] <-
    .aggregate_variables.data.table(predictions = pred.draw,
                                    agg.fun = mean,
                                    ids = idx.geo$id,
                                    pred.var = resp.var,
                                    draw.var = ".draw",
                                    id.var = id.var,
                                    agg.name = resp.var,
                                    group.name = "group.id",
                                    draw.chunk = draws.eval.chunk,
                                    agg.size = 1e6,
                                    parallel = n.threads,
                                    progress = TRUE)

  b <- Sys.time()
  print(b-a)

  rm(pred.draw)
  gc()

}

eval.agg <- rbindlist(eval.agg.l)

agg <- merge(idx.geo[, -"id"], eval.agg)
setorder(agg, .draw, group.id)

print(agg)

paste0("Saving aggregated predictions as ", file.agg, " …") |>
message()

saveRDS(agg, file.agg)
gc()

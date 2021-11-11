a <- Sys.time()

# args <- commandArgs(trailingOnly = TRUE)

library(arrow)
library(dplyr)
library(stringi)
library(data.table)

# region <- tolower(args[1])
# n.threads <- as.integer(args[1])

region <- "amz"
marginals <- c("full", "ten_loc")
n.threads <- 1

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
suffix.lp <- "lp"

set_cpu_count(n.threads)

path.in <- paste0(path.lp, region, ".", suffix.lp, "/")
path.out <- paste0(path.lp, region, ".", suffix.lp, ".arrow/")
paths.marginals <- paste0(path.out, "marginal=", marginals, "/")
silent <- lapply(paths.marginals, \(x) if(!dir.exists(x)) dir.create(x, recursive = TRUE))

files.parquet <- list.files(path.in, pattern = "parquet")
files.parquet <- files.parquet[1:3]
files.arrow <- stri_replace_last_fixed(files.parquet, ".parquet", ".arrow")

cat(paste0("Converting: `", path.in, "` -> `", path.out, "` (",
           length(files.parquet), " files) …\n")) 

prog <- txtProgressBar(min = 0, max = length(files.parquet), initial = 0,
                         char = "=", width = NA, title = "Progress", style = 3)

for(i in 1:length(files.parquet)) {
  # print(i)
  lp.pq <- read_parquet(paste0(path.in, files.parquet[i]), as_data_frame = FALSE)
  for(m in 1:length(marginals)) {
    # print(i)
    lp.out <-
      collect(select(filter(lp.pq, marginal == marginals[m]), id, draw, eta))
    lp.out$id <- as.integer(lp.out$id)
    file.tmp <- tempfile(fileext = "arrow")
    # print(i)
    write_feather(lp.out,
                   # paste0(path.out, files.arrow[i]),
                   file.tmp,
                   chunk_size = 50000L , version = 2)
    # print(i)
    file.copy(file.tmp, paste0(paths.marginals[m], files.arrow[i]))
    file.remove(file.tmp)
    rm(lp.out)
    # print("done")
  }
  rm(lp.pq)
  setTxtProgressBar(prog, i)
}

close(prog)

b <- Sys.time()

b-a

# # Performance
# ds <- open_dataset(paste0(path.lp, region, ".", suffix.lp, "/"), format = "parquet")
# ds_arc <- open_dataset(paste0(path.lp, region, ".", suffix.lp, ".arrow/"), format = "arrow")
# ds_arc <- open_dataset(paste0(path.lp, region, ".", suffix.lp, ".arrow/marginal=ten_loc"),
#                        format = "arrow")
# ds_aru <- open_dataset(paste0(path.lp, region, ".", suffix.lp, ".arrow2/"), format = "arrow")

# sam <- sample(1:1.5e5, 100)
# sam <- 1:1000

# system.time({
# res <-
#   ds |>
#   filter(marginal == "ten_loc", draw %in% 1:250, id %in% sam) |>
#   select(id, draw, eta) |>
#   collect()
# })

# system.time({
# res_arc <-
#   ds_arc |>
#   filter(marginal == "ten_loc", id %in% sam) |>
#   # filter(draw %in% 1:25, id %in% sam) |>
#   select(id, draw, eta) |>
#   collect()
# })

# collect(res_arc)

# filter(res_arc, id %in% 1:10)

# # system.time({
# res_aru <-
#   ds_aru |>
#   filter(draw %in% 1:250, id %in% sam) |>
#   select(id, draw, eta) |>
#   collect()
# })

# head(res)

# sam[1:5]

# all(res$id == res_arc$id)
# res_aru$id

a <- Sys.time()

args <- commandArgs(trailingOnly = TRUE)

library(arrow)
library(dplyr)
library(stringi)
library(data.table)

region <- tolower(args[1])
n.threads <- as.integer(args[2])

set_cpu_count(n.threads)
marginals <- c("full", "ten_loc")

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
suffix.lp <- "lp"
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
  lp.pq <- read_parquet(paste0(path.in, files.parquet[i]), as_data_frame = FALSE)
  for(m in 1:length(marginals)) {
    lp.out <-
      collect(select(filter(lp.pq, marginal == marginals[m]), id, draw, eta))
    lp.out$id <- as.integer(lp.out$id)
    file.tmp <- tempfile(fileext = "arrow")
    write_feather(lp.out,
                   file.tmp,
                   chunk_size = 50000L , version = 2)
    file.copy(file.tmp, paste0(paths.marginals[m], files.arrow[i]))
    file.remove(file.tmp)
    rm(lp.out)
  }
  rm(lp.pq)
  setTxtProgressBar(prog, i)
}

close(prog)

b <- Sys.time()

b-a

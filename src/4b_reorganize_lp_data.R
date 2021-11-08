args <- commandArgs(trailingOnly = TRUE)

library(arrow)
library(dplyr)

source("utilities.R")

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")

n.threads <- as.integer(args[1])
# n.threads <- 1
# set_cpu_count(n.threads)

# regions <- c("amz", "cam")
regions <- c("amz")


for(i in 1:length(regions)) {
  path.in <- paste0(path.lp, regions[i], ".lp/")
  path.out <- paste0(path.lp, regions[i], ".lp_ro/")
  # ds <- open_dataset(path.in)
  ds <- open_dataset(paste0(path.in, regions[i], ".lp-", 1:5, ".parquet"))
  write_dataset(group_by(ds, marginal, draw), path.out,
                format = "parquet", version = 2, chunk_size = 5e4,
                # , partitioning = c("marginal")
                , hive_style = TRUE)
}

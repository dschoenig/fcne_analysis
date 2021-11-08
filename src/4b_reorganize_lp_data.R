args <- commandArgs(trailingOnly = TRUE)

library(arrow)

source("utilities.R")

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")

n.threads <- as.integer(args[1])

# regions <- c("amz", "cam")
regions <- c("amz")

for(i in 1:length(regions)) {
  path.in <- paste0(path.lp, regions[i], ".lp_old/")
  path.out <- paste0(path.lp, regions[i], ".lp/")
  ds <- open_dataset(path.in)
  write_dataset(ds, path.out, format = "parquet", version = "2.0",
                partitioning = c("marginal", "draw"), hive_style = TRUE)
}


args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))

# n.threads <- 4
# region <- "amz"

setDTthreads(n.threads)

path.base <- "../"
path.cf <- paste0(path.base, "models/cf/", region, "/")
path.mar <- paste0(path.base, "models/marginal/", region, "/")
if(!dir.exists(path.mar))
  dir.create(path.mar, recursive = TRUE)

cf.files <- list.files(path.cf)
file.sam <- paste0(path.mar, region, ".sam.rds")

n.sam <- list()

for(i in seq_along(cf.files)) {
  cf.load <- paste(path.cf, cf.files[i], sep = "/")
  cf <- readRDS(cf.load)
  cf.cols <- names(cf$groups)
  col.fac <- cf.cols[cf.cols %like% "id.factual"]
  n.sam[[i]] <-
    cf$groups[,
              .(group.id,
                n = unlist(lapply(fac, length))),
              env = list(fac = col.fac)]
}

names(n.sam) <- cf.files

saveRDS(n.sam, file.sam)


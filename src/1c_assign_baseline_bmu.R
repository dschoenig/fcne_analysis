args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.som <- "../models/som/"
path.data <- "../data/"
path.data.int <- paste0(path.data, "intermediate/")

path.data.proc <- paste0(path.data, "processed/")
if(!dir.exists(path.data.proc)){
  dir.create(path.data, recursive = TRUE)
}

region <- tolower(as.character(args[1]))
n.cores <- as.integer(args[2])
region <- "cam"
n.cores <- 4

file.data.proc <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")

min.obs <- 100

## Map covariates to SOM #######################################################

som.fit <- readRDS(file.som)

data.proc <- readRDS(file.data.proc)

# DETERMINE BASELINE BMUs

bl.bmu <- data.proc[it_type == "none" & pa_type == "none", .(n = .N), som_bmu]

# If BMU contains a sufficient number of baseline observations, it is set to
# be the baseline BMU.
data.bmu.bl <-
  copy(data.proc)[som_bmu %in% bl.bmu[n >= min.obs, som_bmu]][,
                  som_bmu.bl := as.list(som_bmu)]

# Observations to which multiple baseline BMUs have to be assigned (due to a
# lack of baseline observations)

data.bmu.bl.mult <-
  data.proc[!som_bmu %in% bl.bmu[n >= min.obs, som_bmu] &
           (pa_type != "none" | it_type != "none")]

data.bmu.bl.mult$som_bmu.bl.mult <-
  scale_data_som(data.bmu.bl.mult[, .(tri, dist_set, dist_roads,
                                      dist_rivers, dens_pop, dens_roads)],
                 som.fit) |>
  bmu_match_reference(som.fit, bl.bmu[, .(som_bmu, n)], min.obs, 4)

data.bmu <-
  merge(data.proc, data.bmu.bl[, .(id, som_bmu.bl)], all = TRUE, sort = FALSE) |>
  merge(data.bmu.bl.mult[, .(id, som_bmu.bl.mult)], all = TRUE, sort = FALSE)


data.bmu[, som_bmu.bl := fifelse(unlist(lapply(som_bmu.bl, is.null)),
                                  som_bmu.bl.mult, som_bmu.bl)]

sum(data.bmu$id != data.proc$id)

saveRDS(data.bmu[, !"som_bmu.bl.mult"], file.data.proc)



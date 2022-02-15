library(data.table)
library(posterior)
library(parallel)

source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

region <- "cam"
n.threads <- 4

path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.effects <- paste0(path.base, "models/gam/effects/")

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.risk.tenure_cov <- paste0(path.effects, region, ".risk.tenure_cov.rds")
file.risk.tenure_areas <- paste0(path.effects, region, ".risk.tenure_areas.rds")
file.riskchange <- paste0(path.effects, region, ".riskchange.tenure_areas.rds")

setDTthreads(n.threads)

## EVALUATE RISK CHANGE ########################################################

data.proc <- readRDS(file.data)

# Posterior for SOM units, baseline observations
r.ten_cov <- readRDS(file.risk.tenure_cov)
post.units <- r.ten_cov$r$baseline

rc.ten <- list()
r.ten_areas <- readRDS(file.risk.tenure_areas)
id.list <- r.ten_areas$areas.it_pa$ids
names(id.list) <- r.ten_areas$areas.it_pa$area.label
ids.units <- data.proc[, .(id, som_bmu.bl, som_bmu.bl.w)
                       ][, lapply(.SD, unlist), id]
# Reweigh baseline SOM units for each group, based on what points they where
# assigned to
w.points <-
  lapply(id.list,
         \(x) {
           extract_weights(ids.units[id %in% x],
                           w.col = "som_bmu.bl.w",
                           by.col = "som_bmu.bl",
                           standardize = TRUE)
         })
r.bl.ten <- reweigh_posterior(post.units, w = w.points)
rc.ten$arc <- arc(r.ten_areas$r, r.bl.ten)
rc.ten$rrc <- rrc(r.ten_areas$r, r.bl.ten)
rc.ten$groups <- r.ten_areas$areas.it_pa

message(paste0("Saving outputs to `", file.riskchange, "` …"))
saveRDS(rc.ten, file.riskchange)

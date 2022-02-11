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
file.risk.tenure <- paste0(path.effects, region, ".risk.tenure.rds")
file.riskchange <- paste0(path.effects, region, ".riskchange.tenure.rds")

setDTthreads(n.threads)

## EVALUATE RISK CHANGE ########################################################

data.proc <- readRDS(file.data)

# Posterior for SOM units, baseline observations
r.ten_cov <- readRDS(file.risk.tenure_cov)
post.units <- r.ten_cov$r$baseline

# Initial weights based on no. of observations for each BMU
n.units <- data.proc[it_type == "none" & pa_type == "none",
                     .(n = .N), som_bmu
                     ][order(som_bmu)]
w.units <- n.units$n
names(w.units) <- as.character(n.units$som_bmu)

rc.ten <- list()
r.ten <- readRDS(file.risk.tenure)
id.list <- r.ten$groups$ids
names(id.list) <- r.ten$groups$group.label
ids.units <- data.proc[, .(id, som_bmu.bl)]
# Reweigh SOM units based on point observations in groups
w.points <-
  lapply(id.list,
         \(x) {ids.units[id %in% x,
                         extract_weights(as.character(unlist(som_bmu.bl)))]
         })
r.bl.ten <- reweigh_posterior(post.units, w.units, w.points)
rc.ten$arc <- arc(r.ten$full, r.bl.ten)
rc.ten$rrc <- rrc(r.ten$full, r.bl.ten)
rc.ten$groups <- r.ten$groups

message(paste0("Saving outputs to `", file.riskchange, "` …"))
saveRDS(rc.ten, file.riskchange)

library(data.table)
library(posterior)
library(parallel)
# library(sf)
# library(ggplot2)
# library(ggdist)
# library(colorspace)

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
prefix.file.risk.geo <- paste0(region, ".risk.geo.")
file.riskchange <- paste0(path.effects, region, ".riskchange.geo.rds")

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


# cl <- makeForkCluster(4)
maps <- c("all", "it_c", "pa_c", "it", "pa", "ov")
rc.geo <- list()
for(i in seq_along(maps)){
  message(paste0("Calculating risk change for `", maps[i], "` …"))
  rc.map <- list()
  file.risk.geo <- paste0(path.effects, prefix.file.risk.geo, maps[i], ".rds")
  r.geo <- readRDS(file.risk.geo)
  id.list <- r.geo$map.units$ids
  names(id.list) <- r.geo$map.units$group.label
  ids.units <- data.proc[, .(id, som_bmu.bl)]
  # Reweigh SOM units based on point observations in groups
  w.points <-
    lapply(id.list,
           \(x) {ids.units[id %in% x,
                           extract_weights(as.character(unlist(som_bmu.bl)))]
           })
  r.bl.geo <- reweigh_posterior(post.units, w.units, w.points)
  rc.map$arc <- arc(r.geo$full, r.bl.geo)
  rc.map$rrc <- rrc(r.geo$full, r.bl.geo)
  rc.map$map.units <- r.geo$map.units
  rc.geo[[i]] <- rc.map
  rm(rc.map)
}

names(rc.geo) <- maps

message(paste0("Saving outputs to `", file.riskchange, "` …"))
saveRDS(rc.geo, file.riskchange)

library(data.table)
library(posterior)
# library(parallel)

source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

# region <- "amz"
# n.threads <- 4

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.effects <- paste0(path.base, "models/gam/effects/")

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.risk.tenure_cov <- paste0(path.effects, region, ".risk.tenure_cov.rds")
# file.risk.tenure <- paste0(path.effects, region, ".risk.tenure.rds")
file.riskchange <- paste0(path.effects, region, ".riskchange.tenure_cov.rds")

setDTthreads(n.threads)

## EVALUATE RISK CHANGE ########################################################

data.proc <- readRDS(file.data)

# Posterior for SOM units, baseline observations
r.ten_cov <- readRDS(file.risk.tenure_cov)
post.units <- r.ten_cov$r$baseline


ten_cat <- 
  data.table(
    name = c(
             "for_type.all",
             "for_type.primary",
             "it_type.recognized", 
             "it_type.not_recognized",
             "pa_type.indirect_use",
             "pa_type.direct_use"),
    subset = c('cat == "all" & is.na(for_type) & is.na(it_type) & is.na(pa_type)',
               'cat == "for_p" & for_type == "primary" & is.na(it_type) & is.na(pa_type)',
               'cat == "it" & it_type == "recognized" & is.na(pa_type)',
               'cat == "it" & it_type == "not_recognized" & is.na(pa_type)',
               'cat == "pa" & is.na(it_type) & pa_type == "indirect_use"',
               'cat == "pa" & is.na(it_type) & pa_type == "direct_use"')
    )
# for(i in 1:nrow(ten_cat)) {
#   units.sub <- r.ten_cov$som.units[eval(parse(text = ten_cat$subset[i]))]
#   print(units.sub)
#   print(nrow(units.sub))
# }

rc.ten_cov <- list()

for(i in 1:nrow(ten_cat)){
  message(paste0("Calculating risk change for tenure category `",
                 ten_cat$name[i], "` …"))
  rc.ten_cat <- list()
  units.sub <- r.ten_cov$som.units[eval(parse(text = ten_cat$subset[i]))]
  id.list <- units.sub$ids
  names(id.list) <- units.sub$group.label

  ids.units <- data.proc[,
                         .(id,
                           for_type,
                           som_bmu.bl,
                           som_bmu.bl.w)
                         ][,
                           lapply(.SD, unlist), c("id", "for_type")
                           ][,
                             .(id,
                               som_bmu.bl = paste0(som_bmu.bl, ":", for_type),
                               som_bmu.bl.w)]
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
  r.bl.ten_cat <- reweigh_posterior(post.units, w = w.points)

  rc.ten_cat$arc <- arc(r.ten_cov$r[[ten_cat$name[i]]], r.bl.ten_cat)
  rc.ten_cat$rrc <- rrc(r.ten_cov$r[[ten_cat$name[i]]], r.bl.ten_cat)
  rc.ten_cat$som.units <- units.sub

  rc.ten_cov[[i]] <- rc.ten_cat
  rm(rc.ten_cat)
}

names(rc.ten_cov) <- ten_cat$name

message(paste0("Saving outputs to `", file.riskchange, "` …"))
saveRDS(rc.ten_cov, file.riskchange)

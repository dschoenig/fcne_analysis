library(data.table)
library(posterior)
library(sf)
library(ggplot2)
library(ggdist)
library(colorspace)

source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

region <- "cam"
n.threads <- 4

# path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.effects <- paste0(path.base, "models/gam/effects/")

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.risk.tenure <- paste0(path.effects, region, ".risk.tenure.rds")
file.risk.tenure_cov <- paste0(path.effects, region, ".risk.tenure_cov.rds")
# file.riskchange.geo <- paste0(path.effects, region, ".riskchange.geo.rds")
# file.riskchange.tenure <- paste0(path.effects, region, ".riskchange.tenure.rds")
# file.risk.geo.all <- paste0(path.effects, region, ".risk.geo.all.rds")
file.risk.geo.it_c <- paste0(path.effects, region, ".risk.geo.it_c.rds")
# file.risk.geo.pa_c <- paste0(path.effects, region, ".risk.geo.pa_c.rds")
# file.bg_adm0 <- paste0(path.data, "map_bg/gadm36_levels.gpkg")

setDTthreads(n.threads)


## BASELINES FOR COVARIATES

data.proc <- readRDS(file.data)

r.ten <- readRDS(file.risk.tenure)
r.ten_cov <- readRDS(file.risk.tenure_cov)
r.geo.it_c <- readRDS(file.risk.geo.it_c)

str(r.ten, max.level = 1)
str(r.geo.it_c, max.level = 1)


# Posterior for SOM units, baseline observations
post.units <- r.ten_cov$r$baseline

# Initial weights based on no. of observations for each BMU
n.units <- data.proc[it_type == "none" & pa_type == "none",
                     .(n = .N), som_bmu
                     ][order(som_bmu)]
w.units <- n.units$n
names(w.units) <- as.character(n.units$som_bmu)


# Reweigh based on the reference BMUs for each point observation

# groups <- r.ten$groups[is.na(adm0) & is.na(pa_type)]
# id.list <- groups$ids
# names(id.list) <- groups$group.label
id.list <- r.ten$groups$ids
names(id.list) <- r.ten$groups$group.label
ids.units <- data.proc[, .(id, som_bmu.bl)]
w.points <-
  lapply(id.list,
         \(x) {ids.units[id %in% x,
                         extract_weights(as.character(unlist(som_bmu.bl)))]
         })

r.bl.ten <- reweigh_posterior(post.units, w.units, w.points)

# selg <- r.ten$groups[is.na(pa_type) & it_type == "not_recognized", group.label]
# selg <- r.ten$groups[is.na(it_type) & pa_type == "direct_use", group.label]
# selg <- r.ten$groups[is.na(adm0), group.label]
selg <- r.ten$groups[adm0 == "MEX", group.label]
arc.ten <- arc(r.ten$full, r.bl.ten, selg)
rrc.ten <- rrc(r.ten$full, r.bl.ten, selg)

summary(rrc.ten, mean, median, Mode)
summary(rrc.ten)





## TESTING STUFF


id.list <- groups$ids
names(id.list) <- groups$group.label

post.units <- r.ten_cov$r$baseline



# Initial weights based on no. of observations for each BMU
w.units <- n.units$n
names(w.init) <- as.character(n.units$som_bmu)

# Reweigh based on the reference BMUs for each point observation
w.points <-
  lapply(id.list,
         \(x) {
           ids.units[id %in% x,
                     extract_weights(as.character(unlist(som_bmu.bl)))]
         })


# Compute baseline by reweighting posterior for reference observations
r.geo.bl <- reweigh_posterior(post.units, w.init, w.mod)




summary(var.w)

summary(arc(r.geo.it_c$full[,1:1000], var.w), mode = Mode, hdi) |>
summarize(mean(mode))


extract_weights <- function(x, normalize = FALSE) {
  w <- as.matrix(table(x))[,1]
  if(normalize) {
    w <- w / sum(w)
  }
  return(w)
}


weighted_avg <- function(x, ...) {
  UseMethod("weighted_avg", x)
}

weighted_avg.draws_matrix <- function(x, w) {
  y <-
    x[,names(w)] %*% diag(w) |>
    rowSums() |>
    rvar()
  return(y)
}

reweigh_posterior <- function(posterior, ...) {
  UseMethod("reweigh_posterior", posterior)
}

reweigh_posterior.draws_matrix <- function(posterior, w.init, w.mod) {
  w <- lapply(w.mod,
              \(x) {
                    w <- w.init[names(x)] * x
                    w / sum(w)
                    })
  var.w <- lapply(w, \(w) weighted_avg(post.units, w)) |>
                  as_draws_matrix()
  return(var.w)
}

  w.id <-

w.u
w.idx <- lapply(id.list, \(x) ids.units[id %in% x, extract_weights(as.character(unlist(som_bmu.bl)))])


  idx.units <- ids.units[1:10,]

  idx.units[, (bmu = as.character(unlist(som_bmu.bl))), by = id]
  idx.units[, (bmu = as.character(unlist(som_bmu.bl))), by = id][, (n = .N), by = c("id", "V1")]

  sel <- names(w.id)
  w.c <- w.u[sel] * w.id
  w.avg <- post.units[, names(w.c)] %*% diag(w.c / sum(w.c)) |>
           rowSums()

  |>
           rvar(w.avg)
 
post.units[names(w[[1]]))




           as.matrix(ncol = 1)

           summary(as_draws_matrix(w.avg))
          
  

  i=1
  



}


## CRS / LIMITS

crs.ea <- 
  list(cam = st_crs('PROJCS["Central_America_Equidistant_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Equidistant_Conic"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900001"]]'))

map_xlim <- list(cam = c(-80e4, 120e4))
map_ylim <- list(cam = c(-90e4, 80e4))




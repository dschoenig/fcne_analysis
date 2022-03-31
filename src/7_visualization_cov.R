library(data.table)
library(posterior)
library(sf)
library(ggplot2)
library(ggdist)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)

source("utilities.R")

# path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.effects <- paste0(path.base, "models/gam/effects/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

file.data.vis <- paste0(path.data.vis, "cov.rds")

regions <- c("amz", "cam")



if(!file.exists(file.data.vis)) {
  
  cov.sum <- list()

  # Prepare data for maps

  for(i in seq_along(regions)){
      
    region <- regions[i]

    message(paste0("Preparing data for region `", region, "` …"))

    file.risk.ten_cov <- paste0(path.effects, region, ".risk.tenure_cov.rds")
    file.riskchange.cov <- paste0(path.effects, region, ".riskchange.geo.rds")
    file.limit <- paste0(path.data.raw, region, ".limit.gpkg")
    file.areas <- paste0(path.data.raw, region, ".areas_union.gpkg")


    # Treatment of auxilary geospatial data

    message("Auxiliary geospatial data …")

    poly[[region]] <- list()

    poly[[region]]$areas <-
      st_read(file.areas) |>
      st_transform(crs.ea[[region]])
    poly[[region]]$areas$it_type <-
      factor(poly[[region]]$areas$it_type, levels = c("recognized", "not_recognized"))

    poly[[region]]$limit <-
      st_read(file.limit) |>
      st_transform(crs.ea[[region]])

    poly[[region]]$bg <- st_transform(bg_adm0, crs.ea[[region]])
    poly[[region]]$bg_coasts <- st_transform(bg_coasts, crs.ea[[region]])
    poly[[region]]$bg_is_limit <- st_intersection(poly[[region]]$bg, poly[[region]]$limit)

     
    # Forest loss risk

    message("Forest loss risk …")

    r.geo <- readRDS(file.risk.geo.all)

    r.geo.dt <-
    as_draws_df(r.geo$r) |>
    as.data.table() |>
    melt(measure.vars = 1:ncol(r.geo$r),
         variable.name = "group.label",
         value.name = "risk")

    geo.sum[[region]]$r <-
      r.geo.dt[,
           .(mean = mean(risk),
             sd = sd(risk),
             q2.5 = quantile(risk, 0.025),
             q97.5 = quantile(risk, 0.975)),
           by = group.label] |>
      merge(r.geo$map.units[,-"ids"], by = "group.label")

    rm(r.geo)


    # Absolute change in risk (compared to baseline)

    message("Absolute risk change …")

    rc.geo <- readRDS(file.riskchange.geo)

    arc.geo.dt <-
    as_draws_df(rc.geo$all$arc) |>
    as.data.table() |>
    melt(measure.vars = 1:ncol(rc.geo$all$arc),
         variable.name = "group.label",
         value.name = "arc")

    geo.sum[[region]]$arc <-
      arc.geo.dt[,
           .(mean = mean(arc),
             sd = sd(arc),
             q2.5 = quantile(arc, 0.025),
             q97.5 = quantile(arc, 0.975)),
           by = group.label] |>
      merge(rc.geo$all$map.units[,-"ids"], by = "group.label")

    rm(rc.geo)

  }

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(poly = poly,
       geo.sum = geo.sum) |>
  saveRDS(file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


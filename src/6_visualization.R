library(data.table)
library(posterior)
library(sf)
library(ggplot2)
library(ggdist)

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
prefix.file.effects <- paste0(region, ".eff.riskchange.")
file.effects.geo <- paste0(path.effects, prefix.file.effects, "geo.rds")
file.effects.tenure <- paste0(path.effects, prefix.file.effects, "tenure.rds")
file.bg_adm0 <- paste0(path.data, "map_bg/gadm36_levels.gpkg")

setDTthreads(n.threads)

## CRS / LIMITS

crs.ea <- 
  list(cam = st_crs('PROJCS["Central_America_Equidistant_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Equidistant_Conic"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900001"]]'))

map_xlim <- list(cam = c(-80e4, 120e4))
map_ylim <- list(cam = c(-90e4, 80e4))


## DATA PREPARATION

it_c.arc.sum <- summarize_draws(rc.geo$it_c$arc,
                                mean, 
                                mode = Mode,
                                median,
                                sd,
                                mad,
                                \(x) hdi(x, mass = c(0.5, 0.95)),
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                as.data.table()

it_c.arc <- summarize_draws(rc.geo$it_c$arc,
                                mean, 
                                mode = Mode,
                                median,
                                sd,
                                mad,
                                \(x) hdi(x, mass = c(0.5, 0.95)),
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                as.data.table() |>
                setnames("variable", "group.label") |>
                merge(rc.geo$it_c$map.units[, !"ids"],
                      by = "group.label") |>
                setnames("group.label", "rcell")

it_c.rrc <- summarize_draws(rc.geo$it_c$rrc,
                                mean, 
                                mode = Mode,
                                median,
                                sd,
                                mad,
                                \(x) hdi(x, mass = c(0.5, 0.95)),
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                as.data.table() |>
                setnames("variable", "group.label") |>
                merge(rc.geo$it_c$map.units[, !"ids"],
                      by = "group.label") |>
                setnames("group.label", "rcell")

pa_c.arc <- summarize_draws(rc.geo$pa_c$arc,
                                mean, 
                                mode = Mode,
                                median,
                                sd,
                                mad,
                                \(x) hdi(x, mass = c(0.5, 0.95)),
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                as.data.table() |>
                setnames("variable", "group.label") |>
                merge(rc.geo$pa_c$map.units[, !"ids"],
                      by = "group.label") |>
                setnames("group.label", "rcell")

pa_c.rrc <- summarize_draws(rc.geo$pa_c$rrc,
                                mean, 
                                mode = Mode,
                                median,
                                sd,
                                mad,
                                \(x) hdi(x, mass = c(0.5, 0.95)),
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                as.data.table() |>
                setnames("variable", "group.label") |>
                merge(rc.geo$pa_c$map.units[, !"ids"],
                      by = "group.label") |>
                setnames("group.label", "rcell")

cov.arc <- summarize_draws(rc.geo$cov$arc,
                                mean, 
                                mode = Mode,
                                median,
                                sd,
                                mad,
                                \(x) hdi(x, mass = c(0.5, 0.95)),
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                as.data.table() |>
                setnames("variable", "group.label") |>
                merge(rc.geo$cov$map.units[, !"ids"],
                      by = "group.label") |>
                setnames("group.label", "rcell")

cov.rrc <- summarize_draws(rc.geo$cov$rrc,
                                mean, 
                                mode = Mode,
                                median,
                                sd,
                                mad,
                                \(x) hdi(x, mass = c(0.5, 0.95)),
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                as.data.table() |>
                setnames("variable", "group.label") |>
                merge(rc.geo$cov$map.units[, !"ids"],
                      by = "group.label") |>
                setnames("group.label", "rcell")

## MAPS

bg_adm0 <- st_read(paste0(path.data, "map_bg/gadm36_levels.gpkg"),
                   query = "SELECT * FROM level0 
                            WHERE GID_0 IN ('ABW', 'AIA', 'ARG', 'ATG', 'BES', 
                                            'BHS', 'BLM', 'BLZ', 'BOL', 'BRA', 
                                            'BRB', 'CHL', 'COL', 'CRI', 'CUB', 
                                            'CUW', 'CYM', 'DMA', 'DOM', 'ECU', 
                                            'FLK', 'GLP', 'GRD', 'GTM', 'GUF', 
                                            'GUY', 'HND', 'HTI', 'JAM', 'KNA', 
                                            'LCA', 'MAF', 'MEX', 'MSR', 'MTQ', 
                                            'NIC', 'PAN', 'PER', 'PRI', 'PRY', 
                                            'SGS', 'SLV', 'SUR', 'SXM', 'TCA', 
                                            'TTO', 'UMI', 'URY', 'VCT', 
                                            'VEN', 'VGB', 'VIR', 'XCL')"
                   ) |>
           st_transform(crs.ea[[region]])


map_theme <-  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey90", colour = NA),
        panel.grid = element_line(colour = "grey75")
        # , legend.position = "bottom",
        # , legend.justification = "right"
        )

 bins <-c(1, 0.5, 0.25, 0.1, 0.05, 0.01)

map.it_c.arc <- 
  ggplot(it_c.arc[n >1]) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(fill = median,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = TRUE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_binned_diverging(palette = "Blue-Red 3", mid = 0,
                              breaks = c(-bins, rev(bins))
                                  # ,limits = c(-0.1, 0.1)
                                  # ,oob = scales::squish
                                  ) +
  # scale_fill_viridis_c(limits = c(0, 1)
  #                      # , rescaler = rescaler_div_mean
  #                      ) +
  # scale_fill_gradientn(colours = scico(256, alpha = NULL,
  #                                        begin = 0, end = 1, direction = 1, "imola"),
  #                        rescaler = rescaler_div_mean,
  #                        limits = c(0,1),
  #                        oob = scales::squish) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
map.it_c.arc

map.pa_c.arc <- 
  ggplot(pa_c.arc[n >1]) +
  geom_sf(data = bg_adm0, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = median,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = TRUE) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3", mid = 0, limits = c(-1, 1)) +
  # scale_fill_viridis_c(limits = c(0, 1)
  #                      # , rescaler = rescaler_div_mean
  #                      ) +
  # scale_fill_gradientn(colours = scico(256, alpha = NULL,
  #                                        begin = 0, end = 1, direction = 1, "imola"),
  #                        rescaler = rescaler_div_mean,
  #                        limits = c(0,1),
  #                        oob = scales::squish) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
map.pa_c.arc

map.cov.arc <- 
  ggplot(cov.arc[n >1]) +
  geom_sf(data = bg_adm0, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = median,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = TRUE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_binned_diverging(palette = "Blue-Red 3", mid = 0,
                              breaks = c(-bins, rev(bins))
                                  # ,limits = c(-0.1, 0.1)
                                  # ,oob = scales::squish
                                  ) +
  # scale_fill_viridis_c(limits = c(0, 1)
  #                      # , rescaler = rescaler_div_mean
  #                      ) +
  # scale_fill_gradientn(colours = scico(256, alpha = NULL,
  #                                        begin = 0, end = 1, direction = 1, "imola"),
  #                        rescaler = rescaler_div_mean,
  #                        limits = c(0,1),
  #                        oob = scales::squish) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
map.cov.arc
















log_trans

rescaler_div_log <- function(x,
                             to = c(-1,1),
                             from = range(x, na.rm = TRUE, finite = TRUE),
                             mp = 0) {
  x2 <- rescaler_div_mp(x, to, from, mp)
  x2
}

rescaler_div_mp <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), mp = 0) {
  id.l <- which(x <= mp)
  id.u <- which(x > mp)
  int <- to[2] - to[1]
  x[id.l] <- (from[1] - x[id.l]) / (from[1] - mp) * (int / 2) 
  x[id.u] <- (int / 2) + (mp - x[id.u]) / (mp - from[2]) * (int / 2)
  return(x)
}

rescaler_div_mean <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) {
  mp <- mean(x)
  rescaler_div_mp(x, to, from, mp)
}

rescaler_div_0 <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) {
  rescaler_div_mp(x, to, from, 0)
}

data.risk.e[, rrc.reg.bin := cut(rrc.reg,
                                 # breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.5, 1, 2, Inf))]
                                 breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.5, 1, 2, 5, Inf))]
cols <- scico(length(levels(data.risk.e$rrc.reg.bin)),
                            alpha = NULL, begin = 0, end = 1, direction = -1, "roma")
names(cols) <- levels(data.risk.e$rrc.reg.bin)

map.rrc <- 
  ggplot(data.risk.e) +
  geom_sf(data = bg_adm0, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = rrc.reg.bin,
                            x = ea_east.bin, y = ea_north.bin)) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  # scale_fill_scico(palette = "vik", direction = -1, midpoint = 0) +
  # scale_fill_continuous_diverging(palette = "Blue-Red", mid = 0) +
  # scale_fill_distiller(type = "div", palette = 7, direction = -1,
  #                      values = rescale_mp(data.risk.e$rrc.reg)) +
  # scale_fill_gradientn(colours = scico(8, alpha = NULL,
  #                                      begin = 0, end = 1, direction = -1, "roma"),
  #                      rescaler = rescaler_div_0,
  #                      # limits = c(-0.9,10),
  #                      breaks = c(0,1),
  #                      oob = scales::squish) +
  scale_fill_manual(values = cols) +
  labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
map.rrc




?scale_fill_discrete

?scale_fill_binned

?scale_fill_scico
?scale_fill_gradientn

hcl_palettes("diverging", n = 7, plot = TRUE)

library(scico)
library(colorspace)

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

path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.effects <- paste0(path.base, "models/gam/effects/")

# file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.n50.rds")
file.riskchange.tenure <- paste0(path.effects, region, ".riskchange.tenure.rds")
file.riskchange.tenure_areas <- paste0(path.effects, region, ".riskchange.tenure.rds")
file.riskchange.geo <- paste0(path.effects, region, ".riskchange.geo.rds")
file.risk.geo.all <- paste0(path.effects, region, ".risk.geo.all.rds")

setDTthreads(n.threads)



## CRS / LIMITS

crs.ea <- 
  list(cam = st_crs('PROJCS["Central_America_Equidistant_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Equidistant_Conic"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900001"]]'))

map_xlim <- list(cam = c(-80e4, 120e4))
map_ylim <- list(cam = c(-90e4, 80e4))


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

r.geo.all <- readRDS(file.risk.geo.all)
r.geo.all.sum <- summarize_draws(r.geo.all$full,
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
             as.data.table() |>
             setnames("variable", "group.label") |>
             merge(r.geo.all$map.units[, !"ids"],
                   by = "group.label") |>
             setnames("group.label", "rcell")


rc.geo <- readRDS(file.riskchange.geo)

arc.geo.all.sum <- summarize_draws(rc.geo$all$arc,
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                   as.data.table() |>
                   setnames("variable", "group.label") |>
                   merge(rc.geo$all$map.units[, !"ids"],
                         by = "group.label") |>
                   setnames("group.label", "rcell")

rrc.geo.all.sum <- summarize_draws(rc.geo$all$rrc,
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                   as.data.table() |>
                   setnames("variable", "group.label") |>
                   merge(rc.geo$all$map.units[, !"ids"],
                         by = "group.label") |>
                   setnames("group.label", "rcell")


arc.geo.it_c.sum <- summarize_draws(rc.geo$it_c$arc,
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                   as.data.table() |>
                   setnames("variable", "group.label") |>
                   merge(rc.geo$it_c$map.units[, !"ids"],
                         by = "group.label") |>
                   setnames("group.label", "rcell")

arc.geo.pa_c.sum <- summarize_draws(rc.geo$pa_c$arc,
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                   as.data.table() |>
                   setnames("variable", "group.label") |>
                   merge(rc.geo$pa_c$map.units[, !"ids"],
                         by = "group.label") |>
                   setnames("group.label", "rcell")



rcell.it_rec <- rc.geo$it$map.units[it_type == "recognized", group.label]
arc.geo.it_rec.sum <- summarize_draws(rc.geo$it$arc[,rcell.it_rec],
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                   as.data.table() |>
                   setnames("variable", "group.label") |>
                   merge(rc.geo$it$map.units[group.label %in% rcell.it_rec, !"ids"],
                         by = "group.label") |>
                   setnames("group.label", "rcell")

rcell.it_notrec <- rc.geo$it$map.units[it_type == "not_recognized", group.label]
arc.geo.it_notrec.sum <- summarize_draws(rc.geo$it$arc[,rcell.it_notrec],
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
                   as.data.table() |>
                   setnames("variable", "group.label") |>
                   merge(rc.geo$it$map.units[group.label %in% rcell.it_notrec, !"ids"],
                         by = "group.label") |>
                   setnames("group.label", "rcell")

map_theme <-  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey90", colour = NA),
        panel.grid = element_line(colour = "grey75")
        # , legend.position = "bottom",
        # , legend.justification = "right"
        )

r.geo.all.map <- 
  ggplot(r.geo.all.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = TRUE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_viridis_c(limits = c(0,1), option = "D") +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
r.geo.all.map

arc.geo.all.map <- 
  ggplot(arc.geo.all.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = TRUE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_fermenter(type = "div"
                       , palette = 5, direction = -1, n.breaks = 13,
                       # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
                       ,limits = c(-0.5, 0.5)
                       # ,limits = c(-1, 1)
                       ,oob = scales::squish
                       ) +
  # scale_fill_continuous_diverging(
  #                                 # palette = "Blue-Red 2"
  #                                 palette = "Purple-Green", rev = TRUE
  #                                 ,mid = 0
  #                                 # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
  #                                 ,limits = c(-0.5, 0.5)
  #                                 # ,limits = c(-1, 1)
  #                                 ,oob = scales::squish
  #                                 ) +
  # scale_fill_binned_diverging(palette = "Blue-Red 3"
  #                                 # ,n.breaks = 8
  #                             # ,breaks = seq(-0.5, 0.5, 0.1),
  #                                 # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
  #                                 ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
  #                                 ,limits = c(-0.51, 0.51)
  #                                 # ,limits = c(-1, 1)
  #                                 ,oob = scales::squish
  #                                 ) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
arc.geo.all.map

rrc.geo.all.map <- 
  ggplot(rrc.geo.all.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = TRUE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3"
                                  ,mid = 0 
                                  # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
                                  # ,limits = c(-0.5, 0.5)
                                  # ,limits = c(-1, 5)
                                  # ,oob = scales::squish
                                  ) +
  # scale_fill_binned_diverging(palette = "Blue-Red 3"
  #                                 # ,n.breaks = 8
  #                             # ,breaks = seq(-0.5, 0.5, 0.1),
  #                                 # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
  #                                 ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
  #                                 ,limits = c(-0.51, 0.51)
  #                                 # ,limits = c(-1, 1)
  #                                 ,oob = scales::squish
  #                                 ) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
rrc.geo.all.map


arc.geo.it_c.map <- 
  ggplot(arc.geo.it_c.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = TRUE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3"
                                  ,mid = 0
                                  # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
                                  ,limits = c(-0.5, 0.5)
                                  # ,limits = c(-1, 1)
                                  ,oob = scales::squish
                                  ) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
arc.geo.it_c.map




arc.geo.it_rec.map <- 
  ggplot(arc.geo.it_rec.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin)
              ,interpolate = TRUE
              ) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3"
                                  ,mid = 0
                                  # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
                                  ,limits = c(-0.5, 0.5)
                                  # ,limits = c(-1, 1)
                                  ,oob = scales::squish
                                  ) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
arc.geo.it_rec.map

arc.geo.it_notrec.map <- 
  ggplot(arc.geo.it_notrec.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin)
              ,interpolate = TRUE
              ) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3"
                                  ,mid = 0
                                  # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
                                  ,limits = c(-0.5, 0.5)
                                  # ,limits = c(-1, 1)
                                  ,oob = scales::squish
                                  ) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
arc.geo.it_notrec.map


arc.geo.pa_c.map <- 
  ggplot(arc.geo.pa_c.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = TRUE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey50", size = 0.5) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  # scale_fill_continuous_diverging(palette = "Blue-Red 3"
  #                                 ,mid = 0
  #                                 # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
  #                                 ,limits = c(-0.5, 0.5)
  #                                 # ,limits = c(-1, 1)
  #                                 ,oob = scales::squish
  #                                 ) +
  scale_fill_binned_diverging(palette = "Blue-Red 3"
                                  # ,n.breaks = 8
                              ,breaks = seq(-0.5, 0.5, 0.1),
                                  # ,breaks = c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
                                  ,limits = c(-0.51, 0.51)
                                  # ,limits = c(-1, 1)
                                  ,oob = scales::squish
                                  ) +
  labs(fill = "Absolute change in risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
arc.geo.pa_c.map



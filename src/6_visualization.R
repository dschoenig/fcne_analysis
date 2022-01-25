library(data.table)
library(posterior)
library(sf)
library(ggplot)
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
file.effects.tenure <- paste0(path.effects, region, ".eff.tenure.rds")
file.effects.location <- paste0(path.effects, region, ".eff.location.rds")
file.bg_adm0 <- paste0(path.data, "map_bg/gadm36_levels.gpkg")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## CRS / LIMITS

crs.ea <- 
  list(cam = st_crs('PROJCS["Central_America_Equidistant_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Equidistant_Conic"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900001"]]'))

map_xlim <- list(cam = c(-80e4, 120e4))
map_ylim <- list(cam = c(-90e4, 80e4))


## DATA PREPARATION

effects.tenure <- readRDS(file.effects.tenure)

selg <- effects.tenure$groups[is.na(adm0) & n > 10000, group.id]

selg <- c(1, effects.tenure$groups[adm0 == "SLV", group.id])

selg <- c(1, effects.tenure$groups[it_type == "recognized", group.id])

selg <- c(1, effects.tenure$groups[!is.na(adm0) & it_type == "recognized", group.id])

summary(rrc(effects.tenure[[1]][,selg], "it_type.none:pa_type.none"), mean, median, sd)

data.fit[, risk := loc.means]
risk.bl.reg <- mean(data.fit[it_type == "none" & pa_type == "none", risk])

# data.fit <- data.fit[it_type != "none"]

res <- 1e3
corner <- c(min(data.fit$ed_east), min(data.fit$ed_north))
corner <- floor(corner / res) * res
bins.ea <- bin_cols(data.fit, c("ea_east", "ea_north"),
                      rep(res, 2), corner,
                      bin.names = c("ea_east.bin", "ea_north.bin"))
data.fit[, `:=`(ea_east.bin = bins.ea[["ea_east.bin"]],
                ea_north.bin = bins.ea[["ea_north.bin"]])]

data.risk.e <- data.fit[, .(n = .N, risk = E(risk)), c("ea_east.bin", "ea_north.bin")]


data.risk.e[, rrc.reg := risk/risk.bl.reg -1]


## MAPS

bg_adm0 <- st_read("~/projects/data/source/gadm/3.6/world/gadm36_levels.gpkg",
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
  theme(panel.background = element_rect(fill = "grey97", colour = NA),
        panel.grid = element_line(colour = "grey82")
        # , legend.position = "bottom",
        # , legend.justification = "right"
        )


map.risk <- 
  ggplot(data.risk.e) +
  geom_sf(data = bg_adm0, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = risk,
                            x = ea_east.bin, y = ea_north.bin)) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_viridis_c(limits = c(0, 1)
                       # , rescaler = rescaler_div_mean
                       ) +
  # scale_fill_gradientn(colours = scico(256, alpha = NULL,
  #                                        begin = 0, end = 1, direction = 1, "imola"),
  #                        rescaler = rescaler_div_mean,
  #                        limits = c(0,1),
  #                        oob = scales::squish) +
  labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
map.risk



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

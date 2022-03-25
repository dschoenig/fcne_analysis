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
path.figures <- paste0(path.base, "figures/")

file.data.vis <- paste0(path.data.vis, "maps.rds")

regions <- c("amz", "cam")


## COLOURS AND LABELS

## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.map <- col.div[c(3,6,17)]


## MAP SETUP

crs.ea <- 
  list(cam = st_crs('PROJCS["Central_America_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900002"]]'),
       amz = st_crs('PROJCS["Amazon_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",-5.59],PARAMETER["longitude_of_center",-62.05],PARAMETER["standard_parallel_1",3.81],PARAMETER["standard_parallel_2",-15.62],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900004"]]'))

map_xlim <- list(cam = c(-80e4, 120e4), amz = c(-240e4, 210e4))
map_ylim <- list(cam = c(-90e4,  80e4), amz = c(-200e4, 185e4))


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
                   )

bg_coasts <- st_union(bg_adm0, is_coverage = TRUE)


# poly[[region]]$anot.adm0 <- data.frame(adm0 = c("BLZ", "CRI", "GTM", "HND",
#                                  "MEX", "NIC", "PAN", "SLV"),
#                         x = c(0.5, 3, -4.5, 2.75,
#                               -4.75, 5, 8.75, -1.5) * 1e5,
#                         y = c(3.5, -6.5, -1.5, 1.75,
#                               5, -1.5, -5.25, -2.25) * 1e5)

map_theme <-  
  theme_minimal(base_family = "IBMPlexSans", base_size = 10) +
  theme(panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid = element_line(colour = "grey75"),
        legend.title = element_text(size = 10),
        # , legend.position = "bottom",
        # , legend.justification = "right"
        )




## EFFECTS IN GEOGRAPHICAL SPACE ###############################################


poly <- list()
geo.sum <- list()


# Prepare data for maps

for(i in seq_along(regions)){

  message(paste0("Preparing data for region `", region, "` …"))
    
  region <- regions[i]

  file.risk.geo.all <- paste0(path.effects, region, ".risk.geo.all.rds")
  file.riskchange.geo <- paste0(path.effects, region, ".riskchange.geo.rds")
  file.limit <- paste0(path.data.raw, region, ".limit.gpkg")
  file.areas <- paste0(path.data.raw, "raw/", region, ".areas_union.gpkg")


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


## MAPS

maps <- list()

for(i in seq_along(regions)) {

  message(paste0("Preparing maps for region `", region, "` …"))

  region <- regions[i]

  # Study region: ITs and PAs

  maps[[region]]$areas <- 
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey65", size = 0.4) +
    # geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
    geom_sf(data = poly[[region]]$limit, fill = "grey95", colour = NA) +
    geom_sf(data = subset(poly[[region]]$areas, is.na(pa_type)),
            aes(fill = it_type), colour = NA) +
    geom_sf_pattern(data = subset(poly[[region]]$areas, is.na(it_type)),
            aes(pattern = pa_type), colour = c.map[3], size = 0.3,, fill = NA,
                pattern_colour = NA, pattern_fill = c.map[3], pattern_spacing = 0.005,
                pattern_density = 0.35) +
    geom_sf(data = poly[[region]]$bg_is_limit, fill = NA, colour = "grey30", size = 0.4) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey30", size = 0.3) +
    # geom_text(data = anot.adm0, aes(x = x, y = y, label = adm0), size = 3) +
    scale_fill_manual(values = c.map[1:2],
                      breaks = c("recognized", "not_recognized"),
                      labels = c("IT, recognized", "IT, not recognized"),
                      name = NULL,
                      guide = guide_legend(byrow = TRUE)) +
    scale_pattern_manual(values = c("indirect_use" = "stripe", "direct_use" = "none"),
                               breaks = c("indirect_use", "direct_use"),
                               labels = c("PA, indirect use", "PA, direct use"),
                               name = NULL,
                               guide = guide_legend(override.aes = list(pattern_spacing = 0.01,
                                                                        size = 1),
                                                                        byrow = TRUE)) +
    # coord_sf(crs = crs.ea[[region]]) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, style = "ticks") +
    labs(x = NULL, y = NULL) +
    map_theme +
    theme(legend.position = "right",
          legend.justification = c(0,1)
          , legend.spacing.y = unit(0.1, "cm")
          # , legend.key = element_rect(size = 10, colour = "white")
          # , legend.key.size = unit(0.5, "cm")
          )

  # Risk

  maps[[region]]$risk <- 
    # r.dt.sum2[, .(wmean = (mean * n), n, ea_east.bin.bin, ea_north.bin.bin, group.label)
    #           ][, .(mean = sum(wmean) / sum(n)), by = c("ea_east.bin.bin", "ea_north.bin.bin")] |>
    ggplot(geo.sum[[region]]$r) +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.4) +
    geom_raster(mapping = aes(
                              fill = mean,
                              x = ea_east.bin, y = ea_north.bin),
                              # x = ea_east.bin.bin, y = ea_north.bin.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.4) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey30", size = 0.3) +
    scale_fill_continuous_sequential(palette = "Viridis",
                                     rev = FALSE,
                                     limits = c(0,1),
                                     labels = scales::label_percent()) +
                                     # ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, style = "ticks") +
    guides(fill = guide_colorbar(ticks.colour = "grey35",
                                 ticks.linewidth = 1,
                                 frame.colour = "grey35",
                                 frame.linewidth = 1,
                                 barheight = 7.5,
                                 label.position = "left",
                                 label.hjust = 1,
                                 draw.ulim = FALSE,
                                 draw.llim = FALSE)) +
    labs(fill = "Forest loss risk", x = NULL, y = NULL) +
    map_theme +
    theme(legend.position = "right",
          legend.justification = c(0,1),
          legend.spacing.y = unit(5, "mm"))


  # Absolute risk change

  maps[[region]]$arc <- 
    # r.dt.sum2[, .(wmean = (mean * n), n, ea_east.bin.bin, ea_north.bin.bin, group.label)
    #           ][, .(mean = sum(wmean) / sum(n)), by = c("ea_east.bin.bin", "ea_north.bin.bin")] |>
    ggplot(geo.sum[[region]]$arc) +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.4) +
    geom_raster(mapping = aes(
                              fill = mean,
                              # fill = q97.5,
                              x = ea_east.bin, y = ea_north.bin),
                              # x = ea_east.bin.bin, y = ea_north.bin.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.4) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey30", size = 0.3) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                     ,rev = TRUE,
                                     ,breaks = seq(-0.2, 0.2, 0.05),
                                     ,labels = c("≤ -20%", "", "-10%", "", 0,
                                                 "", "+10%", "", "≥ +20%"),
                                     # ,limits = c(-0.25, 0.25)
                                     ,limits = c(-0.20, 0.20)
                                     ,oob = scales::squish
                                     ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, style = "ticks") +
    guides(fill = guide_colorbar(ticks.colour = "grey35",
                                 ticks.linewidth = 1,
                                 frame.colour = "grey35",
                                 frame.linewidth = 1,
                                 barheight = 7.5,
                                 label.position = "left",
                                 label.hjust = 1,
                                 draw.ulim = FALSE,
                                 draw.llim = FALSE)) +
    labs(fill = "Absolute change in\nforest loss risk", x = NULL, y = NULL) +
    map_theme +
    theme(legend.position = "right",
          legend.justification = c(0,1),
          legend.spacing.y = unit(5, "mm"))

}

# message(paste0("Storing results in `", file.data.vis, "` …"))

# list(poly = poly,
#      geo.sum = geo.sum,
#      maps = maps) |>
# saveRDS(file.data.vis)

# stored <- readRDS(file.data.vis)
# attach(stored)

maps.combined <-
  with(maps,
       (amz$areas + cam$areas + plot_layout(guides = "collect")) / 
       (amz$risk + cam$risk + plot_layout(guides = "collect")) /
       (amz$arc + cam$arc + plot_layout(guides = "collect")) +
       plot_annotation(tag_levels = "A") &
       theme(legend.position = "right",
             legend.justification = c(0,1),
             legend.spacing.y = unit(5, "mm"))
       )

maps.combined

tiff(paste0(path.figures, "fig1.tif"), width = 8.25, height = 10, unit = "in", res = 300)
maps.combined
dev.off()



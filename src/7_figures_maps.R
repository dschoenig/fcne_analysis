args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(sf)
library(stars)
library(ggplot2)
library(ggdist)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)

source("utilities.R")

hurr_type <- tolower(as.character(args[1]))
overwrite <- as.logical(as.character(args[2]))

if(is.na(overwrite)) {
  overwrite <- FALSE
}

# hurr_type <- "no_hurr"
# overwrite <- TRUE

path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.marginal <- paste0(path.base, "models/marginal/")
path.forestloss <- paste0(path.base, "models/forestloss/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.geo <- paste0(path.base, "results/geo/")
if(!dir.exists(path.geo)) dir.create(path.geo, recursive = TRUE)

if(is.na(hurr_type)) {
  hurr_type <- "otto"
}
if(hurr_type == "no_hurr") {
  hurr_suf <- ".no_hurr"
} else {
  hurr_suf <- ""
}

file.bg.adm0 <- paste0(path.data, "auxiliary/gadm41_levels_neotropics.gpkg")
file.bg.coasts <- paste0(path.data, "auxiliary/bg_coasts.gpkg")
file.data.vis <- paste0(path.data.vis, "maps", hurr_suf, ".rds")
file.fig.maps <- paste0(path.figures, "maps", hurr_suf, ".png")
# file.fig.fl.ci <- paste0(path.figures, "si.maps.fl.ci.png")
file.fig.mar.ci <- paste0(path.figures, "si.maps.mar.ci.png")
# file.fig.mar.uc <- paste0(path.figures, "si.maps.mar.uc.png")
# file.fig.n <- paste0(path.figures, "si.maps.n.png")
# inf.fig.cov <- "si.maps.cov."
# suf.geo.fl <- ".fl.tif"
suf.geo.mar <- ".mar.tif"


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

# poly[[region]]$anot.adm0 <- data.frame(adm0 = c("BLZ", "CRI", "GTM", "HND",
#                                  "MEX", "NIC", "PAN", "SLV"),
#                         x = c(0.5, 3, -4.5, 2.75,
#                               -4.75, 5, 8.75, -1.5) * 1e5,
#                         y = c(3.5, -6.5, -1.5, 1.75,
#                               5, -1.5, -5.25, -2.25) * 1e5)

map_theme <-  
  theme_minimal(base_family = "IBMPlexSans", base_size = 7) +
  theme(panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid = element_line(colour = "grey75"),
        legend.position = "right",
        legend.justification = c(0,1),
        legend.title = element_text(size = rel(0.75), hjust = 0,
                                    margin = margin(t = 3, b = 7),
                                    lineheight = rel(1)),
        legend.text = element_text(size = rel(0.65)),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(10, "pt"),
        strip.text = element_text(size = rel(1), hjust = 0,
                                  margin = margin(b = 3, t = 6)),
        plot.tag = element_text(margin = margin(t = 0, r = 6, b = 6, l = 0),
                                size = rel(1.5)),
        plot.tag.location = "margin"
        )

map_guide_fill <-
  guides(fill = guide_colorbar(theme = theme(legend.ticks = element_line(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.frame = element_rect(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.text = element_text(hjust = 1),
                                             legend.text.position = "right",
                                             legend.key.width = unit(7.5, "pt"),
                                             legend.key.height = unit(65, "pt"),
                                             legend.ticks.length = unit(2, "pt")),
                               draw.ulim = TRUE,
                               draw.llim = TRUE
                               ))

cat.lab <- 
  data.table(cat.label = c("Long-term disturbance (deforestation)",
                           "Short-term distrubance (not followed by deforestation)",
                           "IT, recognized", "IT, not recognized",
                           "PA, category I-IV", "PA, category V-VI"),
             it_type = c(NA, NA,
                         "recognized", "not_recognized",
                         NA, NA),
             pa_type = c(NA, NA,
                         NA, NA,
                         "indirect_use", "direct_use"),
             resp_type = c("def", "deg",
                           NA, NA,
                           NA, NA))
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]

title.wrap <- scales::label_wrap(20)
mar.wrap <- scales::label_wrap(25)

# dis.title <- "Forest loss risk\n(2011 – 2020)"
# fl.sd.title <- "Standard deviation of the\nabsolute marginal difference\nin forest loss risk\n(2011 – 2020)"
# fl.iqr.title <- "Interquartile range of the\nabsolute marginal difference\nin forest loss risk\n(2011 – 2020)"
# fl.cv.title <- "Coefficient of variation\nfor forest loss risk\n(2011 – 2020)"
mar.def.title <- "Absolute difference in proportion affected by long-term disturbance (2011 – 2020)"
mar.deg.title <- "Absolute difference in proportion affected by short-term disturbance (2011 – 2020)"
# mar.mean.title <- "Mean of the\nabsolute marginal difference\nin forest loss risk\n(2011 – 2020)"
# mar.med.title <- "Median of the\nabsolute marginal difference\nin forest loss risk\n(2011 – 2020)"
# mar.sd.title <- "Standard deviation of the\nabsolute marginal difference\nin forest loss risk\n(2011 – 2020)"
# mar.iqr.title <- "Interquartile range of the\nabsolute marginal difference\nin forest loss risk\n(2011 – 2020)"
# mar.cv.title <- "Coefficient of variation for\nthe absolute marginal difference\nin forest loss risk\n(2011 – 2020)"
q5.title <- "5% posterior quantile"
q25.title <- "25% posterior quantile"
q75.title <- "75% posterior quantile"
q95.title <- "95% posterior quantile"
mean.title <- "Posterior mean"
# med.title <- "Posterior median"
# sd.title <- "Posterior standard deviation"
# iqr.title <- "Posterior interquartile range"


## EFFECTS IN GEOGRAPHICAL SPACE ###############################################

if(!file.exists(file.data.vis) | overwrite == TRUE) {
  
  poly <- list()
  geo.sum <- list()

  # Prepare data for maps

  # bg_adm0 <- st_read(paste0(path.data, "auxiliary/gadm_410-levels.gpkg"),
  #                    query = "SELECT * FROM ADM_0 
  #                             WHERE GID_0 IN ('ABW', 'AIA', 'ARG', 'ATG', 'BES', 
  #                                             'BHS', 'BLM', 'BLZ', 'BOL', 'BRA', 
  #                                             'BRB', 'CHL', 'COL', 'CRI', 'CUB', 
  #                                             'CUW', 'CYM', 'DMA', 'DOM', 'ECU', 
  #                                             'FLK', 'GLP', 'GRD', 'GTM', 'GUF', 
  #                                             'GUY', 'HND', 'HTI', 'JAM', 'KNA', 
  #                                             'LCA', 'MAF', 'MEX', 'MSR', 'MTQ', 
  #                                             'NIC', 'PAN', 'PER', 'PRI', 'PRY', 
  #                                             'SGS', 'SLV', 'SUR', 'SXM', 'TCA', 
  #                                             'TTO', 'UMI', 'URY', 'VCT', 
  #                                             'VEN', 'VGB', 'VIR', 'XCL')"
  #                    )
  # st_make_valid(bg_adm0) |>
  # st_write(file.bg.adm0, append = FALSE)
  # bg_coasts <-
  #   st_make_valid(bg_adm0) |>
  #   st_union() |>
  #   st_exterior_ring()
  # st_write(bg_coasts, file.bg.coasts)

  bg_adm0 <- st_read(file.bg.adm0)
  bg_coasts <- st_read(file.bg.coasts)


  for(i in seq_along(regions)){
      
    region <- regions[i]

    message(paste0("Preparing data for region `", region, "` …"))

    if(region == "cam") {
      hurr_suf_mar <- hurr_suf
    } else {
      hurr_suf_mar <- ""
    }

    file.data.proc <- paste0(path.data.proc, region, ".data.fit.proc.rds")
    file.mar.sam <- paste0(path.marginal, region, "/", region, ".sam.rds")
    # file.fl.geo.all <- paste0(path.forestloss, region, "/", region, ".geo.af", hurr_suf_mar, ".rds")
    name.sam <- paste0(region, ".geo.itpa", hurr_suf_mar, ".rds")
    file.mar.def.geo <- paste0(path.marginal, region, "/", region, ".def.geo.itpa", hurr_suf_mar, ".rds")
    file.mar.deg.geo <- paste0(path.marginal, region, "/", region, ".deg.geo.itpa", hurr_suf_mar, ".rds")
    file.limit <- paste0(path.data.raw, region, ".limit.gpkg")
    file.areas <- paste0(path.data.raw, region, ".areas_union_2015.gpkg")


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

     
    # # Forest loss risk

    # message("Forest loss risk …")

    # fl.geo <- readRDS(file.fl.geo.all)

    # geo.sum[[region]]$fl <-
    #   fl.geo[,
    #          .(
    #            forestloss.mean = mean(forestloss),
    #            forestloss.median = median(forestloss),
    #            forestloss.sd = sd(forestloss),
    #            forestloss.q2.5 = quantile(forestloss, 0.025),
    #            forestloss.q5 = quantile(forestloss, 0.05),
    #            forestloss.q25 = quantile(forestloss, 0.25),
    #            forestloss.q75 = quantile(forestloss, 0.75),
    #            forestloss.q95 = quantile(forestloss, 0.95),
    #            forestloss.q97.5 = quantile(forestloss, 0.975)),
    #          by = .(group.id, ea_east.bin, ea_north.bin)]
    # geo.sum[[region]]$fl[,
    #                      `:=`(forestloss.iqr = forestloss.q75 - forestloss.q25,
    #                           forestloss.cv = forestloss.sd / forestloss.mean)]

    # rm(fl.geo)


    # Absolute change in disturbance (compared to baseline)

    message("Absolute change in disturbance …")

    mar.sam <- readRDS(file.mar.sam)
    mar.def.geo <- readRDS(file.mar.def.geo)
    mar.deg.geo <- readRDS(file.mar.deg.geo)

    mar.def.geo[, resp_type := factor("def", levels = c("def", "deg"))]
    mar.deg.geo[, resp_type := factor("deg", levels = c("def", "deg"))]

    mar.geo <- rbind(mar.def.geo, mar.deg.geo)
    rm(mar.def.geo, mar.deg.geo)

    geo.sum[[region]]$mar <-
      mar.geo[,
             .(mar.mean = mean(marginal),
               mar.median = median(marginal),
               mar.sd = sd(marginal),
               mar.q2.5 = quantile(marginal, 0.025),
               mar.q5 = quantile(marginal, 0.05),
               mar.q25 = quantile(marginal, 0.25),
               mar.q75 = quantile(marginal, 0.75),
               mar.q95 = quantile(marginal, 0.95),
               mar.q97.5 = quantile(marginal, 0.975)),
             by = .(resp_type, group.id, ea_east.bin, ea_north.bin)] |>
      merge(mar.sam[[name.sam]], by = "group.id")

    geo.sum[[region]]$mar[,
                          `:=`(mar.iqr = mar.q75 - mar.q25,
                               mar.cv = mar.sd / mar.mean)]

    rm(mar.geo)
    gc()


  } # End loop over regions

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(poly = poly,
       geo.sum = geo.sum) |>
  saveRDS(file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}



## MAPS (POSTERIOR MEANS) ######################################################

maps <- list()

for(i in seq_along(regions)) {
    
  region <- regions[i]

  message(paste0("Preparing maps for region `", region, "` …"))

  # Study region: ITs and PAs

  maps[[region]]$areas <- 
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", linewidth = 0.3) +
    geom_sf(data = poly[[region]]$limit, fill = "grey95", colour = NA) +
    geom_sf(data = subset(poly[[region]]$areas, is.na(pa_type)),
            aes(fill = it_type), colour = NA) +
    geom_sf_pattern(data = subset(poly[[region]]$areas, is.na(it_type)),
                    aes(pattern = pa_type), colour = c.map[3], linewidth = 0.2,, fill = NA,
                    pattern_colour = NA, pattern_fill = c.map[3], pattern_spacing = 0.005,
                    pattern_density = 0.35) +
    geom_sf(data = poly[[region]]$bg_is_limit, fill = NA, colour = "grey30", size = 0.3) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_manual(values = c.map[1:2],
                      breaks = c("recognized", "not_recognized"),
                      labels = c("Recognized", "Not recognized"),
                      name = "Indigenous territories",
                      guide = guide_legend(byrow = TRUE,
                                           order = 1)) +
    scale_pattern_manual(values = c("indirect_use" = "stripe", "direct_use" = "none"),
                               breaks = c("indirect_use", "direct_use"),
                               labels = c("IUCN category I-IV", "IUCN category V-VI"),
                               name = "Protected areas",
                               guide = guide_legend(override.aes = list(pattern_spacing = 0.01,
                                                                        linewidth = 1),
                                                    theme = theme(legend.key.spacing.y = unit(2, "pt")),
                                                    byrow = TRUE,
                                                    order = 2)) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSans") +
    labs(x = NULL, y = NULL) +
    map_theme

  # # Forest loss risk

  # maps[[region]]$fl <- 
  #   ggplot(geo.sum[[region]]$fl) +
  #   geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
  #   geom_raster(mapping = aes(
  #                             fill = forestloss.mean,
  #                             x = ea_east.bin, y = ea_north.bin),
  #               interpolate = FALSE) +
  #   geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.3) +
  #   geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
  #   scale_fill_continuous_sequential(palette = "Viridis",
  #                                     rev = FALSE,
  #                                     limits = c(0,1),
  #                                     labels = scales::label_percent()) +
  #   coord_sf(crs = crs.ea[[region]], expand = FALSE, 
  #            xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
  #   scale_x_continuous(breaks = seq(-170, 0, 10)) +
  #   scale_y_continuous(breaks = seq(-80, 30, 10)) +
  #   annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
  #                    style = "ticks", text_cex = 0.5,
  #                    line_width = 0.5, text_family = "IBMPlexSans") +
  #   map_guide_fill +
  #   labs(fill = fl.title, x = NULL, y = NULL) +
  #   map_theme


  # Absolute change in long-term disturbance
  maps[[region]]$mar.def <- 
    geo.sum[[region]]$mar[resp_type == "def" & n >= 10] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_raster(mapping = aes(
                              fill = mar.mean,
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                     ,mid = 0
                                     ,rev = TRUE,
                                     ,breaks = seq(-0.15, 0.15, 0.05),
                                     ,labels = c("≤ -15%", "-10%", "-5%", "0%",
                                                 "+5%", "+10%", "≥ +15%"),
                                     ,limits = c(-0.15, 0.15)
                                     # ,breaks = seq(-0.2, 0.2, 0.05),
                                     # ,labels = c("≤ -20%", "", "-10%", "", 0,
                                     #             "", "+10%", "", "≥ +20%"),
                                     # ,limits = c(-0.20, 0.20)
                                     ,oob = scales::squish
                                     ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSans") +
    map_guide_fill +
    labs(fill = mar.wrap(mar.def.title), x = NULL, y = NULL) +
    map_theme


  # Absolute change in short-term disturbance
  maps[[region]]$mar.deg <- 

    geo.sum[[region]]$mar[resp_type == "deg" & n >= 10] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_raster(mapping = aes(
                              fill = mar.mean,
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                     ,mid = 0
                                     ,rev = TRUE,
                                     ,breaks = seq(-0.15, 0.15, 0.05),
                                     ,labels = c("≤ -15%", "-10%", "-5%", "0%",
                                                 "+5%", "+10%", "≥ +15%"),
                                     ,limits = c(-0.15, 0.15)
                                     # ,breaks = seq(-0.2, 0.2, 0.05),
                                     # ,labels = c("≤ -20%", "", "-10%", "", 0,
                                     #             "", "+10%", "", "≥ +20%"),
                                     # ,limits = c(-0.20, 0.20)
                                     ,oob = scales::squish
                                     ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSans") +
    map_guide_fill +
    labs(fill = mar.wrap(mar.deg.title), x = NULL, y = NULL) +
    map_theme

}



## EXPORT (FIGURES) ############################################################

message("Exporting figures …")

maps.combined <-
  with(maps,
       (amz$areas + cam$areas + plot_layout(guides = "collect")) / 
       (amz$mar.def + cam$mar.def + plot_layout(guides = "collect")) / 
       (amz$mar.deg + cam$mar.deg + plot_layout(guides = "collect")) +
       plot_annotation(tag_levels = list(c("A", "", "B", "", "C", ""))) &
       map_theme
       )

png(file.fig.maps, width = 7, height = 7.8, unit = "in", res = 600)
maps.combined
dev.off()


## EXPORT (GEODATA) ############################################################


message("Exporting geodata …")

map.res <- list(amz = 1e4, cam = 5e3)

for(i in seq_along(regions)){

  region <- regions[i]

  mar.reg <- geo.sum[[region]]$mar
  mar.grid <-
    CJ(ea_east.bin = seq(min(mar.reg$ea_east.bin),
                         max(mar.reg$ea_east.bin),
                         map.res[[region]]),
     ea_north.bin = seq(min(mar.reg$ea_north.bin),
                        max(mar.reg$ea_north.bin),
                        map.res[[region]]))

  resp.types <- c("def", "deg")

  for(i in seq_along(resp.types)) {
    rast.mar <-
      mar.reg[resp_type == resp.types[i]
              ][mar.grid, on = c("ea_east.bin", "ea_north.bin")
                ][, .(ea_east.bin, ea_north.bin,
                      mar.mean, mar.q5, mar.q95)] |>
      st_as_stars(dims = c("ea_east.bin", "ea_north.bin")) |>
      st_set_crs(crs.ea[[region]]) |>
      merge(name = "band") |>
      setNames("marginal")
  write_stars(rast.mar, paste0(path.geo, region, ".", resp.types[i], hurr_suf, suf.geo.mar))
  }

}




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
library(stringi)

source("utilities.R")

hurr_type <- tolower(as.character(args[1]))
overwrite <- as.logical(as.character(args[2]))

if(is.na(overwrite)) {
  overwrite <- FALSE
}

# hurr_type <- "no_hurr"
# overwrite <- TRUE

# hurr_type <- "no_hurr"
# overwrite <- FALSE

path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.marginal <- paste0(path.base, "models/marginal/")
path.agg <- paste0(path.base, "models/gam/agg/")
path.forestloss <- paste0(path.base, "models/forestloss/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.geo <- paste0(path.base, "results/geo/")
if(!dir.exists(path.geo)) dir.create(path.geo, recursive = TRUE)

if(is.na(hurr_type)) {
  hurr_type <- "hurr"
}
if(hurr_type == "no_hurr") {
  hurr_suf <- ".no_hurr"
} else {
  hurr_suf <- ""
}

file.bg.adm0 <- paste0(path.data, "auxiliary/gadm41_levels_neotropics.gpkg")
file.bg.coasts <- paste0(path.data, "auxiliary/bg_coasts.gpkg")
file.hurr.lf <- paste0(path.data.proc, "cam.hurr.landfall.gpkg")

file.data.vis <- paste0(path.data.vis, "maps", hurr_suf, ".rds")
file.fig.maps <- paste0(path.figures, "maps", hurr_suf, ".png")
file.fig.maps.press <- paste0(path.figures, "maps.press", hurr_suf, ".png")
file.fig.areas <- paste0(path.figures, "areas", hurr_suf, ".png")
file.fig.dist.amz <- paste0(path.figures, "maps.dist.amz", hurr_suf, ".png")
file.fig.press.amz <- paste0(path.figures, "maps.press.amz", hurr_suf, ".png")
file.fig.area.amz <- paste0(path.figures, "maps.area.amz", hurr_suf, ".png")
file.fig.dist.cam <- paste0(path.figures, "maps.dist.cam", hurr_suf, ".png")
file.fig.press.cam <- paste0(path.figures, "maps.press.cam", hurr_suf, ".png")
file.fig.area.cam <- paste0(path.figures, "maps.area.cam", hurr_suf, ".png")
file.fig.hurr <- paste0(path.figures, "hurr.landfall.png")
suf.geo.mar <- ".mar.tif"


regions <- c("amz", "cam")


## COLOURS AND LABELS

## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.map <- col.div[c(3,6,17)]
# c.map <- c("#CC79A7", "#E0AFCA", "#009E73")


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
  theme(panel.background = element_rect(fill = "grey99", colour = NA),
        panel.grid = element_line(colour = "grey75"),
        legend.position = "right",
        legend.justification = "center",
        legend.title = element_text(size = rel(0.75), hjust = 0,
                                    margin = margin(t = 3, b = 7),
                                    lineheight = rel(1)),
        legend.text = element_text(size = rel(0.65)),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(10, "pt"),
        strip.text = element_text(size = rel(1),
                                  lineheight = rel(1.15),
                                  hjust = 0.5,
                                  vjust = 0.5,
                                  color = "black",
                                  margin = margin(
                                                  5,
                                                  5,
                                                  5,
                                                  5)),
        strip.background = element_rect(fill = "gray93", colour = NA),
        plot.title = element_text(size = rel(1)),
        plot.subtitle = element_text(size = rel(0.85),
                                     margin = margin(t = 6, b = 3)),
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

dist.lab <-
  data.table(dist_type = c("def", "deg"),
             dist.label = c("Long-term disturbance (deforestation)",
                            "Short-term disturbance (not followed by deforestation)"),
             dist.label2 = c("Long-term disturbance\n(deforestation)",
                             "Short-term disturbance\n(not followed by deforestation)"))
dist.lab[, dist.label := factor(dist.label, levels = dist.label)]
dist.lab[, dist.label2 := factor(dist.label2, levels = dist.label2)]


fac.est.lab <-
  data.table(est_type = paste0("fac.", c("q5", "q25", "median", "q75", "q95")),
             est.label = c("5% quantile", "25% quantile", "Median", "75% quantile", "95% quantile"))
fac.est.lab[, est.label := factor(est.label, levels = est.label)]

agg.est.lab <-
  data.table(est_type = paste0("agg.", c("q5", "q25", "median", "q75", "q95")),
             est.label = c("5% quantile", "25% quantile", "Median", "75% quantile", "95% quantile"))
agg.est.lab[, est.label := factor(est.label, levels = est.label)]

area.est.lab <-
  data.table(est_type = paste0("area.mar.", c("q5", "q25", "median", "q75", "q95")),
             est.label = c("5% quantile", "25% quantile", "Median", "75% quantile", "95% quantile"))
area.est.lab[, est.label := factor(est.label, levels = est.label)]


cf.est.lab <-
  data.table(est_type = paste0("area.cf.", c("q5", "q25", "median", "q75", "q95")),
             est.label = c("5% quantile", "25% quantile", "Median", "75% quantile", "95% quantile"))
cf.est.lab[, est.label := factor(est.label, levels = est.label)]


wrap_title <- function(x, width = 25, ...) {
  paste(stri_wrap(x,
                  width = width,
                  whitespace_only = TRUE),
        collapse = "\n")
}


dist.title <- "Disturbance risk"
mar.title.av.l <- "Absolute marginal difference in area affected by disturbances (km² per pixel)"
mar.title.av.2l <- "Absolute marginal difference in\narea affected by disturbances (km² per pixel)"
mar.title.av <- wrap_title(mar.title.av.l)
cf.title.av.l <- "Area affected by disturbances under counterfactual conditions (km² per pixel)"
cf.title.av.2l <- "Area affected by disturbances under\ncounterfactual conditions (km² per pixel)"
cf.title.av <- wrap_title(cf.title.av.l)


area.lim <- list(amz = c(-10, 10),
                 cam = c(-5, 5))
area.breaks <- list(amz = seq(-10, 10, 2.5),
                    cam = seq(-5, 5, 1.25))
area.labels <- list(
                    amz = c("≤ –10", "", "–5", "", "0",
                            "", "+5", "", "≥ +10"),
                    cam = c("≤ –5.0", "", "–2.5", "", "0",
                            "", "+2.5", "", "≥ +5.0"))

# area.lim <- list(amz = c(-13.75, 13.75),
#                  cam = c(-6.875, 6.875))
# area.breaks <- list(amz = c(-13.75, seq(-10, 10, 2.5), 13.75),
#                     cam = c(-6.875, seq(-5, 5, 1.25), 6.875))
# area.labels <- list(
#                     amz = c("< –10", "–10", "", "–5", "", "0",
#                             "", "+5", "", "+10", "> +10"),
#                     cam = c("< –5", "–5", "", "–2.5", "", "0",
#                             "", "+2.5", "", "+5", "> +5"))


area.cf.lim <- list(amz = c(0, 25),
                 cam = c(0, 10))
area.cf.breaks <- list(amz = seq(0, 25, 5),
                    cam = seq(0, 10, 2.5))
area.cf.labels <- list(
                    amz = c("0", "5", "10", "15", "20", "≥ 25"),
                    cam = c("0", "2.5", "5.0", "7.5", "≥ 10"))


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
    file.area <- paste0(path.data.proc, region, ".sumstats.area", hurr_suf_mar, ".rds")
    file.mar.sam <- paste0(path.marginal, region, "/", region, ".sam.rds")
    name.sam <- paste0(region, ".geo.itpa", hurr_suf_mar, ".rds")
    file.mar.def.geo <- paste0(path.marginal, region, "/", region, ".def.geo.itpa", hurr_suf_mar, ".rds")
    file.mar.deg.geo <- paste0(path.marginal, region, "/", region, ".deg.geo.itpa", hurr_suf_mar, ".rds")
    file.agg.def.geo <- paste0(path.agg, region, "/", region, ".def.geo.rds")
    file.agg.deg.geo <- paste0(path.agg, region, "/", region, ".deg.geo.rds")
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


    if(region == "cam" & hurr_type == "hurr") {

      hurr.lf <-
        st_read(file.hurr.lf) |>
        st_transform(crs.ea[[region]])

      poly[[region]]$hurr_lf <-
        st_intersection(poly[[region]]$bg, hurr.lf) |>
        st_union() |>
        st_as_sf()

    }

    # Absolute change in disturbance (compared to baseline)

    message("Absolute change in disturbance …")

    mar.sam <- readRDS(file.mar.sam)
    area.undist <- readRDS(file.area)$undist.px
    mar.def.geo <-
      readRDS(file.mar.def.geo) |>
      merge(mar.sam[[name.sam]])
    mar.deg.geo <-
      readRDS(file.mar.deg.geo) |>
      merge(mar.sam[[name.sam]])

    mar.def.geo[, dist_type := factor("def", levels = c("def", "deg"))]
    mar.deg.geo[, dist_type := factor("deg", levels = c("def", "deg"))]

    mar.post <-
      rbind(mar.def.geo, mar.deg.geo) |>
      merge(area.undist)
    rm(mar.def.geo, mar.deg.geo)
    gc()
    # mar.post[, `:=`(area.prop.mar = marginal * area)]
    mar.post[, `:=`(area.prop.mar = marginal * area,
                    area.prop.fac = factual * area,
                    area.prop.cf = counterfactual * area)]


    mar.geo <-
      mar.post[,
               .(n.fac = unique(n.fac),
                 n.cf = unique(n.cf),
                 n.frac = unique(n.fac)/1e7,
                 mar.mean = mean(marginal),
                 mar.median = median(marginal),
                 mar.sd = sd(marginal),
                 mar.mad = mad(marginal),
                 mar.q5 = quantile(marginal, 0.05),
                 mar.q25 = quantile(marginal, 0.25),
                 mar.q75 = quantile(marginal, 0.75),
                 mar.q95 = quantile(marginal, 0.95),
                 fac.mean = mean(factual),
                 fac.median = median(factual),
                 fac.sd = sd(factual),
                 fac.mad = mad(factual),
                 fac.q5 = quantile(factual, 0.025),
                 fac.q25 = quantile(factual, 0.25),
                 fac.q75 = quantile(factual, 0.75),
                 fac.q95 = quantile(factual, 0.975),
                 cf.mean = mean(counterfactual),
                 cf.median = median(counterfactual),
                 cf.sd = sd(counterfactual),
                 cf.mad = mad(counterfactual),
                 cf.q5 = quantile(counterfactual, 0.025),
                 cf.q25 = quantile(counterfactual, 0.25),
                 cf.q75 = quantile(counterfactual, 0.75),
                 cf.q95 = quantile(counterfactual, 0.975),
                 area.mar.mean = mean(area.prop.mar),
                 area.mar.median = median(area.prop.mar),
                 area.mar.sd = sd(area.prop.mar),
                 area.mar.mad = mad(area.prop.mar),
                 area.mar.q5 = quantile(area.prop.mar, 0.05),
                 area.mar.q25 = quantile(area.prop.mar, 0.25),
                 area.mar.q75 = quantile(area.prop.mar, 0.75),
                 area.mar.q95 = quantile(area.prop.mar, 0.95),
                 area.fac.mean = mean(area.prop.fac),
                 area.fac.median = median(area.prop.fac),
                 area.fac.sd = sd(area.prop.fac),
                 area.fac.mad = mad(area.prop.fac),
                 area.fac.q5 = quantile(area.prop.fac, 0.05),
                 area.fac.q25 = quantile(area.prop.fac, 0.25),
                 area.fac.q75 = quantile(area.prop.fac, 0.75),
                 area.fac.q95 = quantile(area.prop.fac, 0.95),
                 area.cf.mean = mean(area.prop.cf),
                 area.cf.median = median(area.prop.cf),
                 area.cf.sd = sd(area.prop.cf),
                 area.cf.mad = mad(area.prop.cf),
                 area.cf.q5 = quantile(area.prop.cf, 0.05),
                 area.cf.q25 = quantile(area.prop.cf, 0.25),
                 area.cf.q75 = quantile(area.prop.cf, 0.75),
                 area.cf.q95 = quantile(area.prop.cf, 0.95),
                 mar.prob.pos = sum(marginal > 0)/.N,
                 mar.prob.neg = sum(marginal < 0)/.N),
               by = c("dist_type", "group.id", "ea_east.bin", "ea_north.bin")]

    geo.sum[[region]]$mar <- merge(mar.geo, dist.lab)

    rm(mar.geo)
    gc()


    # Aggregated model predictions

    message("Disturbance risk …")

    agg.def.geo <- readRDS(file.agg.def.geo)
    agg.deg.geo <- readRDS(file.agg.deg.geo)

    agg.def.geo[, dist_type := factor("def", levels = c("def", "deg"))]
    agg.deg.geo[, dist_type := factor("deg", levels = c("def", "deg"))]

    setnames(agg.def.geo, "deforestation", "disturbance")
    setnames(agg.deg.geo, "degradation", "disturbance")

    agg.post <- rbind(agg.def.geo, agg.deg.geo)
    rm(agg.def.geo, agg.deg.geo)
    gc()
    # mar.post[, `:=`(area.prop.mar = marginal * area)]
    # mar.post[, `:=`(area.prop.mar = marginal * area,
    #                 area.prop.fac = factual * area,
    #                 area.prop.cf = counterfactual * area)]

    agg.geo <-
      agg.post[,
               .(agg.mean = mean(disturbance),
                 agg.median = median(disturbance),
                 agg.sd = sd(disturbance),
                 agg.mad = mad(disturbance),
                 agg.q5 = quantile(disturbance, 0.05),
                 agg.q25 = quantile(disturbance, 0.25),
                 agg.q75 = quantile(disturbance, 0.75),
                 agg.q95 = quantile(disturbance, 0.95)
                 ),
               by = c("dist_type", "ea_east.bin", "ea_north.bin")]

    geo.sum[[region]]$agg <- merge(agg.geo, dist.lab)

    rm(agg.geo)
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
geo.sum.fac.l <- list()
geo.sum.dist.l <- list()
geo.sum.area.l <- list()
geo.sum.cf.l <- list()

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
                      name = "Mapped Indigenous lands",
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
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    labs(x = NULL, y = NULL) +
    map_theme

  if(region == "cam" & hurr_type == "hurr") {
    maps[[region]]$hurr.lf <-
      ggplot() +
      geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", linewidth = 0.3) +
      geom_sf(data = poly[[region]]$limit, fill = "grey95", colour = NA) +
      geom_sf(data = poly[[region]]$hurr_lf, fill = "#377eb8", colour = NA) +
      geom_sf(data = poly[[region]]$bg_is_limit, fill = NA, colour = "grey30", size = 0.3) +
      geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
      scale_fill_manual(values = c.map[1:2],
                        breaks = c("recognized", "not_recognized"),
                        labels = c("Recognized", "Not recognized"),
                        name = "Indigenous lands",
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
                       line_width = 0.5, text_family = "IBMPlexSansCondensed") +
      labs(x = NULL, y = NULL) +
      map_theme 
  }

  # Disturbance loss risk (posterior)

  geo.sum.dist.l[[region]] <-
    geo.sum[[region]]$agg |>
           melt(measure.vars = c("agg.q5", "agg.q25", "agg.median", "agg.q75", "agg.q95"),
                variable.name = "est_type", value.name = "estimate") |>
    merge(agg.est.lab)

  maps[[region]]$dist.post <- 
    geo.sum.dist.l[[region]] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$limit, fill = "grey65", colour = NA) +
    geom_raster(mapping = aes(fill = as.numeric(estimate),
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    # geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.01) +
    scale_fill_continuous_sequential(palette = "Viridis",
                                 rev = FALSE,
                                 limits = c(0, 1),
                                 labels = scales::label_percent()
                                 ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]],
             label_axes = "-NE-") +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    facet_grid(rows = vars(est.label), cols = vars(dist.label2), switch = "y") +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    labs(fill = dist.title, x = NULL, y = NULL) +
    map_theme


  # Absolute change in disturbance (posterior)

  geo.sum.area.l[[region]] <-
    geo.sum[[region]]$mar[n.fac >= 10] |>
           melt(measure.vars = c("area.mar.q5", "area.mar.q25", "area.mar.median", "area.mar.q75", "area.mar.q95"),
                variable.name = "est_type", value.name = "estimate") |>
    merge(area.est.lab)

  maps[[region]]$area.post <- 
    geo.sum.area.l[[region]][n.fac >= 10] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$areas, fill = "grey65", colour = "grey65", size = 0.5) +
    geom_raster(mapping = aes(fill = as.numeric(estimate),
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    # geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.01) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                 mid = 0,
                                 rev = TRUE,
                                 limits = area.lim[[region]],
                                 breaks = area.breaks[[region]],
                                 labels = area.labels[[region]],
                                 oob = scales::squish
                                 ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]],
             label_axes = "-NE-") +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    facet_grid(rows = vars(est.label), cols = vars(dist.label2), switch = "y") +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    labs(fill = mar.title.av, x = NULL, y = NULL) +
    map_theme


  # Underlying pressure (posterior)

  geo.sum.cf.l[[region]] <-
    geo.sum[[region]]$mar[n.fac >= 10] |>
           melt(measure.vars = c("area.cf.q5", "area.cf.q25", "area.cf.median", "area.cf.q75", "area.cf.q95"),
                variable.name = "est_type", value.name = "estimate") |>
    merge(cf.est.lab)

  maps[[region]]$cf.post <- 
    geo.sum.cf.l[[region]][n.fac >= 10] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$areas, fill = "grey65", colour = "grey65", size = 0.5) +
    geom_raster(mapping = aes(fill = as.numeric(estimate),
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    # geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.01) +
    scale_fill_viridis_c(option = "C",
                         limits = area.cf.lim[[region]],
                         breaks = area.cf.breaks[[region]],
                         labels = area.cf.labels[[region]],
                         oob = scales::squish
                         ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]],
             label_axes = "-NE-") +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    facet_grid(rows = vars(est.label), cols = vars(dist.label2), switch = "y") +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    labs(fill = mar.title.av, x = NULL, y = NULL) +
    map_theme



  # Absolute change in long-term disturbance (median)

  maps[[region]]$mar.def <- 
    geo.sum[[region]]$mar[dist_type == "def" & n.fac >= 10] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$areas, fill = "grey65", colour = "grey65", size = 0.5) +
    geom_raster(mapping = aes(fill = as.numeric(area.mar.median),
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    # geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.01) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                 mid = 0,
                                 rev = TRUE,
                                 limits = area.lim[[region]],
                                 breaks = area.breaks[[region]],
                                 labels = area.labels[[region]],
                                 oob = scales::squish
                                 ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    labs(
         subtitle = "Long-term disturbance (deforestation)",
         fill = mar.title.av, x = NULL, y = NULL) +
    map_theme


  # Absolute change in short-term disturbance (median)

  maps[[region]]$mar.deg <- 
    geo.sum[[region]]$mar[dist_type == "deg" & n.fac >= 10] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$areas, fill = "grey65", colour = "grey65", size = 0.5) +
    geom_raster(mapping = aes(fill = as.numeric(area.mar.median),
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    # geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.01) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                 mid = 0,
                                 rev = TRUE,
                                 limits = area.lim[[region]],
                                 breaks = area.breaks[[region]],
                                 labels = area.labels[[region]],
                                 oob = scales::squish
                                 ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    labs(subtitle = "Short-term disturbance (not followed by deforestation)",
         fill = mar.title.av, x = NULL, y = NULL) +
    map_theme



  # Underlying pressure, long-term disturbance (median)

  maps[[region]]$cf.def <- 
    geo.sum[[region]]$mar[dist_type == "def" & n.fac >= 10] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$areas, fill = "grey65", colour = NA) +
    geom_raster(mapping = aes(fill = as.numeric(area.cf.median),
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    # geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.01) +
    # scale_fill_continuous_divergingx(palette = "Roma",
    scale_fill_viridis_c(option = "C",
                         limits = area.cf.lim[[region]],
                         breaks = area.cf.breaks[[region]],
                         labels = area.cf.labels[[region]],
                         oob = scales::squish
                         ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    labs(
         subtitle = "Long-term disturbance (deforestation)",
         fill = cf.title.av, x = NULL, y = NULL) +
    map_theme


  # Underlying pressure, long-term disturbance (median)

  maps[[region]]$cf.deg <- 
    geo.sum[[region]]$mar[dist_type == "deg" & n.fac >= 10] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$areas, fill = "grey65", colour = NA) +
    geom_raster(mapping = aes(fill = as.numeric(area.cf.median),
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    # geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.01) +
    scale_fill_viridis_c(option = "C",
                         limits = area.cf.lim[[region]],
                         breaks = area.cf.breaks[[region]],
                         labels = area.cf.labels[[region]],
                         oob = scales::squish
                         ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    labs(subtitle = "Short-term disturbance (not followed by deforestation)",
         fill = cf.title.av, x = NULL, y = NULL) +
    map_theme

}



## EXPORT (FIGURES) ############################################################

message("Exporting figures …")


maps.dist.c <-
  with(maps,
       ((amz$mar.def + labs(title = "Amazon") +
         theme(plot.margin = margin(b = 20, r = 10))) +
        (amz$mar.deg +
         theme(plot.margin = margin(b = 20, l = 10))) +
        plot_layout(guides = "collect")) /
       ((cam$mar.def + labs(title = "Central America") +
         theme(plot.margin = margin(b = 20, r = 10))) +
        (cam$mar.deg +
         theme(plot.margin = margin(b = 20, l = 10))) +
        plot_layout(guides = "collect")) /
       plot_layout(guides = "keep") 
       )

png(file.fig.maps, width = 7, height = 5.25, unit = "in", res = 600)
maps.dist.c
dev.off()

maps.press.c <-
  with(maps,
       ((amz$cf.def + labs(title = "Amazon") +
         theme(plot.margin = margin(b = 20, r = 10))) +
        (amz$cf.deg +
         theme(plot.margin = margin(b = 20, l = 10))) +
        plot_layout(guides = "collect")) /
       ((cam$cf.def + labs(title = "Central America") +
         theme(plot.margin = margin(b = 20, r = 10))) +
        (cam$cf.deg +
         theme(plot.margin = margin(b = 20, l = 10))) +
        plot_layout(guides = "collect")) /
       plot_layout(guides = "keep") 
       )

png(file.fig.maps.press, width = 7, height = 5.25, unit = "in", res = 600)
maps.press.c
dev.off()

if(hurr_type == "hurr") {

  maps.areas <-
    with(maps,
         ((amz$areas) +
           theme(plot.margin = margin(b = 20, r = 10))) +
          (cam$areas +
           theme(plot.margin = margin(b = 20, l = 10))) +
          plot_layout(guides = "collect"))

  png(file.fig.areas, width = 7, height = 2.5, unit = "in", res = 600)
  print(maps.areas)
  dev.off()

  png(file.fig.dist.amz, width = 7, height = 8.5, unit = "in", res = 600)
  print(maps$amz$dist.post)
  dev.off()

  png(file.fig.press.amz, width = 7, height = 8.5, unit = "in", res = 600)
  print(maps$amz$cf.post)
  dev.off()

  png(file.fig.area.amz, width = 7, height = 8.5, unit = "in", res = 600)
  print(maps$amz$area.post)
  dev.off()

  png(file.fig.hurr, width = 3.5, height = 3.5, unit = "in", res = 600)
  print(maps[[region]]$hurr.lf)
  dev.off()

}

png(file.fig.dist.cam, width = 7, height = 8.5, unit = "in", res = 600)
maps$cam$dist.post
dev.off()

png(file.fig.press.cam, width = 7, height = 8.5, unit = "in", res = 600)
maps$cam$cf.post
dev.off()

png(file.fig.area.cam, width = 7, height = 8.5, unit = "in", res = 600)
maps$cam$area.post
dev.off()



## EXPORT (GEODATA) ############################################################


message("Exporting geodata …")

map.res <- list(amz = 1e4, cam = 5e3)

for(i in seq_along(regions)){

  region <- regions[i]

  mar.reg <- geo.sum[[region]]$mar[, -c("dist.label", "dist.label2")]
  mar.grid <-
    CJ(ea_east.bin = seq(min(mar.reg$ea_east.bin),
                         max(mar.reg$ea_east.bin),
                         map.res[[region]]),
       ea_north.bin = seq(min(mar.reg$ea_north.bin),
                          max(mar.reg$ea_north.bin),
                          map.res[[region]]))

  dist.types <- c("def", "deg")

  for(i in seq_along(dist.types)) {
    rast.mar <-
      mar.reg[dist_type == dist.types[i]
              ][mar.grid, on = c("ea_east.bin", "ea_north.bin")
                ][, .(ea_east.bin, ea_north.bin,
                      area.mar.median, area.mar.q5, area.mar.q95,
                      mar.median, mar.q5, mar.q95)] |>
      st_as_stars(dims = c("ea_east.bin", "ea_north.bin")) |>
      st_set_crs(crs.ea[[region]]) |>
      merge(name = "band") |>
      setNames("marginal")
  write_stars(rast.mar, paste0(path.geo, region, ".", dist.types[i], hurr_suf, suf.geo.mar))
  }

}




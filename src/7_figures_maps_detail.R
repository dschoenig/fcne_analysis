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

hurr_type <- "no_hurr"
# hurr_type <- "hurr"
# overwrite <- TRUE
overwrite <- FALSE

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

file.data.vis <- paste0(path.data.vis, "maps", hurr_suf, ".rds")

file.fig.det.amz <- paste0(path.figures, "maps_detail.amz.png")
file.fig.det.cam <- paste0(path.figures, "maps_detail.cam", hurr_suf, ".png")


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

xy_ratio <- 
  c(amz =  with(map_xlim, amz[2] - amz[1]) / with(map_ylim, amz[2] - amz[1]),
    cam = with(map_xlim, cam[2] - cam[1]) / with(map_ylim, cam[2] - cam[1]))


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
        axis.text = element_text(size = rel(0.7)),
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
        plot.subtitle = element_text(size = rel(0.6),
                                     margin = margin(t = 0, b = 7/2)),
        plot.tag = element_text(
                                size = rel(1),
                                family = "IBMPlexSansCondensed",
                                face = "bold",
                                margin = margin(7/2, 7/4, 7/2, 7)),
        plot.tag.location = "margin"
        )

map_theme2 <-
    map_theme +
    theme(panel.background = element_blank(),
          panel.grid = element_line(colour = "grey25", linewidth = 0.1),
          panel.ontop = TRUE)



map_guide_fill <-
  guides(fill = guide_colorbar(theme = theme(legend.ticks = element_line(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.frame = element_rect(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.text = element_text(hjust = 1),
                                             legend.text.position = "right",
                                             legend.key.width = unit(7.5, "pt"),
                                             legend.key.height = unit(45, "pt"),
                                             legend.ticks.length = unit(2, "pt")),
                               draw.ulim = TRUE,
                               draw.llim = TRUE,
                               order = 1))

map_guide_alpha <-
  guides(alpha = guide_bins(theme = theme(
                                          legend.ticks = element_blank(),
                                          legend.frame = element_blank(),
                                          legend.text = element_text(hjust = 1),
                                          legend.text.position = "right",
                                          legend.key.width = unit(7.5, "pt"),
                                          legend.key.height = unit(7.5, "pt")
                                          ),
                            order = 2))
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


dist.title <- "Disturbance probability"
mar.title.av.l <- "Marginal difference between actual and expected disturbances (km² per pixel)"
mar.title.av.2l <- "Marginal difference between actual and\nexpected disturbances (km² per pixel)"
mar.title.av <- wrap_title(mar.title.av.l)
prob.title.l <- "Posterior probability that marginal difference is < 0 or > 0"
prob.title <- wrap_title(prob.title.l)
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

message("Loading data for visualization …")
stored <- readRDS(file.data.vis)
attach(stored)

## DETAIL MAPS #########################################################




taglet <- toupper(letters)

poi <- data.table(region = c("amz", "amz", "amz",
                             "cam", "cam", "cam"),
                  name = c("ecu", "bol", "xin",
                           "lac", "bos", "pan"),
                  tags = list(
                              taglet[c(1,4,7)],
                              taglet[c(2,5,8)],
                              taglet[c(3,6,9)],
                              taglet[c(1,4,7)],
                              taglet[c(2,5,8)],
                              taglet[c(3,6,9)]
                              ),
                  x = c(-1.5e6, -2.3e5, 8e5,
                        -3.2e5, 2.8e5, 9.75e5),
                  y = c(5.5e5, -6.5e5, -2e5,
                        2.2e5, -2.5e4, -6.5e5),
                  window = c(5e5, 6.5e5, 7e5,
                             1.75e5, 1.85e5, 1.9e5),
                  inloc = c("tl", "tl", "tl",
                            "tl", "tl", "tl"))
poi[, `:=`(xmin = x-window, xmax = x+window, ymin = y-window, ymax = y+window)]
poi[, xy_rat := xy_ratio[region]]
poi[, id := 1:.N]
setkey(poi, id)

poi.bb <- list()
for(i in 1:nrow(poi)) {
poi.bb[[i]] <- 
  poi[i,
      st_bbox(c(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
              crs = crs.ea[[poi[i, region]]])]
}
poi.poly <- lapply(poi.bb, st_as_sfc)

plots.inset <- list()
plots.ten <- list()
plots.poi <- list()
plots.def <- list()
plots.deg <- list()

for(i in 1:nrow(poi)) {

  region <- poi[i, region]
  poi.xlim <- poi[i, c(xmin, xmax)]
  poi.ylim <- poi[i, c(ymin, ymax)]

  breaks.x <- switch(region, amz = seq(-170, 0, 5), cam =  seq(-170, 0, 1))
  breaks.y <- switch(region, amz = seq(-80, 30, 5), cam = seq(-80, 30, 1))

  plots.inset[[i]] <-
      poi[i] |>
      ggplot() +
      geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", linewidth = 0.3) +
      geom_sf(data = poly[[region]]$limit, fill = "grey95", colour = NA) +
      geom_sf(data = poly[[region]]$bg_is_limit, fill = NA, colour = "grey30", size = 0.3) +
      geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
      # geom_point(aes(x = x, y = y), colour = "#e41a1c") +
      geom_sf(data = poi.poly[[i]], colour = NA, fill = "#e41a1c", alpha = 0.65) +
      coord_sf(crs = crs.ea[[region]], expand = FALSE,
               label_axes = "----",
               xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
      # scale_x_continuous(breaks = seq(-170, 0, 10)) +
      # scale_y_continuous(breaks = seq(-80, 30, 10)) +
      labs(x = NULL, y = NULL) +
      map_theme +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(colour = NA, fill = "grey97"),
            plot.background = element_rect(colour = NA, fill = "#FFFFFF"),
            plot.margin = margin(7/3, 7/3, 7/3, 7/3),
            title = element_blank(),
            plot.subtitle = element_blank()
            )


  plots.ten[[i]] <-
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", linewidth = 0.3) +
    geom_sf(data = poly[[region]]$limit, fill = "grey95", colour = NA) +
    geom_sf(data = subset(poly[[region]]$areas, is.na(pa_type)),
            aes(fill = it_type), colour = NA) +
    geom_sf_pattern(data = subset(poly[[region]]$areas, is.na(it_type)),
                    aes(pattern = pa_type), colour = c.map[3], linewidth = 0.2,, fill = NA,
                    pattern_colour = NA, pattern_fill = c.map[3], pattern_spacing = 0.01,
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
    scale_x_continuous(breaks = breaks.x) +
    scale_y_continuous(breaks = breaks.y) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = poi.xlim, ylim = poi.ylim) +
    labs(x = NULL, y = NULL, tag = poi$tags[[i]][1]) +
    map_theme2


  in.w <- 0.35
  in.h <- in.w / poi$xy_rat[i]

  if(poi$inloc[i] == "tl") {
    inloc <- c(-0.01, 1.01-in.h, -0.01+in.w, 1.01)
  }
  if(poi$inloc[i] == "tr") {
    inloc <- c(1.01-in.w, 1.01-in.h, 1.01, 1.01)
  }
  if(poi$inloc[i] == "bl") {
    inloc <- c(-0.01, -0.01, -0.01+in.w, -0.01+in.h)
  }
  if(poi$inloc[i] == "br") {
    inloc <- c(1.01-in.w, -0.01, 1.01, -0.01+in.h)
  }

  plots.poi[[i]] <-
    plots.ten[[i]] +
    inset_element(plots.inset[[i]], inloc[1], inloc[2], inloc[3], inloc[4], align_to = "panel")




  plots.def[[i]] <-
    geo.sum[[region]]$mar[dist_type == "def"] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$areas, fill = "grey90", colour = NA) +
    geom_raster(mapping = aes(fill = as.numeric(area.mar.median),
                              x = ea_east.bin, y = ea_north.bin,
                              alpha = mar.prob.bs),
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
    scale_alpha_binned(limits = c(0.5, 1), range = c(0.1, 1), breaks = c(0.5, 0.75, 0.95, 1)) +
    scale_x_continuous(breaks = breaks.x) +
    scale_y_continuous(breaks = breaks.y) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = poi.xlim, ylim = poi.ylim) +
    map_guide_fill +
    map_guide_alpha +
    labs(
         subtitle = "Long-term disturbance (deforestation)",
         fill = mar.title.av,
         alpha = prob.title,
         x = NULL, y = NULL, tag = poi$tags[[i]][2]) +
    map_theme2


  plots.deg[[i]] <-
    geo.sum[[region]]$mar[dist_type == "deg"] |>
    ggplot() +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$areas, fill = "grey90", colour = NA) +
    geom_raster(mapping = aes(fill = as.numeric(area.mar.median),
                              x = ea_east.bin, y = ea_north.bin,
                              alpha = mar.prob.bs),
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
    scale_alpha_binned(limits = c(0.5, 1), range = c(0.1, 1), breaks = c(0.5, 0.75, 0.95, 1)) +
    scale_x_continuous(breaks = breaks.x) +
    scale_y_continuous(breaks = breaks.y) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = poi.xlim, ylim = poi.ylim) +
    map_guide_fill +
    map_guide_alpha +
    labs(
         subtitle = "Short-term disturbance (not followed by deforestation)",
         fill = mar.title.av,
         alpha = prob.title,
         x = NULL, y = NULL, tag = poi$tags[[i]][3]) +
    map_theme2

}

names(plots.poi) <- poi$name
names(plots.def) <- poi$name
names(plots.deg) <- poi$name


s.poi$lac /
   plots.def$lac /
   plots.deg$lac) +
  plot_layout(guides = "collect")
png("../test.png", width = 4.5, height = 5.75, unit = "in", res = 600)
p.lac
dev.off()

p.des <- "
  123
  456
  789
"

png(file.fig.det.amz, width = 7, height = 6.5, unit = "in", res = 600)
  plots.poi$ecu + plots.poi$bol + plots.poi$xin +
  plots.def$ecu + plots.def$bol + plots.def$xin +
  plots.deg$ecu + plots.deg$bol + plots.deg$xin +
  plot_layout(guides = "collect", design = p.des)
dev.off()

png(file.fig.det.cam, width = 7, height = 6.5, unit = "in", res = 600)
  plots.poi$lac + plots.poi$bos + plots.poi$pan +
  plots.def$lac + plots.def$bos + plots.def$pan +
  plots.deg$lac + plots.deg$bos + plots.deg$pan +
  plot_layout(guides = "collect", design = p.des)
dev.off()

# divergingx_hcl(11, palette = "Roma")



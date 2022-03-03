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
path.figures <- paste0(path.base, "figures/")

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.riskchange.tenure <- paste0(path.effects, region, ".riskchange.tenure.rds")
file.riskchange.tenure_areas <- paste0(path.effects, region, ".riskchange.tenure_areas.rds")
file.riskchange.geo <- paste0(path.effects, region, ".riskchange.geo.rds")
file.risk.geo.all <- paste0(path.effects, region, ".risk.geo.all.rds")

setDTthreads(n.threads)

## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.map <- col.div[c(3,6,17)]
c.plot <- col.div[c(3,17)]

## CRS / LIMITS

crs.ea <- 
  list(cam = st_crs('PROJCS["Central_America_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900002"]]'),
       amz = st_crs('PROJCS["Amazon_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",-5.59],PARAMETER["longitude_of_center",-62.05],PARAMETER["standard_parallel_1",3.81],PARAMETER["standard_parallel_2",-15.62],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900004"]]'))

map_xlim <- list(cam = c(-80e4, 120e4))
map_ylim <- list(cam = c(-90e4, 80e4))


## MAPS ########################################################################

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

anot.adm0 <- data.frame(adm0 = c("BLZ", "CRI", "GTM", "HND",
                                 "MEX", "NIC", "PAN", "SLV"),
                        x = c(0.5, 3, -4.5, 2.75,
                              -4.75, 5, 8.75, -1.5) * 1e5,
                        y = c(3.5, -6.5, -1.5, 1.75,
                              5, -1.5, -5.25, -2.25) * 1e5)


areas.poly <-
  st_read(paste0(path.data, "raw/cam.areas_union.gpkg")) |>
  st_transform(crs.ea[[region]])
areas.poly$it_type <- factor(areas.poly$it_type, levels = c("recognized", "not_recognized"))

limit.poly <-
  st_read(paste0(path.data, "raw/cam.limit.gpkg")) |>
  st_transform(crs.ea[[region]])

map_theme <-  
  theme_minimal(base_family = "IBMPlexSans") +
  theme(panel.background = element_rect(fill = "grey90", colour = NA),
        panel.grid = element_line(colour = "grey75"),
        legend.title = element_text(size = 10),
        # , legend.position = "bottom",
        # , legend.justification = "right"
        )


# Study region: ITs and PAs

areas.map <- 
  ggplot() +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_sf(data = limit.poly, fill = "grey95", colour = NA) +
  geom_sf(data = subset(areas.poly, is.na(pa_type)),
          aes(fill = it_type), colour = NA) +
  geom_sf_pattern(data = subset(areas.poly, is.na(it_type)),
          aes(pattern = pa_type), colour = c.map[3], size = 0.3,, fill = NA,
              pattern_colour = NA, pattern_fill = c.map[3], pattern_spacing = 0.005,
              pattern_density = 0.35) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey30", size = 0.4) +
  geom_text(data = anot.adm0, aes(x = x, y = y, label = adm0), size = 3) +
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
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_x_continuous(breaks = seq(-180, 180, 5)) +
  scale_y_continuous(breaks = seq(-90, 90, 5)) +
  annotation_scale() +
  labs(x = NULL, y = NULL) +
  map_theme +
  theme(legend.position = "right",
        legend.justification = c(0,1)
        , legend.spacing.y = unit(0.1, "cm")
        # , legend.key = element_rect(size = 10, colour = "white")
        # , legend.key.size = unit(0.5, "cm")
        )
areas.map


# Deforestion risk

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

r.geo.all.map <- 
  ggplot(r.geo.all.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = "grey30", size = 0.3) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = FALSE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey30", size = 0.3) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  # scale_fill_viridis_c(limits = c(0,1), option = "D",
  #                      labels = scales::label_percent()) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                   rev = FALSE,
                                   limits = c(0,1),
                                   labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(-180, 180, 5)) +
  scale_y_continuous(breaks = seq(-90, 90, 5)) +
  labs(fill = "Forest loss risk", x = NULL, y = NULL) + 
  guides(fill = guide_colorbar(ticks.colour = "grey35",
                               ticks.linewidth = 1,
                               frame.colour = "grey35",
                               frame.linewidth = 1,
                               barheight = 7.5,
                               label.position = "left",
                               label.hjust = 1,
                               draw.ulim = FALSE,
                               draw.llim = FALSE)) +
  map_theme +
  theme(legend.position = "right",
        legend.justification = c(0,1),
        legend.spacing.y = unit(5, "mm"))
r.geo.all.map


# Absolute change in risk (compared to baseline)

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


arc.geo.all.map <- 
  ggplot(arc.geo.all.sum) +
  geom_sf(data = bg_adm0, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = FALSE) +
  geom_sf(data = bg_adm0, fill = NA, colour = "grey30", size = 0.3) +
  coord_sf(crs = crs.ea$cam, expand = FALSE, 
           xlim = map_xlim$cam, ylim = map_ylim$cam) +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,rev = TRUE,
                                   ,breaks = seq(-0.25, 0.25, 0.05),
                                   ,labels = c("≤ -25%", "-20%", "", "-10%", "", 0,
                                               "", "+10%", "", "+20%", "≥ +25%"),
                                   ,limits = c(-0.25, 0.25)
                                   ,oob = scales::squish
                                   ) +
  scale_x_continuous(breaks = seq(-180, 180, 5)) +
  scale_y_continuous(breaks = seq(-90, 90, 5)) +
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
arc.geo.all.map

maps.combined <-
areas.map / r.geo.all.map / arc.geo.all.map +
plot_annotation(tag_levels = list(c("B", "D", "F")))

tiff(paste0(path.figures, "fig1.tif"), width = 6.25, height = 10.5, unit = "in", res = 300)
maps.combined
dev.off()


## TENURE CATEGORIES ########################################################### 

# Tenure category by administrative areas

rc.ten <- readRDS(file.riskchange.tenure)

arc.ten.sum <- summarize_draws(rc.ten$arc,
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.25, 0.75, 0.025, 0.975)))|>
               as.data.table() |>
               setnames("variable", "group.label") |>
               merge(rc.ten$groups[, !"ids"],
                     by = "group.label")


cat.lab <- 
  data.table(cat.label = c("Reference\n(non-IT, non-PA)",
                            "IT, recognized", "IT, not recognized",
                            "PA, indirect use", "PA, direct use",
                            "IT, recognized &\nPA, indirect use",
                            "IT, recognized &\nPA, direct use",
                            "IT, not recognized &\nPA, indirect use",
                            "IT, not recognized &\nPA, direct use"),
             it_type = c("none",
                         "recognized", "not_recognized",
                         NA, NA,
                         "recognized", "recognized",
                         "not_recognized", "not_recognized"),
             pa_type = c("none",
                         NA, NA,
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use"))
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]

reg.lab <-
  data.table(adm0 = c("BLZ", "CRI", "GTM", "HND",
                      "MEX", "NIC", "PAN", "SLV",
                      NA),
             reg.label = c("Belize", "Costa Rica", "Guatemala", "Honduras",
                           "Mexico", "Nicaragua", "Panama", "El Salvador",
                           "Study region")
             )
reg.lab[, reg.label := factor(reg.label, levels = reg.label)]

ten.cat <-
  expand.grid(cat.label = cat.lab$cat.label,
              reg.label = reg.lab$reg.label) |>
  as.data.table() |>
  merge(cat.lab, by = "cat.label", all = TRUE) |>
  merge(reg.lab, by = "reg.label", all = TRUE) |>
  merge(arc.ten.sum[n >= 1000], by = c("it_type", "pa_type", "adm0"), all.x = TRUE)

arc.ten.plot <-
ggplot(ten.cat) +
  geom_tile(aes(x = reg.label, y = cat.label, fill = mean), size = 1, colour = "grey95") +
  # geom_text(aes(x = reg.label, y = cat.label, label = round(mean*100, 2)), size = 2.5) +
  geom_segment(x = 8.5, y = 0.5, xend = 8.5, yend = 9.5, size = 0.2, colour = "grey35") +
  geom_segment(x = 0.5, y = 8.5, xend = 9.5, yend = 8.5, size = 0.2, colour = "grey35") +
  # geom_segment(x = 0.5, y = 6.5, xend = 9.5, yend = 6.5, size = 1, colour = "grey50") +
  # geom_segment(x = 0.5, y = 4.5, xend = 9.5, yend = 4.5, size = 1, colour = "grey50") +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,rev = TRUE,
                                   ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                   ,labels = label_arc
                                   # ,labels = c(paste0(seq(-15, 0, 5), "%"),
                                   #             paste0("+", seq(5, 15, 5), "%"))
                                   #             # "+5%", "≥ +10%"),
                                   ,limits = c(-0.15, 0.15)
                                   # ,oob = scales::squish,
                                   ,na.value = "grey75"
                                   ) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev) +
  coord_fixed() +
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
  theme_minimal(base_family = "IBMPlexSans") +
  theme(
        legend.position = "right",
        legend.justification = c(0,1),
        legend.spacing.y = unit(5, "mm"),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0,
                                   colour = "grey5"),
        axis.text.y = element_text(hjust = 0, colour = "grey5"),
        # panel.background = element_rect(fill = "grey85", colour = "grey85", size = 2),
        panel.grid.major = element_blank()
        # axis.line = element_line(colour = "grey85")
  )

  arc.ten.plot


# Distribution of areas per tenure category

rc.ten_areas <- readRDS(file.riskchange.tenure_areas)


types <- c("it_type == 'recognized'",
           "it_type == 'not_recognized'",
           "pa_type == 'direct_use'",
           "pa_type == 'indirect_use'")
names(types) <- c("it.rec", "it.notrec", "pa.ind", "pa.dir")

densities <- list()
for(i in seq_along(types)) {
  selg <- rc.ten_areas$groups[eval(parse(text = types[i])), area.label]

  x <- seq(-1, 1, length.out = 512)

  dens <- apply(rc.ten_areas$arc[,selg], 1,
                \(draw) density(draw, from = min(x), to = max(x), n = length(x), bw = "nrd")$y)
  row.names(dens) <- 1:nrow(dens)

  dens <- as_draws_matrix(t(dens))
  dens.sum <-
    summary(dens, mean, \(x) quantile2(x, c(0.025, 0.975))) |>
    as.data.table() |>
    setnames("variable", "point")
  dens.sum[, point := as.integer(as.character(point))]

  densities[[i]] <- 
    merge(dens.sum,
          data.table(point = 1:length(x),
                     arc = x),
          by = "point")
}
names(densities) <- names(types)

densities <- rbindlist(densities, idcol = "type")

arc.ten_areas.plot <-
  ggplot(densities, aes(x = arc, y = mean, ymin = q2.5, ymax = q97.5)) +
  geom_ribbon(aes(fill = type), alpha = 0.2, show.legend = FALSE) +
  geom_line(aes(col = type, linetype = type)) +
  scale_colour_manual(aesthetics = c("colour", "fill"),
                      values = c.plot[c(1, 1, 2, 2)],
                      breaks = c("it.rec", "it.notrec", "pa.ind", "pa.dir"),
                      labels = c("IT, recognized", "IT, not recognized",
                                 "PA, indirect use", "PA, direct use")) +
  scale_linetype_manual(values = c(it.rec = "solid", 
                                   it.notrec = "dashed",
                                   pa.ind = "solid",
                                   pa.dir = "dashed"),
                        labels = c("IT, recognized", "IT, not recognized",
                                   "PA, indirect use", "PA, direct use")) +
  geom_vline(xintercept = 0) +
  scale_y_continuous(limits = c(0, max(densities$q97.5)),
                     breaks = 0, labels = NULL) +
  scale_x_continuous(limits = c(-0.3, 0.4), labels = label_arc) +
  labs(y = "Distribution of\nindividual areas (density)",
       x = "Absolute change in forest loss risk",
       fill = NULL, colour = NULL, linetype = NULL) +
  theme_minimal(base_family = "IBMPlexSans") +
  theme(
        legend.position = "right",
        legend.justification = c(0,1),
        # legend.spacing.y = unit(5, "mm"),
  )

plots.combined <-
  arc.ten.plot /
  (arc.ten_areas.plot
   + 
   theme(axis.title.y = element_text(size = 10,
                                     hjust = 0,
                                     margin = margin(r = - 125, unit = "pt")),
         axis.title.x = element_text(size = 10,
                                     hjust = 0.5,
                                     margin = margin(t = 10, unit = "pt")))
   ) +
  plot_layout(ncol = 1, nrow = 3, heights = c(1.25,0.75,1), widths = 1) +
  plot_annotation(tag_levels = list(c("B", "D")))

tiff(paste0(path.figures, "fig2.tif"), width = 6.25, height = 10.5, unit = "in", res = 300)
plots.combined
dev.off()


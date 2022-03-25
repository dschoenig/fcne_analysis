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

region <- "amz"
# n.threads <- 4

path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
path.figures <- paste0(path.base, "figures/")

regions <- c("amz", "cam")



## CRS / LIMITS

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

bg_coasts <- st_union(bg_adm0)

map_theme <-  
  theme_minimal(base_family = "IBMPlexSans") +
  theme(panel.background = element_rect(fill = "grey90", colour = NA),
        panel.grid = element_line(colour = "grey75"),
        legend.title = element_text(size = 10),
        # , legend.position = "bottom",
        # , legend.justification = "right"
        )


## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.map <- col.div[c(3,6,17)]
c.plot <- col.div[c(3,17)]

# xmin: -1917415 ymin: -1653909 xmax: 1928499 ymax: 1581534

# Labels for tenure categories and administrative regions

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

reg.lab <- list()



reg.lab$amz <-
  data.table(adm0 = c("BOL", "BRA", "COL", "ECU",
                      "GUF", "GUY", "PER", "SUR",
                      "VEN", NA),
             reg.label = c("Bolivia", "Brazil", "Colombia", "Ecuador",
                           "French Guiana", "Guyana", "Peru", "Suriname",
                           "Venezuela", "Amazon (region)")
             )
reg.lab$cam <-
  data.table(adm0 = c("BLZ", "CRI", "GTM", "HND",
                      "MEX", "NIC", "PAN", "SLV",
                      NA),
             reg.label = c("Belize", "Costa Rica", "Guatemala", "Honduras",
                           "Mexico", "Nicaragua", "Panama", "El Salvador",
                           "Central America (region)")
             )
reg.lab$amz[, reg.label := factor(reg.label, levels = reg.label)]
reg.lab$cam[, reg.label := factor(reg.label, levels = reg.label)]



## MAPS ########################################################################


poly <- list()

geo.sum <- list()

ten.sum <- list()

maps <- list()

plots <- list()

for(i in seq_along(regions)){

region <- regions[i]

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.riskchange.tenure <- paste0(path.effects, region, ".riskchange.tenure.rds")
file.riskchange.tenure_areas <- paste0(path.effects, region, ".riskchange.tenure_areas.rds")
file.riskchange.geo <- paste0(path.effects, region, ".riskchange.geo.rds")
file.risk.geo.all <- paste0(path.effects, region, ".risk.geo.all.rds")
file.limit <- paste0(path.data, "raw/", region, ".limit.gpkg")
file.areas <- paste0(path.data, "raw/", region, ".areas_union.gpkg")


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
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey30", size = 0.4) +
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
  scale_x_continuous(breaks = seq(-170, 0, 5)) +
  scale_y_continuous(breaks = seq(-80, 30, 5)) +
  annotation_scale(width_hint = 0.125, style = "ticks") +
  labs(x = NULL, y = NULL) +
  map_theme +
  theme(legend.position = "right",
        legend.justification = c(0,1)
        , legend.spacing.y = unit(0.1, "cm")
        # , legend.key = element_rect(size = 10, colour = "white")
        # , legend.key.size = unit(0.5, "cm")
        )
maps[[region]]$areas


# Deforestion risk

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
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey30", size = 0.4) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                   rev = FALSE,
                                   limits = c(0,1),
                                   labels = scales::label_percent()) +
                                   # ) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
  scale_x_continuous(breaks = seq(-170, 0, 5)) +
  scale_y_continuous(breaks = seq(-80, 30, 5)) +
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
maps[[region]]$risk



# Absolute change in risk (compared to baseline)

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
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey30", size = 0.4) +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,rev = TRUE,
                                   ,breaks = seq(-0.25, 0.25, 0.05),
                                   ,labels = c("≤ -25%", "-20%", "", "-10%", "", 0,
                                               "", "+10%", "", "+20%", "≥ +25%"),
                                   # ,limits = c(-0.25, 0.25)
                                   ,limits = c(-0.20, 0.20)
                                   ,oob = scales::squish
                                   ) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
  scale_x_continuous(breaks = seq(-170, 0, 5)) +
  scale_y_continuous(breaks = seq(-80, 30, 5)) +
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
maps[[region]]$arc




## TENURE CATEGORIES ########################################################### 

# Tenure category by administrative areas

rc.ten <- readRDS(file.riskchange.tenure)

arc.ten.sum <-
  summarize_draws(rc.ten$arc,
                                mean, 
                                sd,
                                \(x) quantile2(x, c(0.025, 0.975)))|>
               as.data.table() |>
               setnames("variable", "group.label") |>
               merge(rc.ten$groups[, !"ids"],
                     by = "group.label")

rm(rc.ten)


ten.sum[[region]]$arc <-
  expand.grid(cat.label = cat.lab$cat.label,
              reg.label = reg.lab[[region]]$reg.label) |>
  as.data.table() |>
  merge(cat.lab, by = "cat.label", all = TRUE) |>
  merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
  merge(arc.ten.sum[n >= 1000], by = c("it_type", "pa_type", "adm0"), all.x = TRUE)

sep.x <- nrow(reg.lab[[region]]) - 0.5

plots[[region]]$ten.arc <-
  ggplot(ten.sum[[region]]$arc[it_type != "none" | pa_type != "none"]) +
    geom_tile(aes(x = reg.label, y = cat.label, fill = mean), size = 1, colour = "white") +
    geom_text(aes(x = reg.label, y = cat.label, label = round(mean*100, 1)), size = 2.5) +
    geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 8.5,
                 size = 0.2, colour = "grey35") +
    # geom_segment(x = 0.5, y = 8.5, xend = 9.5, yend = 8.5, size = 0.2, colour = "grey35") +
    # geom_segment(x = 0.5, y = 6.5, xend = 9.5, yend = 6.5, size = 1, colour = "grey50") +
    # geom_segment(x = 0.5, y = 4.5, xend = 9.5, yend = 4.5, size = 1, colour = "grey50") +
    scale_fill_continuous_divergingx(
                                    # palette = "Blue-Red 3",
                                    palette = "Roma"
                                     ,rev = TRUE,
                                     ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                     ,labels = label_arc
                                     # ,labels = c(paste0(seq(-15, 0, 5), "%"),
                                     #             paste0("+", seq(5, 15, 5), "%"))
                                     #             # "+5%", "≥ +10%"),
                                     # ,limits = c(-0.075, 0.075)
                                     ,limits = c(-0.10, 0.10)
                                     ,oob = scales::squish
                                     ,na.value = "grey95"
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

plots[[region]]$ten.arc


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

  x <- seq(-1, 1, by = 0.001)

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


ten.sum[[region]]$arc.dens <- rbindlist(densities, idcol = "type")

# sfun <- densities[type == "pa.ind", approxfun(arc, cumsum(0.001 * q2.5))]

# sfun(0)

plots[[region]]$ten.arc.dens <-
  ggplot(ten.sum[[region]]$arc.dens,
         aes(x = arc, y = mean, ymin = q2.5, ymax = q97.5)) +
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
  # scale_y_continuous(limits = c(0, max(densities$q97.5))
  #                    # , breaks = 0, labels = NULL) +
  #                    ) +
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

# tiff(paste0(path.figures, "fig2.tif"), width = 6.25, height = 10.5, unit = "in", res = 300)
# plots.combined
# dev.off()

}

saveRDS(ten.sum, "tenure.summary.rds")
saveRDS(geo.sum, "geo.summary.rds")

maps.combined <-
  with(maps,
       (amz$areas + cam$areas + plot_layout(guides = "collect")) / 
       (amz$risk + cam$risk + plot_layout(guides = "collect")) /
       (amz$arc + cam$arc + plot_layout(guides = "collect")) +
       plot_annotation(tag_levels = list(c("A", "B", "C", "D", "E", "F")))
       )

maps.combined

plots.combined <-
  with(plots,
       amz$ten.arc +
       (amz$ten.arc.dens +
        theme(axis.title.y = element_text(size = 10,
                                             hjust = 0,
                                             margin = margin(r = - 125, unit = "pt")),
                 axis.title.x = element_text(size = 10,
                                             hjust = 0.5,
                                             margin = margin(t = 10, unit = "pt")))
        ) +
       cam$ten.arc +
       (cam$ten.arc.dens +
        theme(axis.title.y = element_text(size = 10,
                                             hjust = 0,
                                             margin = margin(r = - 125, unit = "pt")),
                 axis.title.x = element_text(size = 10,
                                             hjust = 0.5,
                                             margin = margin(t = 10, unit = "pt")))
        ) +
       plot_layout(ncol = 2, nrow = 2, heights = c(1.25,0.5), byrow = FALSE,
                   guides = "collect")
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


# tiff(paste0(path.figures, "fig1.tif"), width = 6.25, height = 10.5, unit = "in", res = 300)
# maps.combined
# dev.off()


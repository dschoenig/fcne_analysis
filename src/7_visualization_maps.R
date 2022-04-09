library(data.table)
library(posterior)
library(sf)
library(stars)
library(ggplot2)
library(ggdist)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)

source("utilities.R")

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.effects <- paste0(path.base, "models/gam/effects/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.geo <- paste0(path.base, "results/geo/")
if(!dir.exists(path.geo)) dir.create(path.geo, recursive = TRUE)

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

# Resolution (for covariate maps only)
map.res <- list(amz = 5000, cam = 2500)

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
  theme_minimal(base_family = "IBMPlexSans", base_size = 7) +
  theme(panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid = element_line(colour = "grey75"),
        legend.position = "right",
        legend.justification = c(0,1),
        legend.title = element_text(size = rel(0.9), hjust = 0,
                                    margin = margin(t = 3, b = 6)),
        legend.text = element_text(size = rel(0.8)),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(12, "pt"),
        strip.text = element_text(size = rel(1), hjust = 0,
                                  margin = margin(b = 3, t = 6))
        )

map_guide_fill <-
  guides(fill = guide_colorbar(
                               ticks.colour = "grey5",
                               ticks.linewidth = 1,
                               frame.colour = "grey5",
                               frame.linewidth = 0.5,
                               barwidth = 0.5,
                               barheight = 5,
                               label.position = "left",
                               label.hjust = 1,
                               draw.ulim = FALSE,
                               draw.llim = FALSE
                               ))

# map_guide_fill_bottom <-
#   guides(fill = guide_colorbar(
#                                ticks.colour = "grey5",
#                                ticks.linewidth = 1,
#                                frame.colour = "grey5",
#                                frame.linewidth = 0.5,
#                                barwidth = 5,
#                                barheight = 0.5,
#                                label.position = "bottom",
#                                label.hjust = 0.5,
#                                draw.ulim = FALSE,
#                                draw.llim = FALSE
#                                ))

cov.scales <-
  list(amz = 
         list(tri = list(title = "Terrain ruggedness index",
                         trans = scales::yj_trans(0),
                         breaks = c(0, 3, 10, 30, 100),
                         limits = c(0, 100),
                         labels = scales::label_comma(big.mark =" ", accuracy = 0.1)),
              dist_set.km = list(
                                 title = "Distance to settlements (km)",
                                 trans = scales::identity_trans(),
                                 breaks = seq(0, 3e2, 1e2),
                                 limits = c(0, 3e2),
                                 labels = scales::label_comma(big.mark =" ")),
              dist_roads.km = list(
                                   title = "Distance to roads (km)",
                                   trans = scales::identity_trans(),
                                   breaks = seq(0, 2.5e2, 5e1),
                                   limits = c(0, 2.25e2),
                                   labels = scales::label_comma(big.mark =" ")),
              dist_rivers.km = list(
                                    title = "Distance to rivers (km)",
                                    trans = scales::identity_trans(),
                                    breaks = seq(0, 10, 2.5),
                                    limits = c(0, 10),
                                    labels = scales::label_comma(big.mark =" ")),
              dens_pop = list(
                              title = "Population density (individuals / km²)",
                              trans = scales::yj_trans(0),
                              breaks = c(0, 10^(1:4)),
                              limits = c(0, 1e4),
                              labels = scales::label_comma(big.mark =" ")),
              dens_roads = list(
                                title = "Road density (m / km²)",
                                trans = scales::yj_trans(0),
                                breaks = c(0, 10^(1:4)),
                                limits = c(0, 1.75e3),
                                labels = scales::label_comma(big.mark =" "))
              ),
       cam = 
         list(tri = list(title = "Terrain ruggedness index",
                         trans = scales::yj_trans(0),
                         breaks = c(0, 3, 10, 30),
                         limits = c(0, 30),
                         labels = scales::label_comma(big.mark =" ", accuracy = 0.1)),
              dist_set.km = list(
                                 title = "Distance to settlements (km)",
                                 trans = scales::identity_trans(),
                                 breaks = seq(0, 100, 20),
                                 limits = c(0, 85),
                                 labels = scales::label_comma(big.mark =" ")),
              dist_roads.km = list(
                                   title = "Distance to roads (km)",
                                   trans = scales::identity_trans(),
                                   breaks = seq(0, 80, 20),
                                   limits = c(0, 80),
                                   labels = scales::label_comma(big.mark =" ")),
              dist_rivers.km = list(
                                    title = "Distance to rivers (km)",
                                    trans = scales::identity_trans(),
                                    breaks = seq(0, 10, 2.5),
                                    limits = c(0, 10),
                                    labels = scales::label_comma(big.mark =" ")),
              dens_pop = list(
                              title = "Population density (individuals / km²)",
                              trans = scales::yj_trans(0),
                              breaks = c(0, 10^(1:5)),
                              limits = c(0, 1e5),
                              labels = scales::label_comma(big.mark =" ")),
              dens_roads = list(
                                title = "Road density (m / km²)",
                                trans = scales::yj_trans(0),
                                breaks = c(0, 10^(1:5)),
                                limits = c(0, 3e5),
                                labels = scales::label_comma(big.mark =" "))
              )
       )

cat.lab <- 
  data.table(cat.label = c("All forests", "Primary forests",
                           "IT, recognized", "IT, not recognized",
                           "PA, indirect use", "PA, direct use"),
             it_type = c(NA, NA,
                         "recognized", "not_recognized",
                         NA, NA),
             pa_type = c(NA, NA,
                         NA, NA,
                         "indirect_use", "direct_use"),
             for_type = c(NA, "primary",
                          NA, NA,
                          NA, NA))
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]

title.wrap <- scales::label_wrap(20)


## EFFECTS IN GEOGRAPHICAL SPACE ###############################################

if(!file.exists(file.data.vis)) {
  
  poly <- list()
  geo.sum <- list()

  # Prepare data for maps

  for(i in seq_along(regions)){
      
    region <- regions[i]

    message(paste0("Preparing data for region `", region, "` …"))

    file.data.proc <- paste0(path.data.proc, region, ".data.fit.proc.rds")
    file.risk.geo.all <- paste0(path.effects, region, ".risk.geo.all.rds")
    file.riskchange.geo <- paste0(path.effects, region, ".riskchange.geo.rds")
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


    # Covariate means per raster cell 
    
    message("Covariates …")

    data.proc <- readRDS(file.data.proc)
    
    res <- map.res[[region]]
    map.anchor <- c(ea_east = floor(min(data.proc$ea_east / res)) * res,
                    ea_north = floor(min(data.proc$ea_north / res)) * res)
    data.proc.bin <- bin_cols(data.proc, c("ea_east", "ea_north"), rep(res, 2), append = TRUE)
    cov.names <- c("dist_set", "dist_roads", "dist_rivers", "tri", "dens_roads", "dens_pop")
    cov.trans.km <- c("dist_set", "dist_roads", "dist_rivers")

    geo.sum[[region]]$cov  <-
      data.proc.bin[,
                    lapply(.SD, mean),
                    by = c("ea_east.bin", "ea_north.bin"),
                    .SDcols = cov.names]
    geo.sum[[region]]$cov[,
                          (paste0(cov.trans.km, ".km")) :=
                          lapply(.SD, \(x) x/1000),
                          .SDcols = cov.trans.km]

    message("No. of samples …")

    sum.vars <- c("for_type", "it_type", "pa_type")

    n.sum <- list()

    for(i in seq_along(sum.vars)) {
      # var.sel <- paste0("is.na(", sum.vars[i], ")")
      n.sum[[i]] <- 
        data.proc.bin[,
                      .(n = .N),
                      by = c(sum.vars[i], "ea_east.bin", "ea_north.bin")]
    }
    
    n.sum[[length(sum.vars) + 1]] <-
        data.proc.bin[,
                      .(n = .N),
                      by = c("ea_east.bin", "ea_north.bin")]

    geo.sum[[region]]$n <-
      rbindlist(n.sum, fill = TRUE) |>
      subset((it_type != "none" | is.na(it_type)) &
             (pa_type != "none" | is.na(pa_type)) &
             (for_type != "other" | is.na(for_type))) |>
      setcolorder(c("for_type", "it_type", "pa_type", "ea_east.bin", "ea_north.bin")) |>
      merge(cat.lab, by = c("for_type", "it_type", "pa_type"),
            all.x = TRUE, all.y = FALSE, sort = FALSE) |>
      setorder("cat.label", "ea_east.bin", "ea_north.bin")

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
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$limit, fill = "grey95", colour = NA) +
    geom_sf(data = subset(poly[[region]]$areas, is.na(pa_type)),
            aes(fill = it_type), colour = NA) +
    geom_sf_pattern(data = subset(poly[[region]]$areas, is.na(it_type)),
            aes(pattern = pa_type), colour = c.map[3], size = 0.3,, fill = NA,
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
                               labels = c("Indirect use", "Direct use"),
                               name = "Protected areas",
                               guide = guide_legend(override.aes = list(pattern_spacing = 0.01,
                                                                        size = 1),
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

  # Risk

  maps[[region]]$risk <- 
    ggplot(geo.sum[[region]]$r) +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
    geom_raster(mapping = aes(
                              fill = mean,
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_continuous_sequential(palette = "Viridis",
                                      rev = FALSE,
                                      limits = c(0,1),
                                      labels = scales::label_percent()) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSans") +
    map_guide_fill +
    labs(fill = "Forest loss risk", x = NULL, y = NULL) +
    map_theme


  # Absolute risk change

  maps[[region]]$arc <- 
    ggplot(geo.sum[[region]]$arc) +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_raster(mapping = aes(
                              fill = mean,
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg_is_limit, fill = NA, colour = "grey30", size = 0.3) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                     ,mid = 0
                                     ,rev = TRUE,
                                     ,breaks = seq(-0.2, 0.2, 0.05),
                                     ,labels = c("≤ -20%", "", "-10%", "", 0,
                                                 "", "+10%", "", "≥ +20%"),
                                     ,limits = c(-0.20, 0.20)
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
    labs(fill = "Absolute change in\nforest loss risk", x = NULL, y = NULL) +
    map_theme


    # Covariate maps
   
    cov.scale <- cov.scales[[region]]
    covariates <- names(cov.scale)

    for(j in seq_along(covariates)) {
      maps[[region]]$cov[[covariates[j]]] <-
      melt(geo.sum[[region]]$cov,
           id.vars = c("ea_east.bin", "ea_north.bin"),
           variable.name = "covariate") |>
      subset(covariate == covariates[j]) |>
      ggplot() + 
        geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
        geom_raster(mapping = aes(
                                  fill = value,
                                  x = ea_east.bin, y = ea_north.bin),
                    interpolate = FALSE) +
        geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.3) +
        geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
        scale_fill_continuous_sequential(palette = "Viridis"
                                         ,rev = FALSE,
                                         ,trans = cov.scale[[covariates[j]]]$trans
                                         ,breaks = cov.scale[[covariates[j]]]$breaks
                                         ,limits = cov.scale[[covariates[j]]]$limits
                                         ,labels = cov.scale[[covariates[j]]]$labels
                                         ,name = title.wrap(cov.scale[[covariates[j]]]$title)
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
        labs(x = NULL, y = NULL) +
        map_theme
    }


    # No of samples 

    n.step <- switch(region, amz = 25, cam = 50)
    n.limits <- 
      with(geo.sum[[region]]$n, c(0, ceiling(max(n) / n.step) * n.step))
           
    maps[[region]]$n <-
      geo.sum[[region]]$n[is.na(it_type) & is.na(pa_type)] |>
      ggplot() +
      geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
      geom_raster(mapping = aes(fill = n,
                                x = ea_east.bin, y = ea_north.bin),
                  interpolate = FALSE) +
      geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.3) +
      geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
      scale_fill_continuous_sequential(palette = "Viridis",
                                       rev = FALSE,
                                       limits = n.limits,
                                       breaks = scales::breaks_width(n.step)) +
      coord_sf(crs = crs.ea[[region]], expand = FALSE, 
               xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
      scale_x_continuous(breaks = seq(-170, 0, 10)) +
      scale_y_continuous(breaks = seq(-80, 30, 10)) +
      annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                       style = "ticks", text_cex = 0.5,
                       line_width = 0.5, text_family = "IBMPlexSans") +
      map_guide_fill +
      # labs(fill = expression(N~km^{-2}), x = NULL, y = NULL) +
      labs(fill = "No. of samples", x = NULL, y = NULL) +
      facet_wrap(vars(cat.label), ncol = 2) +
      map_theme

}


## MAPS (0.025 QUANTILE) #######################################################

maps.ci_l <- list()

for(i in seq_along(regions)) {
    
  region <- regions[i]

  message(paste0("Preparing maps for region `", region, "` (0.025 quantile) …"))

  # Risk

  maps.ci_l[[region]]$risk <- 
    ggplot(geo.sum[[region]]$r) +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
    geom_raster(mapping = aes(
                              fill = q2.5,
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_continuous_sequential(palette = "Viridis",
                                      rev = FALSE,
                                      limits = c(0,1),
                                      labels = scales::label_percent()) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSans") +
    map_guide_fill +
    labs(fill = "Forest loss risk", x = NULL, y = NULL) +
    map_theme


  # Absolute risk change

  maps.ci_l[[region]]$arc <- 
    ggplot(geo.sum[[region]]$arc) +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_raster(mapping = aes(
                              fill = q2.5,
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg_is_limit, fill = NA, colour = "grey30", size = 0.3) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                     ,mid = 0
                                     ,rev = TRUE,
                                     ,breaks = seq(-0.2, 0.2, 0.05),
                                     ,labels = c("≤ -20%", "", "-10%", "", 0,
                                                 "", "+10%", "", "≥ +20%"),
                                     ,limits = c(-0.20, 0.20)
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
    labs(fill = "Absolute change in\nforest loss risk", x = NULL, y = NULL) +
    map_theme
}


## MAPS (0.975 QUANTILE) #######################################################

maps.ci_u <- list()

for(i in seq_along(regions)) {
    
  region <- regions[i]

  message(paste0("Preparing maps for region `", region, "` (0.975 quantile) …"))

  # Risk

  maps.ci_u[[region]]$risk <- 
    ggplot(geo.sum[[region]]$r) +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
    geom_raster(mapping = aes(
                              fill = q97.5,
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg, fill = NA, colour = "grey50", size = 0.3) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_continuous_sequential(palette = "Viridis",
                                      rev = FALSE,
                                      limits = c(0,1),
                                      labels = scales::label_percent()) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-170, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSans") +
    map_guide_fill +
    labs(fill = "Forest loss risk", x = NULL, y = NULL) +
    map_theme


  # Absolute risk change

  maps.ci_u[[region]]$arc <- 
    ggplot(geo.sum[[region]]$arc) +
    geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_raster(mapping = aes(
                              fill = q97.5,
                              x = ea_east.bin, y = ea_north.bin),
                interpolate = FALSE) +
    geom_sf(data = poly[[region]]$bg_is_limit, fill = NA, colour = "grey30", size = 0.3) +
    geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                     ,mid = 0
                                     ,rev = TRUE,
                                     ,breaks = seq(-0.2, 0.2, 0.05),
                                     ,labels = c("≤ -20%", "", "-10%", "", 0,
                                                 "", "+10%", "", "≥ +20%"),
                                     ,limits = c(-0.20, 0.20)
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
    labs(fill = "Absolute change in\nforest loss risk", x = NULL, y = NULL) +
    map_theme
}



maps.combined <-
  with(maps,
       (amz$areas + cam$areas + plot_layout(guides = "collect")) / 
       (amz$risk + cam$risk + plot_layout(guides = "collect")) /
       (amz$arc + cam$arc + plot_layout(guides = "collect")) +
       plot_annotation(tag_levels = list(c("A", "", "B", "", "C", ""))) &
       map_theme
       )

tiff(paste0(path.figures, "maps.tif"), width = 6.7, height = 7.5, unit = "in", res = 300)
maps.combined
dev.off()


maps.risk.ci.combined <-
  with(maps.ci_l,
       (amz$risk + cam$risk)) /
  with(maps.ci_u,
       (amz$risk + cam$risk)) +
  plot_annotation(tag_levels = list(c("A", "", "B", ""))) +
  plot_layout(guides = "collect") &
  map_theme

tiff(paste0(path.figures, "si.maps.risk.ci.tif"), width = 6.7, height = 4.85, unit = "in", res = 300)
maps.risk.ci.combined
dev.off()


maps.arc.ci.combined <-
  with(maps.ci_l,
       (amz$arc + cam$arc)) /
  with(maps.ci_u,
       (amz$arc + cam$arc)) +
  plot_annotation(tag_levels = list(c("A", "", "B", ""))) +
  plot_layout(guides = "collect") &
  map_theme

tiff(paste0(path.figures, "si.maps.arc.ci.tif"), width = 6.7, height = 4.85, unit = "in", res = 300)
maps.arc.ci.combined
dev.off()


for(i in seq_along(regions)) {
  region <- regions[i]

  maps.cov.combined <-
    with(maps[[region]]$cov,
         tri + dist_set.km + dist_roads.km + dist_rivers.km + dens_pop + dens_roads +
         plot_layout(ncol = 2, nrow = 3) +
         plot_annotation(tag_levels = "A") &
         map_theme)

  tiff(paste0(path.figures, "si.maps.cov.", region, ".tif"),
       width = 6.7, height = 7.5, unit = "in", res = 300)
  print(maps.cov.combined)
  dev.off()

}


maps.n.combined <-
  with(maps,
       (amz$n / cam$n)) +
  plot_annotation(tag_levels = "A") +
  map_theme
  

tiff(paste0(path.figures, "si.maps.n.tif"), width = 6.7, height = 6, unit = "in", res = 300)
maps.n.combined
dev.off()



for(i in seq_along(regions)){
  region <- regions[i]

  rast.r <-
    geo.sum[[region]]$r[, .(ea_east.bin, ea_north.bin, mean, sd, q2.5, q97.5)] |>
    st_as_stars(dims = c("ea_east.bin", "ea_north.bin")) |>
    st_set_crs(crs.ea$amz) |>
    merge(name = "band") |>
    setNames("risk")

  write_stars(rast.r, paste0(path.geo, region, ".risk.tif"))

  rast.arc <-
    geo.sum[[region]]$arc[, .(ea_east.bin, ea_north.bin, mean, sd, q2.5, q97.5)] |>
    st_as_stars(dims = c("ea_east.bin", "ea_north.bin")) |>
    st_set_crs(crs.ea$amz) |>
    merge(name = "band") |>
    setNames("risk")

  write_stars(rast.arc, paste0(path.geo, region, ".arc.tif"))

}




library(data.table)
library(mgcv)
library(sf)
library(ggplot)
library(ggdist)

source("utilities.R")
options("ffbatchsize" = 1, "ffbatchbytes" = 2 * 2^30)

path.models <- "../results/models/"
path.data <- "../processed/data/"
path.som <- "../processed/som/"

## MAP BACKGROUND CRS

cam.crs.ea <- 
  st_crs('PROJCS["Central_America_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",14.5],PARAMETER["longitude_of_center",-93.6],PARAMETER["standard_parallel_1",20.5],PARAMETER["standard_parallel_2",8.5],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900002"]]')

amz.crs.ea <- st_crs("ESRI:102033")

bg_map <- st_read("~/projects/data/source/gadm/3.6/world/gadm36_levels.gpkg",
                  layer = "level0",
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
                                           'VEN', 'VGB', 'VIR', 'XCL')")

amz.bg_map <- st_transform(bg_map, "ESRI:102033")
cam.bg_map <- st_transform(bg_map, cam.crs.ea)



## PREDICTIONS 

load_models("cam.m2", summary = FALSE)

system.time({
cam.lp <- predict(cam.m2, cam.m2$model[1:5e3,], discrete = FALSE,
                type = "link",
                block.size = 5e3,
                newdata.guaranteed = TRUE,
                )
})

saveRDS(cam.lp, paste0(path.models, "cam.m2.lp.rds"))

rm(cam.m2, cam.lp)


load_models("amz.m2", summary = FALSE)

system.time({
amz.lp <- predict(amz.m2, amz.m2$model, discrete = FALSE,
                type = "link",
                block.size = 5e3,
                newdata.guaranteed = TRUE
                )
})

saveRDS(amz.lp, paste0(path.models, "amz.m2.lp.rds"))

rm(amz.m2, amz.lp)

## CONDITIONAL PREDICTIONS 

# load_models("cam.m2", summary = FALSE)

# system.time({
# cam.predcon <- predict(cam.m2, cam.m2$model, discrete = FALSE,
#                 exclude = c(
#                             "s(som_x,som_y)",
#                             "s(som_x,som_y):adm0BLZ",
#                             "s(som_x,som_y):adm0CRI",
#                             "s(som_x,som_y):adm0GTM",
#                             "s(som_x,som_y):adm0HND",
#                             "s(som_x,som_y):adm0MEX",
#                             "s(som_x,som_y):adm0NIC",
#                             "s(som_x,som_y):adm0PAN",
#                             "s(som_x,som_y):adm0SLV"
#                             ),
#                 type = "link",
#                 block.size = 5e3,
#                 newdata.guaranteed = TRUE,
#                 se.fit = TRUE
#                 )
# })
# saveRDS(cam.predcon, paste0(path.models, "cam.m2.predcon.rds"))

# rm(cam.m2, cam.predcon)


# load_models("amz.m2", summary = FALSE)

# system.time({
# amz.predcon <- predict(amz.m2, amz.m2$model, discrete = FALSE,
#                 exclude = c(
#                             "s(som_x,som_y)",
#                             "s(som_x,som_y):adm0BOL",
#                             "s(som_x,som_y):adm0BRA",
#                             "s(som_x,som_y):adm0COL",
#                             "s(som_x,som_y):adm0ECU",
#                             "s(som_x,som_y):adm0GUF",
#                             "s(som_x,som_y):adm0GUY",
#                             "s(som_x,som_y):adm0PER",
#                             "s(som_x,som_y):adm0SUR",
#                             "s(som_x,som_y):adm0VEN"
#                             ),
#                 type = "link",
#                 block.size = 5e3,
#                 newdata.guaranteed = TRUE
#                 )
# })
# saveRDS(amz.predcon, paste0(path.models, "amz.m2.predcon.rds"))

# rm(amz.m2, amz.predcon)

## MAPPING PREPARATION

## Central America

load_models("cam.m2", summary = FALSE)
linkfun <- cam.m2$fam$linkfun
linkinv <- cam.m2$fam$linkinv
cam.mu <- coefficients(cam.m2)
cam.V <- cam.m2$Vc
cam.p <- fitted(cam.m2)
ndraws <- 1000
cam.post <- mvnfast::rmvn(ndraws, cam.mu, cam.V, ncores = 8)
saveRDS(cam.post, paste0(path.models, "cam.m2.post.rds"))
unload_models("cam.m2")

cam.data <- readRDS(paste0(path.data, "cam.data.rds"))
cam.predcon <- readRDS(paste0(path.models, "cam.m2.predcon.rds"))
cam.post <- readRDS(paste0(path.models, "cam.m2.post.rds"))

cam.data <- cam.data
cam.data$pred <- cam.p
cam.data$lp_con <- cam.predcon

cam.data[, bl_all := cam.data[it_type == "none" & pa_type == "none", 
                              Mode(lp_con)]]
cam.data <- merge(cam.data, 
                  cam.data[it_type == "none" & pa_type == "none", 
                           .(bl_adm = Mode(lp_con)),
                           adm0],
                  by = "adm0", all.x = TRUE)
setkey(cam.data, "id")

cam.data[,
         `:=`(lp_con.bl_all = lp_con - bl_all,
              lp_con.bl_adm = lp_con - bl_adm)
         ][,
           `:=`(rc.bl_all = (exp(lp_con.bl_all) - 1) * 100,
                rc.bl_adm = (exp(lp_con.bl_adm) - 1) * 100)
           ]


map_res <- 1000
cam.coord_bins <- bin_cols(cam.data, c("ea_east", "ea_north"), c(map_res, map_res))
cam.data[, `:=`(ea_east.bin = cam.coord_bins$ea_east.bin,
                ea_north.bin = cam.coord_bins$ea_north.bin)]


cam.agg <- cam.data[,
                    .(
                      pred.m = mean(pred),
                      lp_con.m = mean(lp_con),
                      lp_con.bl_all.m = mean(lp_con.bl_all),
                      lp_con.bl_adm.m = mean(lp_con.bl_adm),
                      nobs = .N),
                    .(ea_east.bin, ea_north.bin)]
cam.agg[,
        `:=`(
             lp_con.bl_all.th = factor(fcase(lp_con.bl_all.m < -0.1, "reduced",
                                             lp_con.bl_all.m > 0.1, "increased",
                                             default = "unchanged"),
                                       levels = c("reduced", "unchanged", "increased")),
             lp_con.bl_adm.th = factor(fcase(lp_con.bl_adm.m < -0.1, "reduced",
                                             lp_con.bl_adm.m > 0.1, "increased",
                                             default = "unchanged"),
                                       levels = c("reduced", "unchanged", "increased")))
        ]

cam.agg.it_rec <- cam.data[it_type == "recognized",
                          .(
                            pred.m = mean(pred),
                            lp_con.m = mean(lp_con),
                            lp_con.bl_all.m = mean(lp_con.bl_all),
                            lp_con.bl_adm.m = mean(lp_con.bl_adm),
                            nobs = .N),
                          .(ea_east.bin, ea_north.bin)]
cam.agg.it_rec[,
               `:=`(
                    lp_con.bl_all.th = factor(fcase(lp_con.bl_all.m < -0.1, "reduced",
                                                    lp_con.bl_all.m > 0.1, "increased",
                                                    default = "unchanged"),
                                              levels = c("reduced", "unchanged", "increased")),
                    lp_con.bl_adm.th = factor(fcase(lp_con.bl_adm.m < -0.1, "reduced",
                                                    lp_con.bl_adm.m > 0.1, "increased",
                                                    default = "unchanged"),
                                              levels = c("reduced", "unchanged", "increased")))
               ]


# save.image("../results/esa/cam.ws.RData")
# load("../results/esa/cam.ws.RData")

## MAPS
map_theme <-  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey97", colour = NA),
        panel.grid = element_line(colour = "grey82"),
        legend.position = "bottom",
        legend.justification = "right"
        )

map_xlim <- c(-1.5e6, 2e6)
map_ylim <- c(-8.5e5, 1.05e6)

# cam.map.pred <- ggplot(cam.agg) +
#   geom_sf(data = cam.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
#   geom_raster(mapping = aes(fill = pred.m * 100,
#                             , x = ea_east.bin, y = ea_north.bin)) +
#   coord_sf(crs = cam.crs.ea, expand = FALSE) +
#   scale_fill_viridis_c(option = "A") +
#   xlim(range(cam.agg$ea_east.bin)[1] - map_res, range(cam.agg$ea_east.bin)[2] + map_res) +
#   ylim(range(cam.agg$ea_north.bin)[1] - map_res, range(cam.agg$ea_north.bin)[2] + map_res) +
#   labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
#   map_theme

# cam.map.lp_con <- ggplot(cam.agg) +
#   geom_sf(data = cam.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
#   geom_raster(mapping = aes(fill = linkinv(lp_con.m) * 100,
#                             , x = ea_east.bin, y = ea_north.bin)) +
#   coord_sf(crs = cam.crs.ea, expand = FALSE) +
#   scale_fill_viridis_c(option = "A") +
#   xlim(range(cam.agg$ea_east.bin)[1] - map_res, range(cam.agg$ea_east.bin)[2] + map_res) +
#   ylim(range(cam.agg$ea_north.bin)[1] - map_res, range(cam.agg$ea_north.bin)[2] + map_res) +
#   labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
#   map_theme

cam.map.lp_con.bl_all <- ggplot(cam.agg) +
  geom_sf(data = cam.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = lp_con.bl_all.th
                            , x = ea_east.bin, y = ea_north.bin)) +
  coord_sf(crs = cam.crs.ea, expand = FALSE, 
           xlim = map_xlim, ylim = map_ylim) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
# cam.map.lp_con.bl_all

cam.map.lp_con.bl_all.it_rec <- ggplot(cam.agg.it_rec) +
  geom_sf(data = cam.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = lp_con.bl_all.th
                            , x = ea_east.bin, y = ea_north.bin)) +
  coord_sf(crs = cam.crs.ea, expand = FALSE, 
           xlim = map_xlim, ylim = map_ylim) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
  map_theme
# cam.map.lp_con.bl_all.it_rec

cam.map.lp_con.bl_adm.it_rec <- ggplot(cam.agg.it_rec) +
  geom_sf(data = cam.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = lp_con.bl_adm.th
                            , x = ea_east.bin, y = ea_north.bin)) +
  coord_sf(crs = cam.crs.ea, expand = FALSE, 
           xlim = map_xlim, ylim = map_ylim) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
  map_theme
# cam.map.lp_con.bl_adm.it_rec

ggsave("../results/esa/cam.map.lp_con.bl_all.png", cam.map.lp_con.bl_all, 
       width = 9, height = 5, dpi = 300, units = "in")
ggsave("../results/esa/cam.map.lp_con.bl_all.it_rec.png", cam.map.lp_con.bl_all.it_rec, 
       width = 9, height = 5, dpi = 300, units = "in")
ggsave("../results/esa/cam.map.lp_con.bl_adm.it_rec.png", cam.map.lp_con.bl_adm.it_rec ,
       width = 9, height = 5, dpi = 300, units = "in")

## TYPES

types <- c("IT, off. recognized",
           "IT, not recognized",
           "PA, indirect use",
           "PA, direct use",
           "* IT recognized, PA indirect use",
           "* IT not recognized, PA indirect use",
           "* IT recognized, PA direct use",
           "* IT not recognized, PA direct use")

types_coef <- matrix(0, nrow = 8, ncol = 8,
                     dimnames = list(types,
                                     names(cam.mu)[2:9]))

coef_id <- list(1, 2, 3, 4, c(1, 3, 5), c(1, 4, 6), c(2, 3, 7), c(2, 4, 8))
for(i in 1:length(coef_id)) {
  cols <- coef_id[[i]]
  types_coef[i, cols] <- 1}

# Contribution to linear predictor
hr <- cam.post[,2:9] %*% t(types_coef)

# Percentage change in risk of forest loss
rc <- (exp(hr) - 1) * 100
rc <- cbind(draw = 1:nrow(rc), rc)

rc_l <- melt(as.data.table(rc), id.vars = "draw", variable.name = "type", value.name = "risk_change")
rc_l$type <- factor(rc_l$type, levels = rev(types))

type_colours <- c(rep("#AA3377", 2),
                  rep("#228833", 2),
                  rep("grey50", 4))
names(type_colours) <- types


cam.plot.rc_it <-
  ggplot(rc_l[type %in% types[1:2]] , aes(y = type, x = risk_change, colour = type)) +
  stat_pointinterval(point_interval = mode_hdi, .width = c(0.5, 0.95)) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_colour_manual(values = type_colours, guide = "none") +
  scale_fill_manual(values = type_colours, guide = "none") +
  guides(fill = "none", colour = "none") +
  labs(x = NULL,
         y = NULL,
         title = "Central America",
         subtitle = "Indigenous territories") +
  theme_ggdist() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(colour = "grey5")
        )

cam.plot.rc_pa <-
  ggplot(rc_l[type %in% types[3:4]] , aes(y = type, x = risk_change, colour = type)) +
  stat_pointinterval(point_interval = mode_hdi, .width = c(0.5, 0.95)) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_colour_manual(values = type_colours, guide = "none") +
  scale_fill_manual(values = type_colours, guide = "none") +
  guides(fill = "none", colour = "none") +
  labs(x = NULL,
       y = NULL,
       subtitle = "Protected areas") +
  theme_ggdist() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(colour = "grey5")
        )

cam.plot.rc_ov <-
  ggplot(rc_l[type %in% types[5:8]] , aes(y = type, x = risk_change, colour = type)) +
  stat_pointinterval(point_interval = mode_hdi, .width = c(0.5, 0.95)) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_colour_manual(values = type_colours, guide = "none") +
  scale_fill_manual(values = type_colours, guide = "none") +
  guides(fill = "none", colour = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       subtitle = "Overlapping areas"
       , caption = "\n* less than 3% of sample"
       ) +
  theme_ggdist()

cam.plot.rc <-
  (cam.plot.rc_it + cam.plot.rc_pa + cam.plot.rc_ov +
  plot_layout(nrow = 3) & 
  theme(plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face = "italic", size = 9, colour = "grey20"),
        plot.caption.position = "plot",
        # axis.ticks = element_blank(),
        # axis.text.y = element_text(colour = "grey20", size = 10)
        ) &
  scale_x_continuous(limits = c(-100, 20), breaks = (-10:1)*10, expand = expansion()) &
  coord_fixed(4))

ggsave("../results/esa/cam.plot.rc.png", 
       cam.plot.rc, width = 8, height = 5.75, dpi = 300, units = "in")


## COUNTRIES

cam.adm.it_rec <- c("CRI", "MEX", "NIC", "PAN")
cam.adm.it_not_rec <- c("BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV")

rc.it_adm <- 
  cam.data[(it_type == "recognized" & adm0 %in% cam.adm.it_rec) | 
           (it_type == "not_recognized" & adm0 %in% cam.adm.it_not_rec),
           .(adm0,
             type = factor(fcase(it_type == "recognized", types[1],
                                  it_type == "not_recognized", types[2]),
                           levels = types [1:2]),
             rc.bl_all,
             rc.bl_adm,
             lp_con.bl_all,
             lp_con.bl_adm)
            ]

trans_breaks <- c(10, 25, 50, 100, 200, 600, 1100)

cam.plot.rc.it_adm.bl_all <-
  ggplot(rc.it_adm[type == types[1]], 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = rc.bl_all + 100
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1024, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 100, colour = "grey30") +
  coord_trans(x = scales::log_trans(10), xlim = c(10, 2000)) +
  scale_x_continuous(breaks = trans_breaks,
                     labels = as.character(trans_breaks - 100),
                     expand = expansion(add = 0.05)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       title = "Recognized indigenous territories",
       subtitle = "Baseline: Study area",
       ) +
  theme_ggdist()

cam.plot.rc.it_adm.bl_adm <-
  ggplot(rc.it_adm[type == types[1]], 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = rc.bl_adm + 100
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1024, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 100, colour = "grey30") +
  coord_trans(x = scales::log_trans(10), xlim = c(10, 2000)) +
  scale_x_continuous(breaks = trans_breaks,
                     labels = as.character(trans_breaks - 100),
                     expand = expansion(add = 0.05)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       subtitle = "Baseline: Country",
       ) +
  theme_ggdist()

cam.plot.rc.it_adm <-
  (cam.plot.rc.it_adm.bl_all + cam.plot.rc.it_adm.bl_adm) &
  theme(axis.text.y = element_text(colour = "grey5", face = "bold"))

ggsave("../results/esa/cam.plot.rc.it_adm.png", 
       cam.plot.rc.it_adm, width = 9, height = 4.5, dpi = 300, units = "in")


## CONDITIONAL PREDICTIONS - POSTERIOR

load_models("cam.m2", summary = FALSE)

set.seed(1234)
sam <- sample(1:nrow(cam.m2$model), 5e5)

system.time({
cam.lpm <- predict(cam.m2, cam.m2$model[sam,], discrete = FALSE,
                exclude = c(
                            "s(som_x,som_y)",
                            "s(som_x,som_y):adm0BLZ",
                            "s(som_x,som_y):adm0CRI",
                            "s(som_x,som_y):adm0GTM",
                            "s(som_x,som_y):adm0HND",
                            "s(som_x,som_y):adm0MEX",
                            "s(som_x,som_y):adm0NIC",
                            "s(som_x,som_y):adm0PAN",
                            "s(som_x,som_y):adm0SLV"
                            ),
                type = "lpmatrix",
                block.size = 5e3,
                newdata.guaranteed = TRUE,
                se.fit = TRUE
                )
})
saveRDS(cam.lpm, paste0(path.models, "cam.m2.lpm.rds"))

unload_models("cam.m2")


## Alternative country-level plots

types <- c("IT, off. recognized",
           "IT, not recognized",
           "PA, indirect use",
           "PA, direct use",
           "* IT recognized, PA indirect use",
           "* IT not recognized, PA indirect use",
           "* IT recognized, PA direct use",
           "* IT not recognized, PA direct use")



cam.data <- readRDS(paste0(path.data, "cam.data.rds"))
cam.data.s <- cam.data[sam,.(it_type, pa_type, adm0)]

cam.lpm <- readRDS(paste0(path.models, "cam.m2.lpm.rds"))
cam.post <- readRDS(paste0(path.models, "cam.m2.post.rds"))

cam.ps <- cam.lpm %*% t(cam.post)
colnames(cam.ps) <- paste0("sim", 1:nrow(cam.post))

cam.data.s <- cbind(cam.data.s, cam.ps)

cam.lp.it_rec.adm <-
  merge(melt(cam.data.s[it_type == "recognized" & adm0 %in% cam.adm.it_rec,
                        lapply(.SD,mean),
                       .SDcols = colnames(cam.ps),
                       by = .(adm0)],
             id.vars = "adm0", variable.name = "draw", value.name = "lp"),
        melt(cam.data.s[it_type == "none" & pa_type == "none" & adm0 %in% cam.adm.it_rec,
                        lapply(.SD,mean),
                        .SDcols = colnames(cam.ps),
                        by = .(adm0)],
             id.vars = "adm0", variable.name = "draw", value.name = "bl_adm"),
        by = c("draw", "adm0")
        )
cam.lp.it_rec.adm <-
  merge(cam.lp.it_rec.adm,
        melt(cam.data.s[it_type == "none" & pa_type == "none",
                        lapply(.SD,mean),
                        .SDcols = colnames(cam.ps)],
             variable.name = "draw", value.name = "bl_all"),
        by = "draw" 
  )

cam.lp.it_not_rec.adm <-
  merge(melt(cam.data.s[it_type == "not_recognized" & adm0 %in% cam.adm.it_not_rec,
                        lapply(.SD,mean),
                       .SDcols = colnames(cam.ps),
                       by = .(adm0)],
             id.vars = "adm0", variable.name = "draw", value.name = "lp"),
        melt(cam.data.s[it_type == "none" & pa_type == "none" & adm0 %in% cam.adm.it_not_rec,
                        lapply(.SD,mean),
                        .SDcols = colnames(cam.ps),
                        by = .(adm0)],
             id.vars = "adm0", variable.name = "draw", value.name = "bl_adm"),
        by = c("draw", "adm0")
        )
cam.lp.it_not_rec.adm <-
  merge(cam.lp.it_not_rec.adm,
        melt(cam.data.s[it_type == "none" & pa_type == "none",
                        lapply(.SD,mean),
                        .SDcols = colnames(cam.ps)],
             variable.name = "draw", value.name = "bl_all"),
        by = "draw" 
  )


cam.plot.rc.it_adm.bl_all <-
  ggplot(cam.lp.it_rec.adm, 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = 100 * (exp(lp-bl_all) - 1)
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1000, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_y_discrete(limits = rev) +
  xlim(c(-100,320)) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       title = "Recognized indigenous territories",
       subtitle = "Baseline: Study area",
       ) +
  theme_ggdist()

cam.plot.rc.it_adm.bl_adm <-
  ggplot(cam.lp.it_rec.adm, 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = 100 * (exp(lp-bl_adm) - 1)
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1000, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_y_discrete(limits = rev) +
  xlim(c(-100,320)) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       title = "Recognized indigenous territories",
       subtitle = "Baseline: Country",
       ) +
  theme_ggdist()

cam.plot.rc.it_adm <-
  (cam.plot.rc.it_adm.bl_all + cam.plot.rc.it_adm.bl_adm) &
  theme(axis.text.y = element_text(colour = "grey5", face = "bold"))

ggsave("../results/esa/cam.plot.rc.it_adm.png", 
       cam.plot.rc.it_adm, width = 9, height = 4.5, dpi = 300, units = "in")



cam.plot.rc.it_adm.bl_all <-
  ggplot(cam.lp.it_not_rec.adm, 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = 100 * (exp(lp-bl_all) - 1)
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1000, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_y_discrete(limits = rev) +
  xlim(c(-100,320)) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       title = "Not off. recognized indigenous territories",
       subtitle = "Baseline: Study area",
       ) +
  theme_ggdist()

cam.plot.rc.it_adm.bl_adm <-
  ggplot(cam.lp.it_not_rec.adm, 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = 100 * (exp(lp-bl_adm) - 1)
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1000, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_y_discrete(limits = rev) +
  xlim(c(-100,320)) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       title = "Not off. recognized indigenous territories",
       subtitle = "Baseline: Country",
       ) +
  theme_ggdist()

cam.plot.rc.it_adm <-
  (cam.plot.rc.it_adm.bl_all + cam.plot.rc.it_adm.bl_adm) &
  theme(axis.text.y = element_text(colour = "grey5", face = "bold"))


ggsave("../results/esa/cam.plot.rc.it_adm.not_rec.png", 
       cam.plot.rc.it_adm, width = 9, height = 4.5, dpi = 300, units = "in")


cam.plot.rc.it_adm.bl_all <-
  ggplot(cam.lp.it_rec.adm, 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = 100 * (exp(lp-bl_all) - 1)
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1000, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       title = "Recognized indigenous territories",
       subtitle = "Baseline: Study area",
       ) +
  theme_ggdist()




bl_all.id <- which(cam.data.s$it_type == "none" & cam.data.s$pa_type == "none")
bl_all <- apply(cam.ps[bl.all.id,], 2, mean) 

pan.bl.id <- which(cam.data.s$adm0 == "PAN" & cam.data.s$it_type == "none" & cam.data.s$pa_type == "none")
pan.id <- which(cam.data.s$adm0 == "PAN" & cam.data.s$it_type == "recognized")


pan <- colMeans(cam.ps[pan.id,])
pan.bl <- colMeans(cam.ps[pan.bl.id,])

par(mfrow = c(3,1))
plot(density(linkinv(bl_all)))
plot(density(linkinv(pan.bl)))
plot(density(linkinv(pan)))


par(mfrow = c(3,1))
plot(density(exp(pan.bl - bl_all)))
plot(density(exp(pan - bl_all)))
plot(density(exp(pan - pan.bl)))
cam.ps.bl_all <- t(apply(cam.ps, 1, \(x) x - bl_all))

cam.rc.bl_all <- cbind(cam.data.s, as.data.table((exp(cam.ps.bl_all)-1)*100))
# cam.rc.bl_all <- cbind(cam.data.s, as.data.table(linkinv(cam.ps.bl_all)))

cam.rc.bl_all <- melt(cam.rc.bl_all,
                      id.vars = c("it_type", "pa_type", "adm0"),
                      variable.name = "draw", value.name = "rc")


trans_breaks <- c(10, 25, 50, 100, 200, 600, 1100)

cam.plot.rc.it_adm.bl_all <-
  ggplot(cam.rc.bl_all[(it_type == "recognized" & adm0 %in% cam.adm.it_rec)], 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = log(rc /100 + 1.0)
             )) +
  stat_eye(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1000, colour = type_colours[1], fill = "grey85",
                    scale = 0.5)

  +
  geom_vline(xintercept = 100, colour = "grey30") +
  coord_trans(x = scales::log_trans(10), xlim = c(10, 2000)) +
  scale_x_continuous(breaks = trans_breaks,
                     labels = as.character(trans_breaks - 100),
                     expand = expansion(add = 0.05)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       title = "Recognized indigenous territories",
       subtitle = "Baseline: Study area",
       ) +
  theme_ggdist()



cam.fl.all <- cbind(cam.data.s, as.data.table(cam.ps))
cam.fl.all <- melt(cam.fl.all,
                      id.vars = c("it_type", "pa_type", "adm0"),
                      variable.name = "draw", value.name = "fl")


it_adm <- ggplot(cam.fl.all[(it_type == "recognized" & adm0 %in% cam.adm.it_rec)], 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = fl 
             )) +
  stat_eye(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 500, colour = type_colours[1], fill = "grey85",
                    scale = 0.5)

ref <- ggplot(cam.fl.all[(it_type == "none" & pa_type == "none" & adm0 %in% cam.adm.it_rec),
                         .(adm0 = "ref", fl)][sample(1:5e7, 1e6)], 
  # ggplot(rc.it_adm, 
         aes(y = adm0, x = linkinv(fl)
             )) +
  stat_eye(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 500, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  coord_cartesian(c(-10,10))

?coord_cartesian

it_adm / ref


## AMAZON

load_models("amz.m2", summary = FALSE)
linkfun <- amz.m2$fam$linkfun
linkinv <- amz.m2$fam$linkinv
amz.mu <- coefficients(amz.m2)
amz.V <- amz.m2$Vc
amz.p <- fitted(amz.m2)
ndraws <- 1000
amz.post <- mvnfast::rmvn(ndraws, amz.mu, amz.V, ncores = 8)
unload_models("amz.m2")

amz.data <- readRDS(paste0(path.data, "amz.data.rds"))
amz.predcon <- readRDS(paste0(path.models, "amz.m2.predcon.rds"))

amz.data <- amz.data
amz.data$pred <- amz.p
amz.data$lp_con <- amz.predcon

amz.data[, bl_all := amz.data[it_type == "none" & pa_type == "none", 
                              Mode(lp_con)]]
amz.data <- merge(amz.data, 
                  amz.data[it_type == "none" & pa_type == "none", 
                           .(bl_adm = Mode(lp_con)),
                           adm0],
                  by = "adm0", all.x = TRUE)
setkey(amz.data, "id")

amz.data[,
         `:=`(lp_con.bl_all = lp_con - bl_all,
              lp_con.bl_adm = lp_con - bl_adm)
         ][,
           `:=`(rc.bl_all = (exp(lp_con.bl_all) - 1) * 100,
                rc.bl_adm = (exp(lp_con.bl_adm) - 1) * 100)
           ]


map_res <- 2000
amz.coord_bins <- bin_cols(amz.data, c("ea_east", "ea_north"), c(map_res, map_res))
amz.data[, `:=`(ea_east.bin = amz.coord_bins$ea_east.bin,
                ea_north.bin = amz.coord_bins$ea_north.bin)]


amz.agg <- amz.data[,
                    .(
                      pred.m = mean(pred),
                      lp_con.m = mean(lp_con),
                      lp_con.bl_all.m = mean(lp_con.bl_all),
                      lp_con.bl_adm.m = mean(lp_con.bl_adm),
                      nobs = .N),
                    .(ea_east.bin, ea_north.bin)]
amz.agg[,
        `:=`(
             lp_con.bl_all.th = factor(fcase(lp_con.bl_all.m < -0.1, "reduced",
                                             lp_con.bl_all.m > 0.1, "increased",
                                             default = "unchanged"),
                                       levels = c("reduced", "unchanged", "increased")),
             lp_con.bl_adm.th = factor(fcase(lp_con.bl_adm.m < -0.1, "reduced",
                                             lp_con.bl_adm.m > 0.1, "increased",
                                             default = "unchanged"),
                                       levels = c("reduced", "unchanged", "increased")))
        ]

amz.agg.it_rec <- amz.data[it_type == "recognized",
                          .(
                            pred.m = mean(pred),
                            lp_con.m = mean(lp_con),
                            lp_con.bl_all.m = mean(lp_con.bl_all),
                            lp_con.bl_adm.m = mean(lp_con.bl_adm),
                            nobs = .N),
                          .(ea_east.bin, ea_north.bin)]
amz.agg.it_rec[,
               `:=`(
                    lp_con.bl_all.th = factor(fcase(lp_con.bl_all.m < -0.1, "reduced",
                                                    lp_con.bl_all.m > 0.1, "increased",
                                                    default = "unchanged"),
                                              levels = c("reduced", "unchanged", "increased")),
                    lp_con.bl_adm.th = factor(fcase(lp_con.bl_adm.m < -0.1, "reduced",
                                                    lp_con.bl_adm.m > 0.1, "increased",
                                                    default = "unchanged"),
                                              levels = c("reduced", "unchanged", "increased")))
               ]


# save.image("../results/esa/amz.ws.RData")
# load("../results/esa/amz.ws.RData")

## MAPS
map_theme <-  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey97", colour = NA),
        panel.grid = element_line(colour = "grey82"),
        legend.position = "bottom",
        legend.justification = "right"
        )

map_xlim <- c(-2.5e6, 2e6)
map_ylim <- c(1.25e6, 4.75e6)
# map_xlim <- range(amz.agg$ea_east.bin)
# map_ylim <- range(amz.agg$ea_north.bin)

# amz.map.pred <- ggplot(amz.agg) +
#   geom_sf(data = amz.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
#   geom_raster(mapping = aes(fill = pred.m * 100,
#                             , x = ea_east.bin, y = ea_north.bin)) +
#   coord_sf(crs = amz.crs.ea, expand = FALSE) +
#   scale_fill_viridis_c(option = "A") +
#   xlim(range(amz.agg$ea_east.bin)[1] - map_res, range(amz.agg$ea_east.bin)[2] + map_res) +
#   ylim(range(amz.agg$ea_north.bin)[1] - map_res, range(amz.agg$ea_north.bin)[2] + map_res) +
#   labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
#   map_theme

# amz.map.lp_con <- ggplot(amz.agg) +
#   geom_sf(data = amz.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
#   geom_raster(mapping = aes(fill = linkinv(lp_con.m) * 100,
#                             , x = ea_east.bin, y = ea_north.bin)) +
#   coord_sf(crs = amz.crs.ea, expand = FALSE) +
#   scale_fill_viridis_c(option = "A") +
#   xlim(range(amz.agg$ea_east.bin)[1] - map_res, range(amz.agg$ea_east.bin)[2] + map_res) +
#   ylim(range(amz.agg$ea_north.bin)[1] - map_res, range(amz.agg$ea_north.bin)[2] + map_res) +
#   labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
#   map_theme

amz.map.lp_con.bl_all <- ggplot(amz.agg) +
  geom_sf(data = amz.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = lp_con.bl_all.th
                            , x = ea_east.bin, y = ea_north.bin)) +
  coord_sf(crs = amz.crs.ea, expand = FALSE, 
           xlim = map_xlim, ylim = map_ylim) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  map_theme
# amz.map.lp_con.bl_all

amz.map.lp_con.bl_all.it_rec <- ggplot(amz.agg.it_rec) +
  geom_sf(data = amz.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = lp_con.bl_all.th
                            , x = ea_east.bin, y = ea_north.bin)) +
  coord_sf(crs = amz.crs.ea, expand = FALSE, 
           xlim = map_xlim, ylim = map_ylim) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
  map_theme
# amz.map.lp_con.bl_all.it_rec

amz.map.lp_con.bl_adm.it_rec <- ggplot(amz.agg.it_rec) +
  geom_sf(data = amz.bg_map, fill = "grey35", colour = "grey50", size = 0.5) +
  geom_raster(mapping = aes(fill = lp_con.bl_adm.th
                            , x = ea_east.bin, y = ea_north.bin)) +
  coord_sf(crs = amz.crs.ea, expand = FALSE, 
           xlim = map_xlim, ylim = map_ylim) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  labs(fill = "Risk of forest loss", x = "Longitude", y = "Latitude") +
  map_theme
# amz.map.lp_con.bl_adm.it_rec

ggsave("../results/esa/amz.map.lp_con.bl_all.png", amz.map.lp_con.bl_all, 
       width = 9, height = 5, dpi = 300, units = "in")
ggsave("../results/esa/amz.map.lp_con.bl_all.it_rec.png", amz.map.lp_con.bl_all.it_rec, 
       width = 9, height = 5, dpi = 300, units = "in")
ggsave("../results/esa/amz.map.lp_con.bl_adm.it_rec.png", amz.map.lp_con.bl_adm.it_rec ,
       width = 9, height = 5, dpi = 300, units = "in")

## TYPES

types <- c("IT, off. recognized",
           "IT, not recognized",
           "PA, indirect use",
           "PA, direct use",
           "* IT recognized, PA indirect use",
           "* IT not recognized, PA indirect use",
           "* IT recognized, PA direct use",
           "* IT not recognized, PA direct use")

types_coef <- matrix(0, nrow = 8, ncol = 8,
                     dimnames = list(types,
                                     names(amz.mu)[2:9]))

coef_id <- list(1, 2, 3, 4, c(1, 3, 5), c(1, 4, 6), c(2, 3, 7), c(2, 4, 8))
for(i in 1:length(coef_id)) {
  cols <- coef_id[[i]]
  types_coef[i, cols] <- 1}

# Contribution to linear predictor
hr <- amz.post[,2:9] %*% t(types_coef)

# Percentage change in risk of forest loss
rc <- (exp(hr) - 1) * 100
rc <- cbind(draw = 1:nrow(rc), rc)

rc_l <- melt(as.data.table(rc), id.vars = "draw", variable.name = "type", value.name = "risk_change")
rc_l$type <- factor(rc_l$type, levels = rev(types))

type_colours <- c(rep("#AA3377", 2),
                  rep("#228833", 2),
                  rep("grey50", 4))
names(type_colours) <- types


amz.plot.rc_it <-
  ggplot(rc_l[type %in% types[1:2]] , aes(y = type, x = risk_change, colour = type)) +
  stat_pointinterval(point_interval = mode_hdi, .width = c(0.5, 0.95)) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_colour_manual(values = type_colours, guide = "none") +
  scale_fill_manual(values = type_colours, guide = "none") +
  guides(fill = "none", colour = "none") +
  labs(x = NULL,
         y = NULL,
         title = "Amazon",
         subtitle = "Indigenous territories") +
  theme_ggdist() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(colour = "grey5")
        )

amz.plot.rc_pa <-
  ggplot(rc_l[type %in% types[3:4]] , aes(y = type, x = risk_change, colour = type)) +
  stat_pointinterval(point_interval = mode_hdi, .width = c(0.5, 0.95)) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_colour_manual(values = type_colours, guide = "none") +
  scale_fill_manual(values = type_colours, guide = "none") +
  guides(fill = "none", colour = "none") +
  labs(x = NULL,
       y = NULL,
       subtitle = "Protected areas") +
  theme_ggdist() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(colour = "grey5")
        )

amz.plot.rc_ov <-
  ggplot(rc_l[type %in% types[5:8]] , aes(y = type, x = risk_change, colour = type)) +
  stat_pointinterval(point_interval = mode_hdi, .width = c(0.5, 0.95)) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_colour_manual(values = type_colours, guide = "none") +
  scale_fill_manual(values = type_colours, guide = "none") +
  guides(fill = "none", colour = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       subtitle = "Overlapping areas"
       , caption = "\n* less than 3% of sample"
       ) +
  theme_ggdist()

amz.plot.rc <-
  (amz.plot.rc_it + amz.plot.rc_pa + amz.plot.rc_ov +
  plot_layout(nrow = 3) & 
  theme(plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face = "italic", size = 9, colour = "grey20"),
        plot.caption.position = "plot",
        # axis.ticks = element_blank(),
        # axis.text.y = element_text(colour = "grey20", size = 10)
        ) &
  scale_x_continuous(limits = c(-100, 20), breaks = (-10:1)*10, expand = expansion()) &
  coord_fixed(4))

ggsave("../results/esa/amz.plot.rc.png", 
       amz.plot.rc, width = 8, height = 5.75, dpi = 300, units = "in")


## COUNTRIES

amz.adm.it_rec <- c("BOL", "BRA", "COL", "ECU", "GUF", "GUY", "PER", "VEN")
amz.adm.it_not_rec <- c("BOL", "ECU", "GUF", "GUY", "PER", "SUR", "VEN")

rc.it_adm <- 
  amz.data[(it_type == "recognized" & adm0 %in% amz.adm.it_rec) | 
           (it_type == "not_recognized" & adm0 %in% amz.adm.it_not_rec),
           .(adm0,
             type = factor(fcase(it_type == "recognized", types[1],
                                  it_type == "not_recognized", types[2]),
                           levels = types[1:2]),
             rc.bl_all,
             rc.bl_adm,
             lp_con.bl_all,
             lp_con.bl_adm)
            ]

amz.plot.rc.it_adm.bl_all <-
  ggplot(rc.it_adm[type == types[1]], 
         aes(y = adm0, x = rc.bl_all
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1024, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 0, colour = "grey30") +
  coord_cartesian(xlim = c(-100, 100)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = type_colours, guide = "none") +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       title = "Recognized indigenous territories",
       subtitle = "Baseline: Study area",
       ) +
  theme_ggdist()

amz.plot.rc.it_adm.bl_adm <-
  ggplot(rc.it_adm[type == types[1]], 
         aes(y = adm0, x = rc.bl_adm 
             )) +
  stat_ccdfinterval(point_interval = mode_hdi, .width = c(0.5, 0.95),
                    n = 1024, colour = type_colours[1], fill = "grey85",
                    scale = 0.5) +
  geom_vline(xintercept = 0, colour = "grey30") +
  coord_cartesian(xlim = c(-100, 500)) +
  scale_fill_manual(values = type_colours, guide = "none") +
  scale_y_discrete(limits = rev) +
  labs(x = "\nChange in risk of forest loss (%)",
       y = NULL,
       subtitle = "Baseline: Country",
       ) +
  theme_ggdist()

amz.plot.rc.it_adm <-
  (amz.plot.rc.it_adm.bl_all + amz.plot.rc.it_adm.bl_adm) &
  theme(axis.text.y = element_text(colour = "grey5", face = "bold"))

ggsave("../results/esa/amz.plot.rc.it_adm.png", 
       amz.plot.rc.it_adm, width = 9, height = 5.75, dpi = 300, units = "in")



args <- commandArgs(trailingOnly = TRUE)
library(data.table)
library(stringi)
library(ggplot2)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)

source("utilities.R")


region <- tolower(as.character(args[1]))
model_resp <- tolower(as.character(args[2]))

# region <- "amz"
# model_resp <- "def"

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.gam <- paste0(path.base, "models/gam/")
path.data.vis <- paste0(path.data, "visualization/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

# file.model <- paste0(path.gam, region, ".m1.", model_resp, ".rds")
# file.post <- paste0(path.gam, region, ".m1.", model_resp, ".post.rds")
file.res <- paste0(path.gam, region, ".m1.", model_resp, ".res.rds")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.data.vis <- paste0(path.data.vis, "maps.rds")

file.fig.res.cont <- paste0(path.figures, "res.cont.", model_resp, ".", region, ".png")
file.fig.res.disc <- paste0(path.figures, "res.disc.", model_resp, ".", region, ".png")
file.fig.res.geo <- paste0(path.figures, "res.geo.", model_resp, ".", region, ".png")
file.fig.res.som <- paste0(path.figures, "res.som.", model_resp, ".", region, ".png")


p.title <-
  paste0(switch(region, amz = "Amazon, ", cam = "Central America, "),
         switch(model_resp, def = "long-term disturbance", deg = "short-term disturbance"))

base.size <- 7 
# base.size <- 10
res_theme <-
  theme_light(base_family = "IBMPlexSansCondensed",
              base_size = base.size) +
  theme(
        axis.line.x = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.line.y = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.title.x = element_text(size = rel(0.9),
                                    lineheight = rel(1.15),
                                    margin = margin(t = base.size/2)),
        axis.title.y = element_text(size = rel(0.9),
                                    margin = margin(r = base.size/2)),
        axis.text.x = element_text(color = "black",
                                   size = rel(0.9),
                                   margin = margin(t = base.size/2)),
        axis.text.y = element_text(color = "black",
                                   size = rel(0.9),
                                   lineheight = rel(1.15),
                                   margin = margin(r = base.size/2),
                                   hjust = 0),
        axis.ticks = element_line(colour = "grey30"),
        legend.title = element_text(size = rel(1),
                                    margin = margin(b = base.size/2)),
        legend.position = "right",
        legend.justification = "center",
        legend.key.size = unit(base.size*1.5, "pt"),
        legend.text = element_text(size = rel(1), margin = margin(l = base.size/3,
                                                                  t = base.size/2,
                                                                  b = base.size/2)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing.x = unit(base.size*2, "pt"),
        panel.spacing.y = unit(base.size*2, "pt"),
        plot.margin = margin(3, 3, 3, 3),
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0,
                                  # face = "bold",
                                  face = "plain",
                                  size = rel(1.5),
                                  margin = margin(l = 0, b = base.size*2, t = base.size/3)),
        plot.subtitle = element_text(size = rel(1),
                                     margin = margin(b = 2*base.size)),
        plot.tag = element_text(face = "bold"),
        strip.text = element_text(size = rel(0.8),
                                  lineheight = rel(1),
                                  hjust = 0.5,
                                  vjust = 0.5,
                                  color = "black",
                                  margin = margin(
                                                  0.75*base.size,
                                                  0.75*base.size,
                                                  0.75*base.size,
                                                  0.75*base.size)),
        strip.background = element_rect(fill = "gray93", colour = NA)
        # strip.background = element_rect(fill = NA, colour = NA)
  )


map_theme <-  
  theme_minimal(base_family = "IBMPlexSansCondensed", base_size = base.size) +
  theme(panel.background = element_rect(fill = "grey99", colour = NA),
        panel.grid = element_line(colour = "grey75"),
        legend.position = "right",
        legend.justification = "center",
        legend.title = element_text(size = rel(0.75), hjust = 0,
                                    margin = margin(t = base.size/2, b = base.size),
                                    lineheight = rel(1)),
        legend.text = element_text(size = rel(0.65)),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(10, "pt"),
        strip.text = element_text(size = rel(0.8),
                                  lineheight = rel(1),
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
        plot.subtitle = element_text(size = rel(1),
                                     margin = margin(b = base.size)),
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
  data.table(itpa.label = c("Reference",
                           "IL, recognized", "IL, not recognized",
                           "PA, category I-IV", "PA, category V-VI",
                           "IL, rec.; PA, cat. I-IV",
                           "IL, rec.; PA, cat. V-VI",
                           "IL, not rec.; PA, cat. I-IV",
                           "IL, not rec.; PA, cat. V-VI"),
             it_type = c("none",
                         "recognized", "not_recognized",
                         "none", "none",
                         "recognized", "recognized",
                         "not_recognized", "not_recognized"),
             pa_type = c("none",
                         "none", "none",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use"))
cat.lab[, itpa.label := factor(itpa.label, levels = itpa.label)]

reg.lab <- list()
reg.lab$amz <-
  data.table(adm0 = c("BOL", "BRA", "COL", "ECU",
                      "GUF", "GUY", "PER", "SUR",
                      "VEN"),
             reg.label = c("Bolivia", "Brazil", "Colombia", "Ecuador",
                           "French Guiana", "Guyana", "Peru", "Suriname",
                           "Venezuela"))
reg.lab$cam <-
  data.table(adm0 = c("BLZ", "CRI", "GTM", "HND",
                      "MEX", "NIC", "PAN", "SLV"),
             reg.label = c("Belize", "Costa Rica", "Guatemala", "Honduras",
                           "Mexico", "Nicaragua", "Panama", "El Salvador"))
reg.lab$amz[, reg.label := factor(reg.label, levels = reg.label)]
reg.lab$cam[, reg.label := factor(reg.label, levels = reg.label)]


res.lab <- c("25% quantile", "Median", "75% quantile")
res.lab <- factor(res.lab, levels = res.lab)
names(res.lab) <- c("res.q25", "res.med", "res.q75")



# Load model, posterior draws, and data
# gam <- readRDS(file.model)
# post <- readRDS(file.post)
res <- readRDS(file.res)
data <- readRDS(file.data)

var.resp <-
  switch(model_resp,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

data.res <- merge(data, res)

## CONTINOUS PREDICTORS AND FITTED VALUES (POSTERIOR MEDIANS) ##########

var.cont <- 
  c("fitted",
    "elevation", "slope", "sx", "cmi_min",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_pop", "dens_roads", "travel_time")
var.cont.rank <- paste0(var.cont, ".rank")
var.cont.bin <- paste0(var.cont.rank, ".bin")

var.cont.label <-
  c("Posterior predictive median", 
    "Elevation", "Slope",
    "Agricultural suitability", "Minimal climate moisture index",
    "Distance to built-up areas", "Distance to roads", "Distance to rivers",
    "Population density", "Road density",
    "Travel time to cities") |>
  paste0("\n(rank transformed)")

data.res[,
         (var.cont.rank) := lapply(.SD, \(x) frank(x, ties.method = "random")/.N),
         .SDcols = var.cont]

res.sum.l <- list()
res.trend.l <- list()

for(i in seq_along(var.cont)) {
  res.sum.l[[i]] <-
    bin_cols(data.res[,
                      .(residual, rank.col),
                      env = list(rank.col = var.cont.rank[i])],
             c("residual", var.cont.rank[i]),
             bin.res = c(0.01, 0.01), bin.min = c(0, 0),
             append = TRUE) |>
    _[, .(n.res = .N), by = c("residual.bin", var.cont.bin[i])]
  trend.form <- as.formula(paste0("residual.bin ~ s(", var.cont.bin[i], ")"))
  trend.mod <- gam(trend.form, data = res.sum.l[[i]],
                   family = betar,
                   method = "REML")
  res.sum.l[[i]][, trend := fitted(trend.mod)]
}

names(res.sum.l)[1:length(var.cont)] <- var.cont

cont.lim <- range(unlist(lapply(res.sum.l, \(x) x[, range(n.res)])))

ncol.cont <- 4

plots.res.cont <- list()
for(i in seq_along(var.cont)) {
  n.med <- res.sum.l[[i]][, median(n.res)]
  n.lim <- res.sum.l[[i]][, n.med + (c(-1, 1) * rep(max(abs(range(n.res)-n.med)), 2))]
  draw.y <- ((i-1) %% ncol.cont) == 0
  if(draw.y) {
    res_theme.draw <- res_theme
  } else {
    res_theme.draw <-
      res_theme +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank())
  }
  res_theme.draw <-
    res_theme.draw + 
    theme(plot.margin = margin(t = base.size,
                             l = base.size,
                             b = base.size,
                             r = base.size))
  plots.res.cont[[i]] <-
    ggplot(res.sum.l[[i]]) +
      geom_raster(aes(x = !!sym(var.cont.bin[i]), y = residual.bin, fill = n.res)) +
      geom_line(aes(x = !!sym(var.cont.bin[i]), y = trend),
                linetype = "dashed", colour = "grey95") +
      scale_fill_viridis_c(guide = "none", limits = n.lim) +
      scale_y_continuous() +
      coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      labs(y = "DHARMa residual", x = var.cont.label[i]) +
      res_theme.draw
}


p.res.cont <-
  wrap_plots(plots.res.cont, ncol = ncol.cont, axes = "collect_y") +
  plot_annotation(subtitle = p.title, theme = res_theme)

png(file.fig.res.cont, width = 7, height = 6.5, unit = "in", res = 600)
p.res.cont
dev.off()


## DISCRETE PREDICTORS #################################################

data.res <-
  merge(data.res, cat.lab, by = c("it_type", "pa_type")) |>
  merge(reg.lab[[region]], by = "adm0")

plots.res.disc <- list()

plots.res.disc$itpa <-
  ggplot(data.res) +
    # geom_hline(yintercept = c(0.25, 0.5, 0.75),
    #            linetype = "dashed", linewidth = 0.3,
    #            colour = "grey35") +
    geom_boxplot(aes(x = itpa.label, y = residual),
                 fill = "grey95", linewidth = 0.4) +
    # coord_fixed(ratio = 6) +
    labs(y = "DHARMa residual", x = "Tenure regime") +
    res_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = base.size/2)),
          # axis.title.x = element_text(margin = margin(t = base.size)),
          axis.title.x = element_blank())

plots.res.disc$adm <-
  ggplot(data.res) +
    # geom_hline(yintercept = c(0.25, 0.5, 0.75),
    #            linetype = "dashed", linewidth = 0.3,
    #            colour = "grey35") +
    geom_boxplot(aes(x = reg.label, y = residual),
                 fill = "grey95", linewidth = 0.4) +
    # coord_fixed(ratio = 6) +
    labs(y = "DHARMa residual", x = "Administrative division (country)") +
    res_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = base.size/2)),
          # axis.title.x = element_text(margin = margin(t = base.size)),
          axis.title.x = element_blank())

p.res.disc <-
  (plots.res.disc$itpa + theme(plot.margin = margin(r = 20))) +
  plots.res.disc$adm +
  plot_annotation(subtitle = p.title, theme = res_theme)

png(file.fig.res.disc, width = 7, height = 2.5, unit = "in", res = 600)
p.res.disc
dev.off()


## GEOGRAPHIC AND SOM SPACE ###########################################

map.res <- switch(region, amz = 1e4, cam = 5e3)
map.anchor <- c(ea_east = floor(min(data$ea_east / map.res)) * map.res,
                ea_north = floor(min(data$ea_north / map.res)) * map.res)
map_xlim <- list(cam = c(-80e4, 120e4), amz = c(-240e4, 210e4))
map_ylim <- list(cam = c(-90e4,  80e4), amz = c(-200e4, 185e4))


poly <- readRDS(file.data.vis)$poly

data.res.geo <-
  bin_cols(data.res[, .(ea_east, ea_north, residual)],
           columns = c("ea_east", "ea_north"), bin.res = rep(map.res, 2),
           bin.min = map.anchor, append = TRUE) |>
  _[,
    .(res.med = median(residual),
      res.q25 = quantile(residual, 0.25),
      res.q75 = quantile(residual, 0.75)),
    by = .(ea_east.bin, ea_north.bin)] |>
  melt(measure.vars = c("res.med", "res.q25", "res.q75"),
       variable.name = "res.quant",
       value.name = "value")
data.res.geo[, res.lab := res.lab[as.character(res.quant)]]

p.res.geo <-
  ggplot(data.res.geo) +
  geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", size = 0.3) +
  geom_sf(data = poly[[region]]$areas, fill = "grey65", colour = NA) +
  geom_raster(mapping = aes(fill = as.numeric(value),
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = FALSE) +
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.15) +
  scale_fill_continuous_sequential(palette = "Viridis",
                               rev = FALSE,
                               limits = c(0, 1),
                               ) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]],
           label_axes = "-NE-") +
  scale_x_continuous(breaks = seq(-170, 0, 10)) +
  scale_y_continuous(breaks = seq(-80, 30, 10)) +
  facet_grid(cols = vars(res.lab)) +
  annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                   style = "ticks", text_cex = 0.5,
                   line_width = 0.5, text_family = "IBMPlexSansCondensed") +
  map_guide_fill +
  labs(x = NULL, y = NULL, fill = "DHARMa residual") +
  map_theme

png(file.fig.res.geo, width = 7, height = 2.25, unit = "in", res = 600)
p.res.geo + plot_annotation(subtitle = p.title, theme = map_theme)
dev.off()


data.res.som <-
  data.res[,
           .(res.med = median(residual),
             res.q25 = quantile(residual, 0.25),
             res.q75 = quantile(residual, 0.75)),
           by = .(som_x, som_y)] |>
  melt(measure.vars = c("res.med", "res.q25", "res.q75"),
       variable.name = "res.quant",
       value.name = "value")
data.res.som[, res.lab := res.lab[as.character(res.quant)]]

p.res.som <-
  ggplot(data.res.som) +
  geom_raster(mapping = aes(fill = as.numeric(value),
                            x = som_x, y = som_y),
              interpolate = FALSE) +
  scale_fill_continuous_sequential(palette = "Viridis",
                               rev = FALSE,
                               limits = c(0, 1),
                               ) +
  coord_fixed() +
  scale_x_continuous(limits = c(1, 100)) +
  scale_y_continuous(limits = c(1, 100)) +
  facet_grid(cols = vars(res.lab)) +
  map_guide_fill +
  labs(x = NULL, y = NULL, fill = "DHARMa residual") +
  map_theme +
  theme(panel.grid = element_blank())

png(file.fig.res.som, width = 7, height = 2.25, unit = "in", res = 600)
p.res.som + plot_spacer() +
  plot_layout(widths = c(6, 1)) +
  plot_annotation(subtitle = p.title, theme = map_theme)
dev.off()



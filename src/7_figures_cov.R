args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(ggplot2)
library(ggdist)
library(patchwork)
library(colorspace)
library(stringi)
library(units)

source("utilities.R")

region <- tolower(as.character(args[1]))
model_resp <- tolower(as.character(args[2]))


# region <- "cam"
# model_resp <- "def"


path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.agg <- paste0(path.agg, region, ".", model_resp, ".cov.rds")

file.fig.cov <- paste0(path.figures, region, ".cov.", model_resp, ".png")

cov <-
  c("elevation", "slope", "sx", "cmi_min",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_pop", "dens_roads", "travel_time")
cov.labs <-
  c("Elevation\n[m]", "Slope\n[deg]",
    "Agricultural suitability\nindex", "Minimum climate moisture\nindex [kg m⁻² month⁻¹]",
    "Distance to built-up areas\n[m]", "Distance to roads\n[m]", "Distance to rivers\n[m]",
    "Population density\n[km⁻¹]", "Road density\n[m km⁻¹]",
    "Travel time to cities\n[min]")
cov.labs <- factor(cov.labs, levels = cov.labs)
names(cov.labs) <- cov


base.size <- 7
cov_theme <-
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
        legend.title = element_text(size = rel(0.9),
                                    margin = margin(b = base.size/2)),
        legend.position = "right",
        legend.justification = "center",
        legend.key.size = unit(base.size, "pt"),
        legend.text = element_text(size = rel(0.8), margin = margin(l = base.size/3,
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


data <- readRDS(file.data)

cov.sum <- readRDS(file.agg)
# cov.sum[cov %in% c("dens_pop", "dens_roads", "travel_time") &
#         cov.val < .Machine$double.eps,
#         cov.val := .Machine$double.eps]
cov.sum[, cov.label := cov.labs[cov]]
cov.var <- unique(cov.sum$cov)


# Rug for percentiles
cov.rug <-
  data[, lapply(.SD, quantile, probs = 1:100/100), .SDcols = cov.var] |>
  melt(measure.vars = cov.var,
       variable.name = "cov",
       value.name = "cov.val")
cov.rug[, cov.label := cov.labs[as.character(cov)]]


cov.scales <-
  list(
       elevation = list(
                        # trans = "identity",
                        trans = scales::log_trans(),
                        breaks = c(10, 100, 1000),
                        labels = scales::label_number(big.mark = " "),
                        lim = c(3, 3000)),
       slope = list(
                    trans = "identity",
                    breaks = waiver(),
                    labels = scales::label_number(big.mark = " "),
                    lim = c(NA, 45)),
       sx = list(
                 trans = "identity",
                 breaks = waiver(),
                 labels = scales::label_number(big.mark = " "),
                 lim = c(NA, NA)),
       cmi_min = list(
                      trans = "identity",
                      breaks = waiver(),
                      labels = scales::label_number(big.mark = " "),
                       lim = c(NA, NA)),
       dist_set = list(trans = scales::log_trans(),
                       breaks = c(1, 10, 100, 1000, 10000),
                       labels = scales::label_number(big.mark = " "),
                       lim = c(500, NA)),
       dist_roads = list(trans = scales::log_trans(),
                         breaks = c(1, 10, 100, 1000, 10000),
                         labels = scales::label_number(big.mark = " "),
                         lim = c(100, NA)),
       dist_rivers = list(trans = scales::log_trans(),
                          breaks = c(30, 100, 300, 1000, 3000),
                          labels = scales::label_number(big.mark = " "),
                          lim = c(100, 3000)),
       dens_pop = list(trans = scales::log_trans(),
                       # breaks = unique(round(exp(seq(0, log(cov.sum[cov == "dens_pop", max(cov.val)]),
                       #                               length.out = 4))/50) * 50),
                       breaks = c(1, 10, 100),
                       labels = scales::label_number(big.mark = " "),
                       lim = c(.5, 100)),
       dens_roads = list(trans = scales::log_trans(),
                         breaks = c(1, 10, 100, 1000),
                         labels = scales::label_number(big.mark = " "),
                         lim = c(1, 1000)),
       travel_time = list(trans = scales::log_trans(),
                          breaks = c(10, 100, 1000),
                          labels = scales::label_number(big.mark = " "),
                          lim = c(10, NA))
  )


quant.sel <- 100

plots.cov <- list()

for(i in seq_along(cov.var)) {
  cov.title <- cov.labs[cov.var[i]]
  plots.cov[[i]] <-
    ggplot(cov.sum[n.quant == quant.sel & cov == cov.var[i]]) +
      geom_hline(yintercept = 0, linewidth = 0.1, linetype = "dashed", colour = "grey35") +
      stat_lineribbon(aes(x = cov.val, y = lp.bin),
                      linewidth = 0.1,
                      .width = c(0.5, 0.9, 0.97),
                      point_interval = median_hdci) +
      geom_rug(data = cov.rug[cov == cov.var[i]],
               aes(x = cov.val), sides = "b", alpha = 0.3,
               length = unit(base.size, "pt")) +
      scale_x_continuous(
                         # trans = scales::log_trans(),
                         trans = cov.scales[[cov.var[i]]]$trans,
                         breaks = cov.scales[[cov.var[i]]]$breaks,
                         labels = cov.scales[[cov.var[i]]]$labels,
                         ) +
      scale_fill_brewer(labels = rev(c("0.50", "0.90", "0.97"))) +
      facet_wrap(vars(cov.label), scales = "free_x") +
      # coord_cartesian(xlim = cov.scales[[cov.var[i]]]$lim) +
      labs(x = "", y = "Contribution to the linear predictor",
           fill = "Uncertainty\ninterval") +
      cov_theme
}

p.cov <-
  wrap_plots(plots.cov, ncol = 5, axes = "collect_y",
             guides = "collect") &
  theme(plot.margin = margin(base.size/2, base.size/2, base.size/2, base.size/2))

png(file.fig.cov, width = 7, height = 3.5, unit = "in", res = 600)
p.cov
dev.off()

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
hurr_type <- tolower(as.character(args[2]))
overwrite <- as.logical(as.character(args[3]))

# region <- "amz"
# hurr_type <- "hurr"
# overwrite <- TRUE

# hurr_type <- "no_hurr"
# overwrite <- FALSE


if(is.na(overwrite)) {
  overwrite <- FALSE
}


path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.cf <- paste0(path.base, "models/cf/", region, "/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.tables <- paste0(path.base, "results/tables/")
if(!dir.exists(path.tables)) dir.create(path.tables, recursive = TRUE)


if(is.na(hurr_type)) {
  hurr_type <- "hurr"
}
if(hurr_type == "no_hurr") {
  hurr_suf <- ".no_hurr"
} else {
  hurr_suf <- ""
}

file.data.proc <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.cf.ten.itpa <- paste0(path.cf, region, ".ten.itpa.all", hurr_suf, ".rds")
file.fig.imb.itpa <- paste0(path.figures, "imb.itpa.", region, hurr_suf, ".png")
# file.data.vis <- paste0(path.data.vis, region, "imb.itpa", hurr_suf, ".rds")


cat.lab <- 
  data.table(cat.label = c("Reference",
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
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]

cov <-
  c("elevation", "slope", "sx", "cmi_min",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_pop", "dens_roads", "travel_time")

cov.labs <-
  c("Elevation", "Slope",
    "Agricultural suitability", "Climate moisture index",
    "Distance to roads", "Distance to built-up areas", "Distance to rivers",
    "Population density", "Road density",
    "Travel time to cities")
cov.labs <- factor(cov.labs, levels = cov.labs)
names(cov.labs) <- cov

img.measures <- c("d_cohen", "var_ratio", "ks_stat")

imb.measure.labs <- c("Standardized mean difference", "Variance ratio (logarithm)", "Kolmogorov–Smirnov statistic")
imb.measure.labs <- factor(imb.measure.labs, levels = imb.measure.labs)
names(imb.measure.labs) <- img.measures

imb.type.labs <- c(raw = "Raw", effective = "Under EGPA")
imb.type.labs <- factor(imb.type.labs, levels = imb.type.labs)


col.imb <- c("#e41a1c", "#377eb8")
names(col.imb) <- imb.type.labs


# base.size <- 7 
base.size <- 7
imb_theme <-
  theme_light(base_family = "IBMPlexSansCondensed",
              base_size = base.size) +
  theme(
        axis.line.x = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.line.y = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.title.x = element_text(size = rel(1.1),
                                    lineheight = rel(1.15),
                                    margin = margin(t = base.size)),
        axis.title.y = element_text(size = rel(1.1),
                                    margin = margin(r = 1.5*base.size)),
        axis.text.x = element_text(color = "black",
                                   size = rel(1),
                                   margin = margin(t = base.size/2)),
        axis.text.y = element_text(color = "black",
                                   size = rel(0.9),
                                   lineheight = rel(1),
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
        # panel.spacing.x = unit(base.size*2, "pt"),
        panel.spacing.y = unit(base.size, "pt"),
        plot.margin = margin(3, 3, 3, 3),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0,
                                  # face = "bold",
                                  face = "plain",
                                  size = rel(1.5),
                                  margin = margin(l = 0, b = base.size*2, t = base.size/3)),
        plot.subtitle = element_text(size = rel(1.35),
                                     margin = margin(b = 14)),
        plot.tag = element_text(face = "bold"),
        strip.text = element_text(size = rel(0.8),
                                  lineheight = rel(1.15),
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



data.proc <- readRDS(file.data.proc)
cf.ten.itpa <- readRDS(file.cf.ten.itpa)

group.idx <- cf.ten.itpa$groups[is.na(adm0) & !is.na(it_type) & !is.na(pa_type), group.id]
group.cols <- with(cf.ten.itpa, c(group.var, group.by.c))

itpa.imb <-
  egp_imbalance(data.proc,
                variables = cov,
                cf = cf.ten.itpa,
                group = group.idx,
                measure = img.measures)

itpa.imb <-
  merge(cat.lab,
        cf.ten.itpa$groups[group.id %in% group.idx, ..group.cols], sort = FALSE) |>
  merge(itpa.imb)

itpa.imb[, cov.label := cov.labs[as.character(.variable)]]
itpa.imb[, imb.measure.label := imb.measure.labs[as.character(.measure)]]

itpa.imb.l <-
  melt(itpa.imb,
       measure.vars = c("raw", "effective"), 
       variable.name = "imb.type",
       value.name = "imbalance")

itpa.imb.l[, imb.type.label := imb.type.labs[as.character(imb.type)]]


lim.smd <- c(-1, 1) * rep(itpa.imb.l[.measure == "d_cohen", max(abs(range(imbalance)))], 2)
lim.vr <- c(-1, 1) * rep(itpa.imb.l[.measure == "var_ratio", max(abs(range(log(imbalance))))], 2)
lim.ks <- c(0, 1)

plots.ten.itpa <- list()

plots.ten.itpa$smd <-
  ggplot(itpa.imb.l[.measure == "d_cohen"]) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey65") +
    geom_linerange(data = itpa.imb[.measure == "d_cohen"],
                   aes(xmin = raw, xmax = effective, y = cov.label),
                   colour = "grey35") +
    geom_point(aes(x = imbalance, y = cov.label,
                   colour = imb.type.label),
               size = 0.8, shape = 19) +
    scale_colour_manual(values = col.imb) +
    scale_x_continuous(limits = lim.smd, expand = expansion(mult = 0.1)) +
    scale_y_discrete(limits = rev) +
    facet_grid(rows = vars(cat.label), cols = vars(imb.measure.label)) +
    labs(colour = "Condition", y = "", x = "Imbalance") +
    imb_theme

plots.ten.itpa$vr <-
  ggplot(itpa.imb.l[.measure == "var_ratio"]) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey65") +
    geom_linerange(data = itpa.imb[.measure == "var_ratio"],
                   aes(xmin = log(raw), xmax = log(effective), y = cov.label),
                   colour = "grey35") +
    geom_point(aes(x = log(imbalance), y = cov.label,
                   colour = imb.type.label),
               size = 0.8, shape = 19) +
    scale_colour_manual(values = col.imb) +
    scale_x_continuous(limits = lim.vr,
                       expand = expansion(mult = 0.1)) +
    scale_y_discrete(limits = rev) +
    facet_grid(rows = vars(cat.label), cols = vars(imb.measure.label)) +
    labs(colour = "Condition", y = "", x = "Imbalance") +
    imb_theme

plots.ten.itpa$ks <-
  ggplot(itpa.imb.l[.measure == "ks_stat"]) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey65") +
    geom_linerange(data = itpa.imb[.measure == "ks_stat"],
                   aes(xmin = raw, xmax = effective, y = cov.label),
                   colour = "grey35") +
    geom_point(aes(x = imbalance, y = cov.label,
                   colour = imb.type.label),
               size = 0.8, shape = 19) +
    scale_colour_manual(values = col.imb) +
    scale_x_continuous(limits = lim.ks, expand = expansion(mult = 0.1)) +
    scale_y_discrete(limits = rev) +
    facet_grid(rows = vars(cat.label), cols = vars(imb.measure.label)) +
    labs(colour = "Condition", y = "", x = "Imbalance") +
    imb_theme


p.imb.ten.itpa <-
  (plots.ten.itpa$smd +
   theme(strip.text.y = element_blank(),
         plot.margin = margin(r = 5))) +
  (plots.ten.itpa$vr +
   theme(strip.text.y = element_blank(),
         plot.margin = margin(l = 5, r = 5))) +
  (plots.ten.itpa$ks +
   theme(plot.margin = margin(l = 5))) +
  plot_layout(axes = "collect", guides = "collect")


png(file.fig.imb.itpa, width = 7, height = 8.5, unit = "in", res = 600)
  p.imb.ten.itpa
dev.off()



args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(ggplot2)
library(ggdist)
library(patchwork)
library(colorspace)
library(stringi)
library(units)

source("utilities.R")

hurr_type <- tolower(as.character(args[1]))
overwrite <- as.logical(as.character(args[2]))

# hurr_type <- "hurr"
# hurr_type <- "no_hurr"
# overwrite <- TRUE
# overwrite <- FALSE


if(is.na(overwrite)) {
  overwrite <- FALSE
}

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.marginal <- paste0(path.base, "models/marginal/")
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

file.data.vis <- paste0(path.data.vis, "tenure_adm", hurr_suf, ".rds")
file.fig.ten <- paste0(path.figures, "ten.full", hurr_suf, ".png")
file.fig.ten.reg <- paste0(path.figures, "ten.full.reg", hurr_suf, ".png")
file.fig.ten.reg.av <- paste0(path.figures, "ten.full.reg.av", hurr_suf, ".png")
file.fig.ten.area <- paste0(path.figures, "ten.full.area", hurr_suf, ".png")
file.fig.ten.av <- paste0(path.figures, "ten.full.av", hurr_suf, ".png")
file.fig.ten.def <- paste0(path.figures, "ten.full.def", hurr_suf, ".png")
file.fig.ten.deg <- paste0(path.figures, "ten.full.deg", hurr_suf, ".png")
file.fig.ten.def.av <- paste0(path.figures, "ten.full.def.av", hurr_suf, ".png")
file.fig.ten.deg.av <- paste0(path.figures, "ten.full.deg.av", hurr_suf, ".png")
file.fig.ten.post.def.cam <- paste0(path.figures, "ten.full.post.def.cam", hurr_suf, ".png")
file.fig.ten.post.deg.cam <- paste0(path.figures, "ten.full.post.deg.cam", hurr_suf, ".png")
file.fig.ten.post.def.amz <- paste0(path.figures, "ten.full.post.def.amz.png")
file.fig.ten.post.deg.amz <- paste0(path.figures, "ten.full.post.deg.amz.png")
file.fig.ten.av.post.def.cam <- paste0(path.figures, "ten.full.av.post.def.cam", hurr_suf, ".png")
file.fig.ten.av.post.deg.cam <- paste0(path.figures, "ten.full.av.post.deg.cam", hurr_suf, ".png")
file.fig.ten.av.post.def.amz <- paste0(path.figures, "ten.full.av.post.def.amz.png")
file.fig.ten.av.post.deg.amz <- paste0(path.figures, "ten.full.av.post.deg.amz.png")
file.table.ten <- paste0(path.data.vis, "tenure_adm", hurr_suf, ".csv")

regions <- c("amz", "cam")


## COLOURS AND LABELS

## Colours for tenure categories
# col.est <- c("#D55E00", "#0072B2", "#CC79A7")
# col.est <- c("#D55E00", "#0072B2", "#E69F00")
col.est <- c("#e41a1c", "#377eb8", "#984ea3")
names(col.est) <- c("Factual\n(observed)", "Counterfactual\n(reference)", "Marginal\ndifference")
col.est.pt <- col.est
col.est.pt[2] <- "#FFFFFF"
size.est.pt <- c(0.2, 0.4, 0.3)
names(size.est.pt) <- names(col.est)
size.est.pt.comp <- c(0.15, 0.3, 0.225)
names(size.est.pt.comp) <- names(col.est)
stroke.pt.comp <- 0.5
linewidth.comp <- 0.3

# Angle for tenure categories in posterior summaries
ten.cat.angle <- 45

ten_theme <- 
  theme_minimal(base_family = "IBMPlexSans", base_size = 7) +
  theme(
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.justification = c(0,1),
        legend.title = element_text(size = rel(0.75), hjust = 0,
                                    margin = margin(t = 0, b = 7),
                                    lineheight = rel(1.15)),
        legend.text = element_text(size = rel(0.65)),
        legend.spacing.y = unit(2, "pt"),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0,
                                   colour = "grey5"),
        axis.text.y = element_text(hjust = 0, colour = "grey5"),
        plot.tag = element_text(margin = margin(t = 9, r = 0, b = 3, l = 0),
                                size = rel(1.5)),
        plot.tag.location = "margin",
        plot.title = element_text(size = rel(1)),
        plot.subtitle = element_text(size = rel(0.85), margin = margin(t = 6, b = 3))
        )


# theme_minimal(base_family = "IBMPlexSans") +
#       theme(
#             legend.position = "right",
#             legend.justification = c(0,1),
#             legend.spacing.y = unit(5, "mm"),
#             legend.title = element_text(size = rel(0.75)),
#             legend.text = element_text(size = rel(0.75)),
#             axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0,
#                                        colour = "grey5"),
#             axis.text.y = element_text(hjust = 0, colour = "grey5"),
#             # panel.background = element_rect(fill = "grey85", colour = "grey85", size = 2),
#             panel.grid.major = element_blank()
#             # axis.line = element_line(colour = "grey85")

ten_guide_fill <-
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


base.size <- 7 
post_theme <-
  theme_light(base_family = "IBMPlexSans",
              base_size = base.size) +
  theme(
        axis.line.x = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.line.y = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.title.x = element_text(size = rel(1),
                                    lineheight = rel(1.15),
                                    margin = margin(t = base.size/2)),
        axis.title.y = element_text(size = rel(1),
                                    margin = margin(r = 0.75*base.size),
                                    lineheight = rel(1.15)),
        axis.text.x = element_text(color = "black",
                                   size = rel(1.1),
                                   margin = margin(t = base.size/2)),
        axis.text.y = element_text(color = "black",
                                   size = rel(1),
                                   lineheight = rel(1.15),
                                   margin = margin(r = base.size/2)),
        axis.ticks = element_line(colour = "grey30"),
        legend.title = element_text(size = rel(1),
                                    margin = margin(b = base.size/3)),
        legend.position = "right",
        legend.justification = "center",
        # legend.key.size = unit(base.size*1.5, "pt"),
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
        plot.tag = element_text(face = "plain", margin = margin(r = base.size/2)),
        strip.text = element_text(size = rel(1),
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



# Labels for tenure categories and administrative regions

cat.lab <- 
  data.table(cat.label = c("Reference",
                            "IL, recognized", "IL, not recognized",
                            "PA, category I-IV", "PA, category V-VI",
                            "IL, rec.; PA, cat. I-IV",
                            "IL, rec.; PA, cat. V-VI",
                            "IL, not rec.; PA, cat. I-IV",
                            "IL, not rec.; PA, cat. V-VI"),
                            # "IL, recognized &\nPA, category I-IV",
                            # "IL, recognized &\nPA, category V-VI",
                            # "IL, not recognized &\nPA, category I-IV",
                            # "IL, not recognized &\nPA, category V-VI"),
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
# cat.lab[, cat.label := stri_pad_left(cat.label, width = max(stri_width(cat.label)))]
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]

reg.lab <- list()
reg.lab$amz <-
  data.table(adm0 = c("BOL", "BRA", "COL", "ECU",
                      "GUF", "GUY", "PER", "SUR",
                      "VEN", NA),
             reg.label = c("Bolivia", "Brazil", "Colombia", "Ecuador",
                           "French Guiana", "Guyana", "Peru", "Suriname",
                           "Venezuela", 
                           # "Amazon")
                           "Region")
             )
reg.lab$cam <-
  data.table(adm0 = c("BLZ", "CRI", "GTM", "HND",
                      "MEX", "NIC", "PAN", "SLV",
                      NA),
             reg.label = c("Belize", "Costa Rica", "Guatemala", "Honduras",
                           "Mexico", "Nicaragua", "Panama", "El Salvador",
                           # "Central America")
                           "Region")
             )
reg.lab$amz[, reg.label := factor(reg.label, levels = reg.label)]
reg.lab$cam[, reg.label := factor(reg.label, levels = reg.label)]

dist.lab <-
  data.table(dist_type = c("def", "deg"),
             dist.label = c("Long-term disturbance (deforestation)",
                            "Short-term disturbance (not followed by deforestation)"))
dist.lab[, dist.label := factor(dist.label, levels = dist.label)]


wrap_title <- function(x, width = 25, ...) {
  paste(stri_wrap(x,
                  width = width,
                  whitespace_only = TRUE),
        collapse = "\n")
}

area.title <- wrap_title("Area covered by least-disturbed TMF in 2010 (km²)")
# mar.title <- wrap_title("Marginal avoided disturbances 2011–2020 (proportion of least-disturbed TMF)")
mar.title.def.l <- "Absolute marginal difference in proportion of area affected"
mar.title.def.2l <- "Absolute marginal difference in\nproportion of area affected"
mar.title.def <- wrap_title(mar.title.def.l)
mar.title.deg.l <- "Absolute marginal difference in proportion of area affected"
mar.title.deg.2l <- "Absolute marginal difference in\nproportion of area affected"
mar.title.deg <- wrap_title(mar.title.deg.l)
mar.title.def.av.l <- "Absolute marginal difference in area affected by disturbances (km²)"
mar.title.def.av.2l <- "Absolute marginal difference in\narea affected by disturbances (km²)"
mar.title.def.av <- wrap_title(mar.title.def.av.l)
mar.title.deg.av.l <- "Absolute marginal difference in area affected by disturbances (km²)"
mar.title.deg.av.2l <- "Absolute marginal difference in\narea affected by disturbances (km²)"
mar.title.deg.av <- wrap_title(mar.title.deg.av.l)
abs.title.def <- "Proportion of area affected"
abs.title.deg <- "Proportion of area affected"
abs.area.title.def <- "Area affected (km²)"
abs.area.title.deg <- "Area affected (km²)"
size.mar.lab <- 1.5



## EFFECT OF TENURE BY ADMINISTRATIVE AREA, FOREST TYPE ########################

if(!file.exists(file.data.vis) | overwrite == TRUE) {

  ten.sum <- list()

  for(i in seq_along(regions)) {
    
    region <- regions[i]

    message(paste0("Preparing data for region `", region, "` …"))
   
    if(region == "cam") {
      hurr_suf_mar <- hurr_suf
    } else {
      hurr_suf_mar <- ""
    }

    file.area <- paste0(path.data.proc, region, ".sumstats.area.rds")
    file.mar.sam <- paste0(path.marginal, region, "/", region, ".sam.rds")
    name.mar.ten.sam <- paste0(region, ".ten.itpa.all.rds")
    name.mar.ten.def <- paste0(region, ".def.ten.itpa.all", hurr_suf_mar, ".rds")
    file.mar.ten.def <- paste0(path.marginal, region, "/", name.mar.ten.def)
    name.mar.ten.deg <- paste0(region, ".deg.ten.itpa.all", hurr_suf_mar, ".rds")
    file.mar.ten.deg <- paste0(path.marginal, region, "/", name.mar.ten.deg)

    # Tenure categories by administrative areas

    message("Tenure category by administrative areas …")

    mar.sam <- readRDS(file.mar.sam)

    area.undist <-
      readRDS(file.area)$undist

    mar.ten.def <-
      readRDS(file.mar.ten.def) |>
      merge(mar.sam[[name.mar.ten.sam]])

    mar.ten.deg <-
      readRDS(file.mar.ten.deg) |>
      merge(mar.sam[[name.mar.ten.sam]])

    mar.ten.def[, dist_type := factor("def", levels = c("def", "deg"))]
    mar.ten.deg[, dist_type := factor("deg", levels = c("def", "deg"))]

    # Effect summaries. Combinations of tenure category and country with < 1000
    # observations are excluded.

    mar.post <-
      rbind(mar.ten.deg, mar.ten.def) |>
      merge(area.undist[, -"area.rel"])
    mar.post[, `:=`(area.prop.mar = marginal * area,
                    area.prop.fac = factual * area,
                    area.prop.cf = counterfactual * area)]

    mar.ten <-
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
                 fac.q5 = quantile(factual, 0.05),
                 fac.q25 = quantile(factual, 0.25),
                 fac.q75 = quantile(factual, 0.75),
                 fac.q95 = quantile(factual, 0.95),
                 cf.mean = mean(counterfactual),
                 cf.median = median(counterfactual),
                 cf.sd = sd(counterfactual),
                 cf.mad = mad(counterfactual),
                 cf.q5 = quantile(counterfactual, 0.05),
                 cf.q25 = quantile(counterfactual, 0.25),
                 cf.q75 = quantile(counterfactual, 0.75),
                 cf.q95 = quantile(counterfactual, 0.95),
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
               by = c("dist_type", "it_type", "pa_type", "adm0")]
    # mar.ten[, mar.lab.mean := label_arc(mar.mean, 1, FALSE)]
    mar.ten[, mar.lab.mean := label_perc(mar.mean, 1, FALSE)]
    mar.ten[, mar.lab.median := label_perc(mar.median, 1, FALSE)]
    mar.ten[,
            area.mar.mean.log :=
              sign(as.numeric(area.mar.mean)) *
              log10(abs(as.numeric(area.mar.mean)))]
    mar.ten[,
            area.mar.median.log :=
              sign(as.numeric(area.mar.median)) *
              log10(abs(as.numeric(area.mar.median)))]

    # Mark where 90% CI includes 0:
    mar.ten[, ci_0 := ifelse(mar.q5 < 0 & mar.q95 > 0, "yes", "no")]
    mar.ten[ci_0 == "yes", mar.lab.mean := paste0("(", mar.lab.mean, ")")]
    mar.ten[ci_0 == "yes", mar.lab.median := paste0("(", mar.lab.median, ")")]
    mar.ten[, mar.lab.shade := ifelse(abs(mar.median) < 0.1, "dark", "light")]

    ten.sum[[region]]$mar <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label,
                  dist.label = dist.lab$dist.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(dist.lab, by = "dist.label", all = TRUE) |> 
      merge(mar.ten[n.fac > 1000], by = c("dist_type", "it_type", "pa_type", "adm0"),
            all.x = TRUE)


    ten.sum[[region]]$post <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label,
                  dist.label = dist.lab$dist.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(dist.lab, by = "dist.label", all = TRUE) |> 
      merge(mar.post[n.fac > 1000], by = c("dist_type", "it_type", "pa_type", "adm0"),
            all.x = TRUE)

    ten.sum[[region]]$area.undist <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(area.undist, by = c("it_type", "pa_type", "adm0"), all.x = TRUE)

  }

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(ten.sum = ten.sum) |>
  saveRDS(file.data.vis)


} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## AREA OF PREVIOUSLY UNDISTURBED TROPICAL MOIST FOREST ########################

plots.area <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Area of previously undisturbed TMF …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  breaks.area <- 10^(0:7)


  plots.area[[region]]$undist.nov <-
    ten.sum[[region]]$area.undist[((it_type == "none"  & pa_type != "none") |
                                   (it_type != "none"  & pa_type == "none"))] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = as.numeric(area)),
                linewidth = 1, colour = "white") +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_binned_sequential(
                                   palette = "YlGn",
                                   # palette = "Greens",
                                   rev = TRUE,
                                   lim = c(1, 1e7),
                                   breaks = breaks.area,
                                   trans = scales::log10_trans(),
                                   labels = scales::label_log(),
                                   na.value = "grey95"
                                       ) +
      scale_discrete_manual("fontface", values = c(yes = "italic", no = "plain"),
                            guide = "none") +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = "Tropical moist forest",
           subtitle = reg.title,
           fill = area.title,
           y = "Non-overlapping regimes\n", x = NULL) +
      ten_theme

  plots.area[[region]]$undist.ov <-
    ten.sum[[region]]$area.undist[it_type != "none"  & pa_type != "none"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = as.numeric(area)),
                linewidth = 1, colour = "white") +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_binned_sequential(
                                   palette = "YlGn",
                                   rev = TRUE,
                                   lim = c(1, 1e7),
                                   breaks = breaks.area,
                                   trans = scales::log10_trans(),
                                   labels = scales::label_log(),
                                   na.value = "grey95"
                                   ) +
      scale_discrete_manual("fontface", values = c(yes = "italic", no = "plain"),
                            guide = "none") +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = "Tropical moist forest",
           subtitle = reg.title,
           fill = area.title,
           y = "Non-overlapping regimes\n", x = NULL) +
      ten_theme

}


## AVOIDED DISTURBANCES IN TERMS OF AREA TROPICAL MOIST FOREST #################

plots.area.av <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  breaks.area.av <- (-4:4)
  # labels.area.av <- expression(-10^4, -10^3, -10^2, -10^1, 10^0, 10^1, 10^2, 10^3, 10^4)
  labels.area.av <- scales::label_number()(10^abs(breaks.area.av) * sign(breaks.area.av))
  labels.area.av[which(breaks.area.av > 0)] <- paste0("+", labels.area.av[which(breaks.area.av > 0)])
  limits.area.av <- log10(c(1e5, 1e5))*c(-1, 1)

  plots.area.av[[region]]$ten.mar$def.nov <-
    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = area.mar.median.log),
                linewidth = 1, colour = "white") +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_binned_divergingx(
                                       palette = "Roma",
                                       rev = TRUE,
                                       breaks = breaks.area.av,
                                       labels = labels.area.av,
                                       limits = limits.area.av,
                                       oob = scales::squish,
                                       na.value = "grey95"
                                       ) +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = dist.lab[dist_type == "def", dist.label],
           subtitle = reg.title,
           fill = mar.title.def.av,
           y = "Non-overlapping regimes\n", x = NULL) +
      ten_theme

  plots.area.av[[region]]$ten.mar$deg.nov <-
    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = area.mar.median.log),
                linewidth = 1, colour = "white") +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_binned_divergingx(
                                       palette = "Roma",
                                       rev = TRUE,
                                       breaks = breaks.area.av,
                                       labels = labels.area.av,
                                       limits = limits.area.av,
                                       oob = scales::squish,
                                       na.value = "grey95"
                                       ) +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = dist.lab[dist_type == "deg", dist.label],
           subtitle = reg.title,
           fill = mar.title.deg.av,
           y = "Non-overlapping regimes\n", x = NULL) +
      ten_theme

  plots.area.av[[region]]$ten.mar$def.ov <-
    ten.sum[[region]]$mar[it_type != "none"  & pa_type != "none" &
                          dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = area.mar.median.log),
                linewidth = 1, colour = "white") +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_binned_divergingx(
                                       palette = "Roma",
                                       rev = TRUE,
                                       breaks = breaks.area.av,
                                       labels = labels.area.av,
                                       limits = limits.area.av,
                                       oob = scales::squish,
                                       na.value = "grey95"
                                       ) +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = dist.lab[dist_type == "def", dist.label],
           subtitle = reg.title,
           fill = mar.title.def.av,
           y = "Overlapping regimes\n", x = NULL) +
      ten_theme

  plots.area.av[[region]]$ten.mar$deg.ov <-
    ten.sum[[region]]$mar[it_type != "none"  & pa_type != "none" &
                          dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = area.mar.median.log),
                linewidth = 1, colour = "white") +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_binned_divergingx(
                                       palette = "Roma",
                                       rev = TRUE,
                                       breaks = breaks.area.av,
                                       labels = labels.area.av,
                                       limits = limits.area.av,
                                       oob = scales::squish,
                                       na.value = "grey95"
                                       ) +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = dist.lab[dist_type == "deg", dist.label],
           subtitle = reg.title,
           fill = mar.title.deg.av,
           y = "Overlapping regimes\n", x = NULL) +
      ten_theme

}




## TENURE EFFECTS BY COUNTRY ###################################################

plots.dis <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  breaks.dis <- round(seq(-0.15, 0.15, 0.05),2)
  limits.dis <- c(-0.15, 0.15)

  plots.dis[[region]]$ten.mar$def.nov <-
    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.median),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.median,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = size.mar.lab) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = breaks.dis
                                       ,labels = label_perc
                                       ,limits = limits.dis
                                       ,oob = scales::squish
                                       ,na.value = "grey95"
                                       ) +
      scale_discrete_manual("fontface", values = c(yes = "italic", no = "plain"),
                            guide = "none") +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = dist.lab[dist_type == "def", dist.label],
           subtitle = reg.title,
           fill = mar.title.def,
           y = "Non-overlapping regimes\n", x = NULL) +
      ten_theme

  plots.dis[[region]]$ten.mar$deg.nov <-
    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.median),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.median,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = size.mar.lab) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = breaks.dis
                                       ,labels = label_perc
                                       ,limits = limits.dis
                                       ,oob = scales::squish
                                       ,na.value = "grey95"
                                       ) +
      scale_discrete_manual("fontface", values = c(yes = "italic", no = "plain"),
                            guide = "none") +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = dist.lab[dist_type == "deg", dist.label],
           subtitle = reg.title,
           fill = mar.title.deg,
           y = "Non-overlapping regimes\n", x = NULL) +
      ten_theme

  plots.dis[[region]]$ten.mar$def.ov <-
    ten.sum[[region]]$mar[it_type != "none"  & pa_type != "none" &
                          dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.median),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.median,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = size.mar.lab) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = breaks.dis
                                       ,labels = label_perc
                                       ,limits = limits.dis
                                       ,oob = scales::squish
                                       ,na.value = "grey95"
                                       ) +
      scale_discrete_manual("fontface", values = c(yes = "italic", no = "plain"),
                            guide = "none") +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = dist.lab[dist_type == "def", dist.label],
           subtitle = reg.title,
           fill = mar.title.def,
           y = "Overlapping regimes\n", x = NULL) +
      ten_theme

  plots.dis[[region]]$ten.mar$deg.ov <-
    ten.sum[[region]]$mar[it_type != "none"  & pa_type != "none" &
                          dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.median),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.median,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = size.mar.lab) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = breaks.dis
                                       ,labels = label_perc
                                       ,limits = limits.dis
                                       ,oob = scales::squish
                                       ,na.value = "grey95"
                                       ) +
      scale_discrete_manual("fontface", values = c(yes = "italic", no = "plain"),
                            guide = "none") +
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(title = dist.lab[dist_type == "deg", dist.label],
           subtitle = reg.title,
           fill = mar.title.deg,
           y = "Overlapping regimes\n", x = NULL) +
      ten_theme

}



## AVOIDED DISTURBANCES (POSTERIOR DISTRIBUTIONS) ##############################


ten.sum.av.l <- list()
plots.av.post <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  est.lab <- names(col.est)
  est.lab <- factor(est.lab, levels = est.lab)

  ten.sum.av.l[[region]] <-
    ten.sum[[region]]$mar |>
         melt(measure.vars = list(est.median = c("area.fac.median", "area.cf.median", "area.mar.median"),
                                  est.q5 = c("area.fac.q5", "area.cf.q5", "area.mar.q5"),
                                  est.q95 = c("area.fac.q95", "area.cf.q95", "area.mar.q95")),
             variable.name = "est_type")
  ten.sum.av.l[[region]][, est.label := est.lab[as.integer(est_type)]]
  ten.sum.av.l[[region]][, est.group := factor(fifelse(est_type == "3",
                                                    "Attributed effect",
                                                    "Comparison against reference"),
                                            levels = c("Comparison against reference",
                                                       "Attributed effect"))]

  reg.lev <- levels(ten.sum.av.l[[region]]$reg.label)
  reg.lev.new <- c(reg.title, reg.lev[1:(length(reg.lev)-1)])
  ten.sum.av.l[[region]][reg.label == "Region", reg.label := reg.title]
  ten.sum.av.l[[region]][, reg.label := factor(reg.label, levels = reg.lev.new)]
  col.to.num <- paste0("est.", c("median", "q5", "q95"))
  ten.sum.av.l[[region]][, (col.to.num) := lapply(.SD, as.numeric), .SDcols = col.to.num]

             
  plots.av.post[[region]]$def.comp <-
    ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") &
                        dist_type == "def" & est_type %in% c("1", "2")
                      ][order(-est.label)] |>
    ggplot() +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = 0.7) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt, guide = "none") +
      facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      labs(colour = "", fill = "", y = abs.area.title.def, x = "",
           title = "") +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1),
            axis.title.y = element_text(margin = margin(l = 0, r = 10)))

  plots.av.post[[region]]$def.mar <-
    ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") &
                        dist_type == "def" & est_type %in% c("3")
                      ][order(-est.label)] |>
    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = 0.7) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1),
                         expand = expansion(mult = c(0.2, 0.2))) +
      expand_limits(y = 1) +
      scale_size_manual(values = size.est.pt, guide = "none") +
      facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      labs(colour = "", fill = "", y = mar.title.def.av.l, x = "",
           title = "") +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.av.post[[region]]$deg.comp <-
    ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") &
                        dist_type == "deg" & est_type %in% c("1", "2")
                      ][order(-est.label)] |>
    ggplot() +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = 0.7) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt, guide = "none") +
      facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      labs(colour = "", fill = "", y = abs.area.title.deg, x = "",
           title = "") +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.av.post[[region]]$deg.mar <-
    ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") &
                        dist_type == "deg" & est_type %in% c("3")
                      ][order(-est.label)] |>
    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = 0.7) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1),
                         expand = expansion(mult = c(0.2, 0.2))) +
      expand_limits(y = 1) +
      scale_size_manual(values = size.est.pt, guide = "none") +
      facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      labs(colour = "", fill = "", y = mar.title.deg.av.l, x = "",
           title = "") +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))


  plots.av.post[[region]]$reg.def.comp <-
    ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
                        dist_type == "def" & est_type %in% c("1", "2")
                      ][order(-est.label)] |>
    ggplot() +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = stroke.pt.comp,
                          linewidth = linewidth.comp) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt.comp, guide = "none") +
      labs(colour = "", fill = "", y = abs.area.title.def, x = "",
           subtitle = reg.title) +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.av.post[[region]]$reg.def.mar <-
    ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
                        dist_type == "def" & est_type %in% c("3")
                      ][order(-est.label)] |>
    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = stroke.pt.comp,
                          linewidth = linewidth.comp) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt.comp, guide = "none") +
      labs(colour = "", fill = "", y = mar.title.def.av.2l, x = "",
           subtitle = reg.title) +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.av.post[[region]]$reg.deg.comp <-
    ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
                        dist_type == "deg" & est_type %in% c("1", "2")
                      ][order(-est.label)] |>
    ggplot() +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = stroke.pt.comp,
                          linewidth = linewidth.comp) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt.comp, guide = "none") +
      labs(colour = "", fill = "", y = abs.area.title.deg, x = "",
           subtitle = reg.title) +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.av.post[[region]]$reg.deg.mar <-
    ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
                        dist_type == "deg" & est_type %in% c("3")
                      ][order(-est.label)] |>
    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = stroke.pt.comp,
                          linewidth = linewidth.comp) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt.comp, guide = "none") +
      labs(colour = "", fill = "", y = mar.title.deg.av.2l, x = "",
           subtitle = reg.title) +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))


  #   ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
  #                          dist_type == "def" & est_type %in% c("1", "2")
  #                         ][order(-est.label)] |>
  #   ggplot() +
  #     geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
  #     geom_pointrange(aes(x = cat.label, y = est.median,
  #                         ymin = est.q5, ymax = est.q95,
  #                         group = est.label, fill = est.label,
  #                         colour = est.label),
  #                     size = 0.3, shape = 21, stroke = 0.7) +
  #     scale_fill_manual(values = col.est.pt, drop = TRUE) +
  #     scale_colour_manual(values = col.est, drop = TRUE) +
  #     scale_y_continuous(transform = "log10", label = scales::label_number(accuracy = 1)) +
  #     expand_limits(y = 0) +
  #     facet_grid(cols = vars(est.group), scales = "free") +
  #     labs(colour = "", fill = "", y = abs.area.title.def, x = "",
  #          title = "") +
  #     post_theme +
  #     theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1),
  #           axis.title.y = element_text(margin = margin(l = 0, r = 10)))

  # plots.av.post[[region]]$def.mar <-
  #   ten.sum.av.l[[region]][!(it_type == "none" & pa_type == "none") &
  #                       dist_type == "def" & est_type %in% c("3")
  #                     ][order(-est.label)] |>
  #   ggplot() +
  #     geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
  #     geom_pointrange(aes(x = cat.label, y = est.median,
  #                         ymin = est.q5, ymax = est.q95,
  #                         group = est.label, fill = est.label,
  #                         colour = est.label),
  #                     size = 0.3, shape = 21, stroke = 0.7) +
  #     scale_fill_manual(values = col.est.pt, drop = TRUE) +
  #     scale_colour_manual(values = col.est, drop = TRUE) +
  #     scale_y_continuous(label = scales::label_number(accuracy = 1)) +
  #     expand_limits(y = c(-0.01, 0.01)) +
  #     facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      # labs(colour = "", fill = "", y = mar.title.def.av.l, x = "",
      #      title = "") +
      # post_theme +
      # theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1),
      #       axis.title.y = element_text(margin = margin(l = 25, r = 10)))


}



## TENURE EFFECTS BY COUNTRY (POSTERIOR DISTRIBUTIONS) ##########################


ten.sum.l <- list()
plots.post <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  est.lab <- names(col.est)
  est.lab <- factor(est.lab, levels = est.lab)

  ten.sum.l[[region]] <-
    ten.sum[[region]]$mar |>
         melt(measure.vars = list(est.median = c("fac.median", "cf.median", "mar.median"),
                                  est.q5 = c("fac.q5", "cf.q5", "mar.q5"),
                                  est.q95 = c("fac.q95", "cf.q95", "mar.q95")),
             variable.name = "est_type")
  ten.sum.l[[region]][, est.label := est.lab[as.integer(est_type)]]
  ten.sum.l[[region]][, est.group := factor(fifelse(est_type == "3",
                                                    "Attributed effect",
                                                    "Comparison against reference"),
                                            levels = c("Comparison against reference",
                                                       "Attributed effect"))]


  reg.lev <- levels(ten.sum.l[[region]]$reg.label)
  reg.lev.new <- c(reg.title, reg.lev[1:(length(reg.lev)-1)])
  ten.sum.l[[region]][reg.label == "Region", reg.label := reg.title]
  ten.sum.l[[region]][, reg.label := factor(reg.label, levels = reg.lev.new)]

  plots.post[[region]]$def.comp <-
    ten.sum.l[[region]][!(it_type == "none" & pa_type == "none") &
                        dist_type == "def" & est_type %in% c("1", "2")
                      ][order(-est.label)] |>
    ggplot() +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = 0.7) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                      style_positive = "none", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt, guide = "none") +
      facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      labs(colour = "", fill = "", y = abs.title.def, x = "",
           title = "") +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))


  plots.post[[region]]$def.mar <-
    ten.sum.l[[region]][!(it_type == "none" & pa_type == "none") &
                        dist_type == "def" & est_type %in% c("3")
                      ][order(-est.label)] |>
    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = 0.7) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      # scale_y_continuous(label = scales::label_number(accuracy = 0.01),
      scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                      style_positive = "plus", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt, guide = "none") +
      facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      labs(colour = "", fill = "", y = mar.title.def.l, x = "",
           title = "") +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.post[[region]]$deg.comp <-
    ten.sum.l[[region]][!(it_type == "none" & pa_type == "none") &
                        dist_type == "deg" & est_type %in% c("1", "2")
                      ][order(-est.label)] |>
    ggplot() +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = 0.7) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                      style_positive = "none", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt, guide = "none") +
      facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      labs(colour = "", fill = "", y = abs.title.deg, x = "",
           title = "") +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.post[[region]]$deg.mar <-
    ten.sum.l[[region]][!(it_type == "none" & pa_type == "none") &
                        dist_type == "deg" & est_type %in% c("3")
                      ][order(-est.label)] |>
    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = 0.7) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                      style_positive = "plus", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt, guide = "none") +
      facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
      labs(colour = "", fill = "", y = mar.title.deg.l, x = "",
           title = "") +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))


  plots.post[[region]]$reg.def.comp <-
    ten.sum.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
                        dist_type == "def" & est_type %in% c("1", "2")
                        ][order(-est.label)] |>
    ggplot() +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = stroke.pt.comp,
                          linewidth = linewidth.comp) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1, scale = 100,
                                                      style_positive = "none", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt.comp, guide = "none") +
      labs(colour = "", fill = "", y = abs.title.def, x = "",
           subtitle = reg.title) +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.post[[region]]$reg.def.mar <-
    ten.sum.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
                        dist_type == "def" & est_type %in% c("3")
                        ][order(-est.label)] |>
    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = stroke.pt.comp,
                          linewidth = linewidth.comp) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1, scale = 100,
                                                      style_positive = "plus", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt.comp, guide = "none") +
      labs(colour = "", fill = "", y = mar.title.def.2l, x = "",
           subtitle = reg.title) +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.post[[region]]$reg.deg.comp <-
    ten.sum.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
                        dist_type == "deg" & est_type %in% c("1", "2")
                        ][order(-est.label)] |>
    ggplot() +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = stroke.pt.comp,
                          linewidth = linewidth.comp) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1, scale = 100,
                                                      style_positive = "none", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt.comp, guide = "none") +
      labs(colour = "", fill = "", y = abs.title.deg, x = "",
           subtitle = reg.title) +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

  plots.post[[region]]$reg.deg.mar <-
    ten.sum.l[[region]][!(it_type == "none" & pa_type == "none") & is.na(adm0) &
                        dist_type == "deg" & est_type %in% c("3")
                        ][order(-est.label)] |>
    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
      geom_pointrange(aes(x = cat.label, y = est.median,
                          ymin = est.q5, ymax = est.q95,
                          group = est.label, fill = est.label,
                          colour = est.label, size = est.label),
                          shape = 21, stroke = stroke.pt.comp,
                          linewidth = linewidth.comp) +
      scale_fill_manual(values = col.est.pt, drop = TRUE) +
      scale_colour_manual(values = col.est, drop = TRUE) +
      scale_y_continuous(label = scales::label_number(accuracy = 1, scale = 100,
                                                      style_positive = "plus", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.2))) +
      scale_size_manual(values = size.est.pt.comp, guide = "none") +
      labs(colour = "", fill = "", y = mar.title.deg.2l, x = "",
           subtitle = reg.title) +
      post_theme +
      theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))






    
}



## FIGURES #####################################################################

message("Assembling plots …")

p.area <-
  ((plots.area$amz$undist.nov &
    theme(
          plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      margin = margin(r = 17.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
         plot.margin = unit(c(5, 5, 0, 0), "pt")
         )) +
   (plots.area$cam$undist.nov &
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(5, 5, 0, 5), "pt")
          ))
  ) /
  ((plots.area$amz$undist.ov &
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.text.y = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0.5,
                                      size = rel(0.85)),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 0), "pt"),
          plot.tag = element_blank()
          )) +
    (plots.area$cam$undist.ov &
     theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 5), "pt"),
          plot.tag = element_blank()
          ))
  )

p.def <-
  ((plots.dis$amz$ten.mar$def.nov &
    theme(
          plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      margin = margin(r = 17.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
         plot.margin = unit(c(5, 5, 0, 0), "pt")
         )) +
   (plots.dis$cam$ten.mar$def.nov &
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(5, 5, 0, 5), "pt")
          ))
  ) /
  ((plots.dis$amz$ten.mar$def.ov &
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.text.y = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0.5,
                                      size = rel(0.85)),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 0), "pt"),
          plot.tag = element_blank()
          )) +
    (plots.dis$cam$ten.mar$def.ov &
     theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 5), "pt"),
          plot.tag = element_blank()
          ))
  )

 p.deg <-
   ((plots.dis$amz$ten.mar$deg.nov &
     theme(
           plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      margin = margin(r = 17.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
          plot.margin = unit(c(5, 5, 0, 0), "pt")
          )) +
    (plots.dis$cam$ten.mar$deg.nov &
     theme(axis.text.y = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_blank(),
           plot.margin = unit(c(5, 5, 0, 5), "pt")
           ))
   ) /
   ((plots.dis$amz$ten.mar$deg.ov &
     theme(plot.title = element_blank(),
           plot.subtitle = element_blank(),
           axis.text.y = element_text(hjust = 0),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           plot.margin = unit(c(0, 5, 5, 0), "pt"),
           plot.tag = element_blank()
           )) +
     (plots.dis$cam$ten.mar$deg.ov &
      theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 5), "pt"),
          plot.tag = element_blank()
          ))
   )

p.def.av <-
  ((plots.area.av$amz$ten.mar$def.nov &
    theme(
          plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      margin = margin(r = 17.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
         plot.margin = unit(c(5, 5, 0, 0), "pt")
         )) +
   (plots.area.av$cam$ten.mar$def.nov &
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(5, 5, 0, 5), "pt")
          ))
  ) /
  ((plots.area.av$amz$ten.mar$def.ov &
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.text.y = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0.5,
                                      size = rel(0.85)),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 0), "pt"),
          plot.tag = element_blank()
          )) +
    (plots.area.av$cam$ten.mar$def.ov &
     theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 5), "pt"),
          plot.tag = element_blank()
          ))
  )

 p.deg.av <-
   ((plots.area.av$amz$ten.mar$deg.nov &
     theme(
           plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      margin = margin(r = 17.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
          plot.margin = unit(c(5, 5, 0, 0), "pt")
          )) +
    (plots.area.av$cam$ten.mar$deg.nov &
     theme(axis.text.y = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_blank(),
           plot.margin = unit(c(5, 5, 0, 5), "pt")
           ))
   ) /
   ((plots.area.av$amz$ten.mar$deg.ov &
     theme(plot.title = element_blank(),
           plot.subtitle = element_blank(),
           axis.text.y = element_text(hjust = 0),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           plot.margin = unit(c(0, 5, 5, 0), "pt"),
           plot.tag = element_blank()
           )) +
     (plots.area.av$cam$ten.mar$deg.ov &
      theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 5), "pt"),
          plot.tag = element_blank()
          ))
   )

p.ten <-
  wrap_plots(
             # A = (p.area + plot_layout(guides = "collect", axes = "collect")),
             A = (p.def + plot_layout(guides = "collect", axes = "collect")),
             B = (p.deg + plot_layout(guides = "collect", axes = "collect")),
             nrow = 2) &
  # plot_annotation(tag_levels = list(c("A", "", "", "", "B", "", "", ""))) &
  theme(legend.justification = c(0, 0.9),
        plot.tag = element_text(family = "IBMPlexSans", size = rel(1.2)))
# png(file.fig.ten, width = 7, height = 10.125, unit = "in", res = 600)
png(file.fig.ten, width = 7, height = 6.75, unit = "in", res = 600)
p.ten
dev.off()

p.ten.av <-
  wrap_plots(A = (p.def.av + plot_layout(guides = "collect", axes = "collect")),
             B = (p.deg.av + plot_layout(guides = "collect", axes = "collect")),
             nrow = 2) &
  # plot_annotation(tag_levels = list(c("A", "", "", "", "B", "", "", ""))) &
  theme(legend.justification = c(0, 0.9),
        plot.tag = element_text(family = "IBMPlexSans"))
png(file.fig.ten.av, width = 7, height = 6.75, unit = "in", res = 600)
p.ten.av
dev.off()

png(file.fig.ten.area, width = 7, height = 3.5, unit = "in", res = 600)
p.area +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()

png(file.fig.ten.deg, width = 7, height = 3.5, unit = "in", res = 600)
p.def +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()

png(file.fig.ten.def, width = 7, height = 3.5, unit = "in", res = 600)
p.deg +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()

png(file.fig.ten.deg.av, width = 7, height = 3.5, unit = "in", res = 600)
p.def.av +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()

png(file.fig.ten.def.av, width = 7, height = 3.5, unit = "in", res = 600)
p.deg.av +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()



png(file.fig.ten.post.def.amz, width = 7, height = 9, unit = "in", res = 600)
  plots.post$amz$def.comp +
  (plots.post$amz$def.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
            legend.title = element_blank(),
            legend.margin = margin(0, 0, 0, 0))
dev.off()

png(file.fig.ten.post.deg.amz, width = 7, height = 9, unit = "in", res = 600)
  plots.post$amz$deg.comp +
  (plots.post$amz$deg.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
            legend.title = element_blank(),
            legend.margin = margin(0, 0, 0, 0))
dev.off()

png(file.fig.ten.post.def.cam, width = 7, height = 9, unit = "in", res = 600)
  plots.post$cam$def.comp +
  (plots.post$cam$def.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
            legend.title = element_blank(),
            legend.margin = margin(0, 0, 0, 0))
dev.off()

png(file.fig.ten.post.deg.cam, width = 7, height = 9, unit = "in", res = 600)
  plots.post$cam$deg.comp +
  (plots.post$cam$deg.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
            legend.title = element_blank(),
            legend.margin = margin(0, 0, 0, 0))
dev.off()


png(file.fig.ten.av.post.def.amz, width = 7, height = 9, unit = "in", res = 600)
  plots.av.post$amz$def.comp +
  (plots.av.post$amz$def.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
            legend.title = element_blank(),
            legend.margin = margin(0, 0, 0, 0))
dev.off()

png(file.fig.ten.av.post.deg.amz, width = 7, height = 9, unit = "in", res = 600)
  plots.av.post$amz$deg.comp +
  (plots.av.post$amz$deg.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
            legend.title = element_blank(),
            legend.margin = margin(0, 0, 0, 0))
dev.off()

png(file.fig.ten.av.post.def.cam, width = 7, height = 9, unit = "in", res = 600)
  plots.av.post$cam$def.comp +
  (plots.av.post$cam$def.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
            legend.title = element_blank(),
            legend.margin = margin(0, 0, 0, 0))
dev.off()

png(file.fig.ten.av.post.deg.cam, width = 7, height = 9.25, unit = "in", res = 600)
  plots.av.post$cam$deg.comp +
  (plots.av.post$cam$deg.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0))
dev.off()


p.reg.av.def <-
(plots.av.post$amz$reg.def.comp + labs(title = "Long-term disturbance (deforestation)") &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(5, 5, 0, 0), "pt")/2,
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1),
                                  margin = margin(t = base.size,
                                                  b = base.size)),
        plot.subtitle = element_text(size = rel(0.9),
                                     margin = margin(b = base.size)))) +
(plots.av.post$amz$reg.def.mar &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(5, 5, 0, 10), "pt")/2,
        plot.title.position = "plot",
        plot.subtitle = element_blank())) +
(plots.av.post$cam$reg.def.comp &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(5, 5, 0, 25), "pt")/2,
        plot.title.position = "plot",
        plot.subtitle = element_text(size = rel(0.9),
                                     margin = margin(b = base.size)))) +
(plots.av.post$cam$reg.def.mar &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(5, 5, 0, 10), "pt")/2,
        plot.title.position = "plot",
        plot.subtitle = element_blank())) +
plot_layout(nrow = 1, guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        # legend.key.size = unit(10, "pt"),
        legend.text = element_text(size = rel(0.8)))
p.reg.av.deg <-
(plots.av.post$amz$reg.deg.comp + labs(title = "Short-term disturbance (not followed by deforestation)") &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(15, 5, 0, 0), "pt")/2,
        plot.tag = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1),
                                  margin = margin(t = base.size,
                                                  b = base.size)),
        plot.subtitle = element_text(size = rel(0.9),
                                     margin = margin(b = base.size)))) +
(plots.av.post$amz$reg.deg.mar &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(15, 5, 0, 10), "pt")/2,
        plot.tag = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_blank())) +
(plots.av.post$cam$reg.deg.comp &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(15, 5, 0, 25), "pt")/2,
        plot.tag = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = rel(0.9),
                                     margin = margin(b = base.size)))) +
(plots.av.post$cam$reg.deg.mar &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(15, 5, 0, 10), "pt")/2,
        plot.tag = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_blank())) +
plot_layout(nrow = 1, guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        # legend.key.size = unit(10, "pt"),
        legend.text = element_text(size = rel(0.8)))


png(file.fig.ten.reg.av, width = 7, height = 4, unit = "in", res = 600)
p.reg.av.def/p.reg.av.deg +
  plot_annotation(tag_levels = list("", "", "", "", "", "", "", ""),
                  theme = post_theme) &
  theme(axis.title = element_text(size = rel(0.7)),
        axis.title.y = element_text(margin = margin(r = 0.5*base.size)),
        axis.text.y = element_text(size = rel(0.7)),
        axis.text.x = element_text(size = rel(0.7)),
        legend.text = element_text(size = rel(0.7)),
        legend.key.size = unit(base.size, "pt"))
dev.off()


p.reg.def <-
(plots.post$amz$reg.def.comp + labs(title = "Long-term disturbance (deforestation)") &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(5, 5, 0, 0), "pt")/2,
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1),
                                  margin = margin(t = base.size,
                                                  b = base.size)),
        plot.subtitle = element_text(size = rel(0.9),
                                     margin = margin(b = base.size)))) +
(plots.post$amz$reg.def.mar &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(5, 5, 0, 10), "pt")/2,
        plot.title.position = "plot",
        plot.subtitle = element_blank())) +
(plots.post$cam$reg.def.comp &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(5, 5, 0, 25), "pt")/2,
        plot.title.position = "plot",
        plot.subtitle = element_text(size = rel(0.9),
                                     margin = margin(b = base.size)))) +
(plots.post$cam$reg.def.mar &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(5, 5, 0, 10), "pt")/2,
        plot.title.position = "plot",
        plot.subtitle = element_blank())) +
plot_layout(nrow = 1, guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        # legend.key.size = unit(10, "pt"),
        legend.text = element_text(size = rel(0.8)))
p.reg.deg <-
(plots.post$amz$reg.deg.comp + labs(title = "Short-term disturbance (not followed by deforestation)") &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(15, 5, 0, 0), "pt")/2,
        plot.tag = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1),
                                  margin = margin(t = base.size,
                                                  b = base.size)),
        plot.subtitle = element_text(size = rel(0.9),
                                     margin = margin(b = base.size)))) +
(plots.post$amz$reg.deg.mar &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(15, 5, 0, 10), "pt")/2,
        plot.tag = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_blank())) +
(plots.post$cam$reg.deg.comp &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(15, 5, 0, 25), "pt")/2,
        plot.tag = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = rel(0.9),
                                     margin = margin(b = base.size)))) +
(plots.post$cam$reg.deg.mar &
  theme(
        # axis.title.y = element_text(size = rel(0.9)),
        # axis.text = element_text(size = rel(0.6)),
        plot.margin = unit(c(15, 5, 0, 10), "pt")/2,
        plot.tag = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_blank())) +
plot_layout(nrow = 1, guides = "collect") &
  theme(legend.spacing.y = unit(0, "pt"),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        # legend.key.size = unit(10, "pt"),
        legend.text = element_text(size = rel(0.8)))


png(file.fig.ten.reg, width = 7, height = 4, unit = "in", res = 600)
p.reg.def/p.reg.deg +
  plot_annotation(tag_levels = list("", "", "", "", "", "", "", ""),
                  theme = post_theme) &
  theme(axis.title = element_text(size = rel(0.7)),
        axis.title.y = element_text(margin = margin(r = 0.5*base.size)),
        axis.text.y = element_text(size = rel(0.7)),
        axis.text.x = element_text(size = rel(0.7)),
        legend.text = element_text(size = rel(0.7)),
        legend.key.size = unit(base.size, "pt"))
dev.off()


tab.ten <-
  list(AMZ = ten.sum$amz$mar, CAM = ten.sum$cam$mar) |>
  rbindlist(idcol = "study_region")
tab.ten <- tab.ten[, -c("area.mar.mean.log", "area.mar.median.log", "ci_0", "mar.lab.shade")]
setorder(tab.ten, study_region, dist_type, adm0, it_type, pa_type)
fwrite(tab.ten, file.table.ten)

# ten.sum$amz$mar[dist_type == "def" & is.na(adm0)][order(area.mean), .(reg.label, cat.label, round(area.mean, -2))]

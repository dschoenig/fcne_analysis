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

hurr_type <- "no_hurr"
overwrite <- TRUE


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

file.data.vis <- paste0(path.data.vis, "tenure_comp", hurr_suf, ".rds")
file.fig.ten <- paste0(path.figures, "ten.comp", hurr_suf, ".png")
file.fig.ten.av <- paste0(path.figures, "ten.comp.av", hurr_suf, ".png")
file.table.ten <- paste0(path.data.vis, "tenure_comp", hurr_suf, ".csv")

regions <- c("amz", "cam")


## COLOURS AND LABELS

## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.plot <- col.div[c(3,17)]


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
        plot.subtitle = element_text(margin = margin(t = 6, b = 3))
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

# Labels for tenure categories and administrative regions

cat.lab <- 
  data.table(cat.label = c(
                            "IT, recognized\n—against IT not recgonized",
                            "IT, rec.; PA, cat. I-IV\n—against IT, not rec., PA, cat. I-IV",
                            "IT, rec.; PA, cat. V-VI\n—against IT, not rec., PA, cat. V-VI",
                            "PA, category I-IV\n—against PA, category V-VI",
                            "PA, cat. I-IV; IT, rec.\n—against PA, cat. V-VI; IT, rec.",
                            "PA, cat. I-IV; IT, not rec.\n—against PA, cat. V-VI; IT, not rec."),
             mar_type = c(rep("rec", 3), rep("ind", 3)),
             it_type = c(
                         "recognized", "recognized", "recognized",
                         "none", "recognized", "not_recognized"),
             pa_type = c(
                         "none", "indirect_use", "direct_use",
                         "indirect_use", "indirect_use", "indirect_use"))
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

area.title <- wrap_title("Area covered by least-disturbed TMF 2010 (km²)")
# mar.title <- wrap_title("Absolute avoided disturbances 2011–2020 (proportion of least-disturbed TMF)")
mar.title.def <- wrap_title("Absolute change in long-term disturbance 2011–2020 (proportion of least-disturbed TMF)")
mar.title.deg <- wrap_title("Absolute change in short-term disturbance 2011–2020 (proportion of least-disturbed TMF)")
mar.title.def.av <- wrap_title("Absolute change in long-term disturbance 2011–2020 (km²)")
mar.title.deg.av <- wrap_title("Absolute change in short-term disturbance 2011–2020 (km²)")
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
    name.mar.ten_comp.rec.sam <- paste0(region, ".ten_comp.rec.all.rds")
    name.mar.ten_comp.ind.sam <- paste0(region, ".ten_comp.ind.all.rds")
    name.mar.ten_comp.def.rec <- paste0(region, ".def.ten_comp.rec.all", hurr_suf_mar, ".rds")
    file.mar.ten_comp.def.rec <- paste0(path.marginal, region, "/", name.mar.ten_comp.def.rec)
    name.mar.ten_comp.deg.rec <- paste0(region, ".deg.ten_comp.rec.all", hurr_suf_mar, ".rds")
    file.mar.ten_comp.deg.rec <- paste0(path.marginal, region, "/", name.mar.ten_comp.deg.rec)
    name.mar.ten_comp.def.ind <- paste0(region, ".def.ten_comp.ind.all", hurr_suf_mar, ".rds")
    file.mar.ten_comp.def.ind <- paste0(path.marginal, region, "/", name.mar.ten_comp.def.ind)
    name.mar.ten_comp.deg.ind <- paste0(region, ".deg.ten_comp.ind.all", hurr_suf_mar, ".rds")
    file.mar.ten_comp.deg.ind <- paste0(path.marginal, region, "/", name.mar.ten_comp.deg.ind)

    # Tenure categories by administrative areas

    message("Tenure category by administrative areas …")

    area.undist <-
      readRDS(file.area)$undist

    mar.sam <- readRDS(file.mar.sam)

    mar.ten_comp.def.rec <-
      readRDS(file.mar.ten_comp.def.rec) |>
      merge(mar.sam[[name.mar.ten_comp.rec.sam]])
    mar.ten_comp.def.ind <-
      readRDS(file.mar.ten_comp.def.ind) |>
      merge(mar.sam[[name.mar.ten_comp.ind.sam]])
    mar.ten_comp.deg.rec <-
      readRDS(file.mar.ten_comp.deg.rec) |>
      merge(mar.sam[[name.mar.ten_comp.rec.sam]])
    mar.ten_comp.deg.ind <-
      readRDS(file.mar.ten_comp.deg.ind) |>
      merge(mar.sam[[name.mar.ten_comp.ind.sam]])

    mar.ten_comp.def.rec[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "rec")]
    mar.ten_comp.def.ind[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "ind")]
    mar.ten_comp.deg.rec[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "rec")]
    mar.ten_comp.deg.ind[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "ind")]

    mar.post <-
      rbind(mar.ten_comp.def.rec, mar.ten_comp.def.ind, mar.ten_comp.deg.rec, mar.ten_comp.deg.ind) |>
      merge(area.undist[, -"area.rel"])
    mar.post[, area.prop := marginal * area]

    mar.ten <-
      mar.post[,
               .(n.fac = unique(n.fac),
                 n.cf = unique(n.cf),
                 n.frac = unique(n.fac)/1e7,
                 mar.mean = mean(marginal),
                 mar.median = median(marginal),
                 mar.sd = sd(marginal),
                 mar.mad = mad(marginal),
                 mar.q5 = quantile(marginal, 0.025),
                 mar.q25 = quantile(marginal, 0.25),
                 mar.q75 = quantile(marginal, 0.75),
                 mar.q95 = quantile(marginal, 0.975),
                 mar.prob.pos = sum(marginal > 0)/.N,
                 mar.prob.neg = sum(marginal < 0)/.N,
                 area.mean = mean(area.prop),
                 area.median = median(area.prop),
                 area.sd = sd(area.prop),
                 area.mad = mad(area.prop),
                 area.q5 = quantile(area.prop, 0.05),
                 area.q25 = quantile(area.prop, 0.25),
                 area.q75 = quantile(area.prop, 0.75),
                 area.q95 = quantile(area.prop, 0.95)),
               by = c("mar_type", "dist_type", "it_type", "pa_type", "adm0")]
    # mar.ten[, mar.lab.mean := label_arc(mar.mean, 1, FALSE)]
    mar.ten[, mar.lab.mean := label_perc(mar.mean, 1, FALSE)]
    mar.ten[, area.mean.log := sign(as.numeric(area.mean)) * log10(abs(as.numeric(area.mean)))]

    # Mark where 90% CI includes 0:
    mar.ten[, ci_0 := ifelse(mar.q5 < 0 & mar.q95 > 0, "yes", "no")]
    mar.ten[ci_0 == "yes", mar.lab.mean := paste0("(", mar.lab.mean, ")")]
    mar.ten[, mar.lab.shade := ifelse(abs(mar.mean) < 0.075, "dark", "light")]

    ten.sum[[region]]$mar <-
      expand.grid(cat.label = cat.lab[,cat.label],
                  reg.label = reg.lab[[region]]$reg.label,
                  dist.label = dist.lab$dist.label) |>
      as.data.table() |>
      merge(cat.lab, by = c("cat.label"), all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(dist.lab, by = "dist.label", all = TRUE) |>
      merge(mar.ten[n.fac > 1000],
            by = c("mar_type", "dist_type", "it_type", "pa_type", "adm0"),
            all.x = TRUE)

    ten.sum[[region]]$post <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label,
                  dist.label = dist.lab$dist.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(dist.lab, by = "dist.label", all = TRUE) |> 
      merge(mar.post[n.fac > 1000], by = c("mar_type", "dist_type", "it_type", "pa_type", "adm0"),
            all.x = TRUE)

  }

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(ten.sum = ten.sum) |>
  saveRDS(file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

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

  plots.area.av[[region]]$ten.mar$def.rec <-
    ten.sum[[region]]$mar[mar_type == "rec" & dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = area.mean.log),
                linewidth = 1, colour = "white") +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_binned_divergingx(palette = "Roma",
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
           y = "Tenure security\n", x = NULL) +
      ten_theme

  plots.area.av[[region]]$ten.mar$deg.rec <-
    ten.sum[[region]]$mar[mar_type == "rec" & dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = area.mean.log),
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
           y = "Tenure security\n", x = NULL) +
      ten_theme

  plots.area.av[[region]]$ten.mar$def.ind <-
    ten.sum[[region]]$mar[mar_type == "ind" & dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = area.mean.log),
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
           y = "Protection status\n", x = NULL) +
      ten_theme

  plots.area.av[[region]]$ten.mar$deg.ind <-
    ten.sum[[region]]$mar[mar_type == "ind" & dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = area.mean.log),
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
           y = "Protection status\n", x = NULL) +
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

  plots.dis[[region]]$ten.mar$def.rec <-
    ten.sum[[region]]$mar[mar_type == "rec" & dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
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
           y = "Tenure security\n", x = NULL) +
      ten_theme

  plots.dis[[region]]$ten.mar$deg.rec <-
    ten.sum[[region]]$mar[mar_type == "rec" & dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
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
           y = "Tenure security\n", x = NULL) +
      ten_theme

  plots.dis[[region]]$ten.mar$def.ind <-
    ten.sum[[region]]$mar[mar_type == "ind" & dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
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
           y = "Protection status\n", x = NULL) +
      ten_theme

  plots.dis[[region]]$ten.mar$deg.ind <-
    ten.sum[[region]]$mar[mar_type == "ind" & dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
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
           y = "Protection status\n", x = NULL) +
      ten_theme

}


## FIGURES #####################################################################

message("Assembling plots …")

p.def <-
  ((plots.dis$amz$ten.mar$def.rec &
    theme(
          plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      size = rel(0.85),
                                      margin = margin(r = 3)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
         plot.margin = unit(c(5, 5, 0, 0), "pt")
         )) +
   (plots.dis$cam$ten.mar$def.rec &
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(5, 5, 0, 5), "pt")
          ))
  ) /
  ((plots.dis$amz$ten.mar$def.ind &
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.text.y = element_text(hjust = 0,
                                     size = rel(0.85),
                                     margin = margin(r = 2.5)),
          axis.title.y = element_text(hjust = 0.5,
                                      size = rel(0.85)),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 0), "pt"),
          plot.tag = element_blank()
          )) +
    (plots.dis$cam$ten.mar$def.ind &
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
   ((plots.dis$amz$ten.mar$deg.rec &
     theme(
           plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      size = rel(0.85),
                                      margin = margin(r = 3)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
          plot.margin = unit(c(5, 5, 0, 0), "pt")
          )) +
    (plots.dis$cam$ten.mar$deg.rec &
     theme(axis.text.y = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_blank(),
           plot.margin = unit(c(5, 5, 0, 5), "pt")
           ))
   ) /
   ((plots.dis$amz$ten.mar$deg.ind &
     theme(plot.title = element_blank(),
           plot.subtitle = element_blank(),
           axis.text.y = element_text(hjust = 0,
                                      size = rel(0.85),
                                      margin = margin(r = 2.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           plot.margin = unit(c(0, 5, 5, 0), "pt"),
           plot.tag = element_blank()
           )) +
     (plots.dis$cam$ten.mar$deg.ind &
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
  ((plots.area.av$amz$ten.mar$def.rec &
    theme(
          plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      size = rel(0.85),
                                      margin = margin(r = 3)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
         plot.margin = unit(c(5, 5, 0, 0), "pt")
         )) +
   (plots.area.av$cam$ten.mar$def.rec &
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(5, 5, 0, 5), "pt")
          ))
  ) /
  ((plots.area.av$amz$ten.mar$def.ind &
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.text.y = element_text(hjust = 0,
                                     size = rel(0.85),
                                     margin = margin(r = 2.5)),
          axis.title.y = element_text(hjust = 0.5,
                                      size = rel(0.85)),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 0), "pt"),
          plot.tag = element_blank()
          )) +
    (plots.area.av$cam$ten.mar$def.ind &
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
   ((plots.area.av$amz$ten.mar$deg.rec &
     theme(
           plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      size = rel(0.85),
                                      margin = margin(r = 3)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
          plot.margin = unit(c(5, 5, 0, 0), "pt")
          )) +
    (plots.area.av$cam$ten.mar$deg.rec &
     theme(axis.text.y = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_blank(),
           plot.margin = unit(c(5, 5, 0, 5), "pt")
           ))
   ) /
   ((plots.area.av$amz$ten.mar$deg.ind &
     theme(plot.title = element_blank(),
           plot.subtitle = element_blank(),
           axis.text.y = element_text(hjust = 0,
                                      size = rel(0.85),
                                      margin = margin(r = 2.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           plot.margin = unit(c(0, 5, 5, 0), "pt"),
           plot.tag = element_blank()
           )) +
     (plots.area.av$cam$ten.mar$deg.ind &
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
  wrap_plots(A = (p.def + plot_layout(guides = "collect", axes = "collect")),
             B = (p.deg + plot_layout(guides = "collect", axes = "collect")),
             nrow = 2) +
  plot_annotation(tag_levels = list(c("A", "", "", "", "B", "", "", ""))) &
  theme(legend.justification = c(0, 0.9))
png(file.fig.ten, width = 7, height = 6.75, unit = "in", res = 600)
p.ten
dev.off()

p.ten.av <-
  wrap_plots(A = (p.def.av + plot_layout(guides = "collect", axes = "collect")),
             B = (p.deg.av + plot_layout(guides = "collect", axes = "collect")),
             nrow = 2) +
  plot_annotation(tag_levels = list(c("A", "", "", "", "B", "", "", ""))) &
  theme(legend.justification = c(0, 0.9))
png(file.fig.ten.av, width = 7, height = 6.75, unit = "in", res = 600)
p.ten.av
dev.off()

tab.ten <-
  list(AMZ = ten.sum$amz$mar[, -c("area.mean.log", "ci_0", "mar.lab.shade")],
       CAM = ten.sum$cam$mar[, -c("area.mean.log", "ci_0", "mar.lab.shade")]) |>
  rbindlist(idcol = "study_region")
setorder(tab.ten, study_region, dist_type, adm0, it_type, pa_type)
fwrite(tab.ten, file.table.ten)


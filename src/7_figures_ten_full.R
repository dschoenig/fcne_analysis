args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(ggplot2)
library(ggdist)
library(patchwork)
library(colorspace)
library(stringi)

source("utilities.R")

hurr_type <- tolower(as.character(args[1]))
overwrite <- as.logical(as.character(args[2]))

# hurr_type <- "no_hurr"
# overwrite <- TRUE


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
file.fig.ten.def <- paste0(path.figures, "ten.full.def", hurr_suf, ".png")
file.fig.ten.deg <- paste0(path.figures, "ten.full.deg", hurr_suf, ".png")

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
  data.table(cat.label = c("Non-IT, non-PA",
                            "IT, recognized", "IT, not recognized",
                            "PA, category I-IV", "PA, category V-VI",
                            "IT, rec.; PA, cat. I-IV",
                            "IT, rec.; PA, cat. V-VI",
                            "IT, not rec.; PA, cat. I-IV",
                            "IT, not rec.; PA, cat. V-VI"),
                            # "IT, recognized &\nPA, category I-IV",
                            # "IT, recognized &\nPA, category V-VI",
                            # "IT, not recognized &\nPA, category I-IV",
                            # "IT, not recognized &\nPA, category V-VI"),
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

mar.title <- "Absolute avoided\ndisturbances 2011–2020\n(proportion of previously\nundisturbed TMF,\npercentage points)"
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

    file.mar.sam <- paste0(path.marginal, region, "/", region, ".sam.rds")
    name.mar.ten.sam <- paste0(region, ".ten.itpa.all.rds")
    name.mar.ten.def <- paste0(region, ".def.ten.itpa.all", hurr_suf_mar, ".rds")
    file.mar.ten.def <- paste0(path.marginal, region, "/", name.mar.ten.def)
    name.mar.ten.deg <- paste0(region, ".deg.ten.itpa.all", hurr_suf_mar, ".rds")
    file.mar.ten.deg <- paste0(path.marginal, region, "/", name.mar.ten.deg)

    # Tenure categories by administrative areas

    message("Tenure category by administrative areas …")

    mar.sam <- readRDS(file.mar.sam)

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


    mar.post <- rbind(mar.ten.deg, mar.ten.def)

    mar.ten <-
      mar.post[,
               .(n.fac = unique(n.fac),
                 n.cf = unique(n.cf),
                 n.frac = unique(n.fac)/1e7,
                 mar.mean = mean(marginal),
                 mar.median = median(marginal),
                 mar.sd = sd(marginal),
                 mar.q2.5 = quantile(marginal, 0.025),
                 mar.q5 = quantile(marginal, 0.025),
                 mar.q95 = quantile(marginal, 0.975),
                 mar.q97.5 = quantile(marginal, 0.975),
                 mar.p.tost = max(sum(marginal > 0)/.N, sum(marginal < 0)/.N)),
               by = c("dist_type", "it_type", "pa_type", "adm0")]
    # mar.ten[, mar.lab.mean := label_arc(mar.mean, 1, FALSE)]
    mar.ten[, mar.lab.mean := label_pp(mar.mean, 1, FALSE)]

    # mar.ten[, ci_0 := ifelse(mar.q2.5 < 0 & mar.q97.5 > 0, "yes", "no")]
    mar.ten[, ci_0 := ifelse(mar.q5 < 0 & mar.q95 > 0, "yes", "no")]
    mar.ten[ci_0 == "yes", mar.lab.mean := paste0("(", mar.lab.mean, ")")]

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


    # Mark where CI includes 0:
    ten.sum[[region]]$mar[, ci_0 := ifelse(mar.q2.5 < 0 & mar.q97.5 > 0, "yes", "no")]
    ten.sum[[region]]$mar[, mar.lab.shade := ifelse(abs(mar.mean) < 0.1, "dark", "light")]

  }

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(ten.sum = ten.sum) |>
  saveRDS(file.data.vis)


} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## TENURE EFFECTS BY COUNTRY (NON-OVERLAPPING REGIONS) #########################


plots <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  plots[[region]]$ten.mar$def <-
    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          dist_type == "def"] |>
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
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_pp
                                       ,limits = c(-0.15, 0.15)
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
           fill = mar.title,
           y = "Non-overlapping regimes\n", x = NULL) +
      ten_theme

  plots[[region]]$ten.mar$deg <-
    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          dist_type == "deg"] |>
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
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_pp
                                       ,limits = c(-0.15, 0.15)
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
           fill = mar.title,
           y = "Non-overlapping regimes\n", x = NULL) +
      ten_theme
}



# ## TENURE EFFECTS BY COUNTRY (FULL POSTERIOR DISTRIBUTIONS) ####################


# plots <- list()

# for(i in seq_along(regions)) {

#   region <- regions[i]

#   message(paste0("Preparing plots for region `", region, "` …"))

#   message("Tenure category by country …")

#   sep.x <- nrow(reg.lab[[region]]) - 0.5

#   reg.title <- switch(region,
#                       cam = "Central America",
#                       amz = "Amazon")

#   plots[[region]]$ten.mar$def <-
#     ten.sum[[region]]$post[dist_type == "def" & !is.na(marginal)] |>
#     melt(measure.vars = c("factual", "counterfactual"),
#          variable.name = "condition",
#          value.name = "outcome") |>
#     ggplot() +
#       stat_slabinterval(aes(y = outcome, x = condition), fill = 2) +
#       facet_grid(rows = vars(cat.label), cols = vars(reg.label)) +
#       theme_ggdist()


#     ten.sum[[region]]$post[dist_type == "def" & !is.na(marginal)] |>
#     ggplot() +
#       stat_slabinterval(aes(x = cat.label, y = marginal), fill = 2) +
#       facet_grid(rows = vars(rev(reg.label)), scales = "free_y") +
#       theme_ggdist()

# ten.sum[[region]]$post[dist_type == "def" & !is.na(marginal) & adm0 == "NIC", unique(group.id)]


#     ten.sum[[region]]$post[!(it_type == "none" & pa_type == "none") & dist_type == "def"] |>
#     ggplot() +
#       stat_halfeye(aes(x = cat.label, y = marginal), fill = 2, normalize = "xy", width = 0.5) +
#       geom_hline(yintercept = 0, linetype = "dashed") +
#       facet_grid(rows = vars(rev(reg.label)), scales = "free_y") +
#       # scale_x_discrete(drop = FALSE) +
#       # facet_wrap(vars(reg.label), scales = "free_y", ncol = 1) +
#       facet_grid(rows = vars(reg.label), cols = vars(dist.label), scales = "free_y") +
#       theme_ggdist()

#     ten.sum[[region]]$post[!(it_type == "none" & pa_type == "none") &
#                            dist_type == "def" & !is.na(marginal)] |>
#     melt(measure.vars = c("factual", "counterfactual"),
#          variable.name = "condition",
#          value.name = "outcome") |>
#     ggplot() +
#       stat_halfeye(aes(x = cat.label, y = outcome, colour = condition), normalize = "xy", width = 0.5,
#                    position = "dodge") +
#       geom_hline(yintercept = 0, linetype = "dashed") +
#       facet_grid(rows = vars(rev(reg.label)), scales = "free_y") +
#       # scale_x_discrete(drop = FALSE) +
#       # facet_wrap(vars(reg.label), scales = "free_y", ncol = 1) +
#       facet_grid(rows = vars(reg.label), cols = vars(dist.label), scales = "free_y") +
#       theme_ggdist()


# ten.sum[[region]]$post[group.id == 101]

#   |>
#     ggplot() +
#       geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
#                 linewidth = 1, colour = "white") +
#       geom_text(aes(x = reg.label, y = cat.label,
#                     label = mar.lab.mean,
#                     fontface = ci_0,
#                     colour = mar.lab.shade),
#                 size = size.mar.lab) +
#       geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
#                    linewidth = 0.3, colour = "grey5") +
#       scale_fill_continuous_divergingx(
#                                        palette = "Roma"
#                                        ,rev = TRUE,
#                                        ,breaks = round(seq(-0.15, 0.15, 0.05),2)
#                                        ,labels = label_pp
#                                        ,limits = c(-0.15, 0.15)
#                                        ,oob = scales::squish
#                                        ,na.value = "grey95"
#                                        ) +
#       scale_discrete_manual("fontface", values = c(yes = "italic", no = "plain"),
#                             guide = "none") +
#       scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
#                           guide = "none") +
#       scale_x_discrete(position = "top") +
#       scale_y_discrete(limits = rev) +
#       coord_fixed() +
#       ten_guide_fill +
      # labs(title = dist.lab[dist_type == "def", dist.label],
#            subtitle = reg.title,
#            fill = mar.title,
#            y = "Non-overlapping regimes\n", x = NULL) +
#       ten_theme

#   plots[[region]]$ten.mar$deg <-
#     ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
#                            (it_type != "none"  & pa_type == "none")) &
#                           dist_type == "deg"] |>
#     ggplot() +
#       geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
#                 linewidth = 1, colour = "white") +
#       geom_text(aes(x = reg.label, y = cat.label,
#                     label = mar.lab.mean,
#                     fontface = ci_0,
#                     colour = mar.lab.shade),
#                 size = size.mar.lab) +
#       geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
#                    linewidth = 0.3, colour = "grey5") +
#       scale_fill_continuous_divergingx(
#                                        palette = "Roma"
#                                        ,rev = TRUE,
#                                        ,breaks = round(seq(-0.15, 0.15, 0.05),2)
#                                        ,labels = label_pp
#                                        ,limits = c(-0.15, 0.15)
#                                        ,oob = scales::squish
#                                        ,na.value = "grey95"
#                                        ) +
#       scale_discrete_manual("fontface", values = c(yes = "italic", no = "plain"),
#                             guide = "none") +
#       scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
#                           guide = "none") +
#       scale_x_discrete(position = "top") +
#       scale_y_discrete(limits = rev) +
#       coord_fixed() +
#       ten_guide_fill +
#       labs(title = dist.lab[dist_type == "def", dist.label],
#            subtitle = reg.title,
#            fill = mar.title,
#            y = "Non-overlapping regimes\n", x = NULL) +
#       ten_theme
# }






## TENURE EFFECTS BY COUNTRY (OVERLAPPING AREAS) ###############################

plots.ov <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  plots.ov[[region]]$ten.mar$def <-
    ten.sum[[region]]$mar[it_type != "none"  & pa_type != "none" &
                          dist_type == "def"] |>
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
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_pp
                                       ,limits = c(-0.15, 0.15)
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
           fill = mar.title,
           y = "Overlapping regimes\n", x = NULL) +
      ten_theme

  plots.ov[[region]]$ten.mar$deg <-
    ten.sum[[region]]$mar[it_type != "none"  & pa_type != "none" &
                          dist_type == "deg"] |>
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
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_pp
                                       ,limits = c(-0.15, 0.15)
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
           fill = mar.title,
           y = "Overlapping regimes\n", x = NULL) +
      ten_theme

}


## FIGURES #####################################################################

message("Assembling plots …")

p.def <-
  ((plots$amz$ten.mar$def &
    theme(
          plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      margin = margin(r = 17.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
         plot.margin = unit(c(5, 5, 0, 0), "pt")
         )) +
   (plots$cam$ten.mar$def &
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(5, 5, 0, 5), "pt")
          ))
  ) /
  ((plots.ov$amz$ten.mar$def &
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
    (plots.ov$cam$ten.mar$def &
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
   ((plots$amz$ten.mar$deg &
     theme(
           plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0,
                                      margin = margin(r = 17.5)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
          plot.margin = unit(c(5, 5, 0, 0), "pt")
          )) +
    (plots$cam$ten.mar$deg &
     theme(axis.text.y = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_blank(),
           plot.margin = unit(c(5, 5, 0, 5), "pt")
           ))
   ) /
   ((plots.ov$amz$ten.mar$deg &
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
     (plots.ov$cam$ten.mar$deg &
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



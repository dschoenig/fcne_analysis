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

overwrite <- TRUE

if(is.na(overwrite)) {
  overwrite <- FALSE
}

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
path.marginal <- paste0(path.base, "models/marginal/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.tables <- paste0(path.base, "results/tables/")
if(!dir.exists(path.tables)) dir.create(path.tables, recursive = TRUE)

if(is.na(hurr_type)) {
  hurr_type <- "otto"
}
if(hurr_type == "no_hurr") {
  hurr_suf <- ".no_hurr"
} else {
  hurr_suf <- ""
}

file.data.vis <- paste0(path.data.vis, "tenure_adm", hurr_suf, ".rds")
file.fig.ten <- paste0(path.figures, "ten.mar", hurr_suf, ".png")
file.fig.ten.def <- paste0(path.figures, "ten.mar.def", hurr_suf, ".png")
file.fig.ten.deg <- paste0(path.figures, "ten.mar.deg", hurr_suf, ".png")

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
        axis.title.y = element_text(size = rel(0.9)),
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
                            "IT, recognized;\noverlapping with PA, cat. I-IV",
                            "IT, recognized;\noverlapping with PA, cat. V-VI",
                            "IT, not recognized;\noverlapping with PA, cat. I-IV",
                            "IT, not recognized;\noverlapping with PA, cat. V-VI",
                            "PA, category I-IV;\noverlapping with IT, rec.",
                            "PA, category I-IV;\noverlapping with IT, not rec.",
                            "PA, category V-VI;\noverlapping with IT, rec.",
                            "PA, category V-VI;\noverlapping with IT, not rec."),
             mar_type = c(rep("it", 4), rep("pa", 4)),
             it_type = c(
                         "recognized", "recognized",
                         "not_recognized", "not_recognized",
                         "recognized", "not_recognized",
                         "recognized", "not_recognized"),
             pa_type = c(
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "indirect_use", "indirect_use",
                         "direct_use", "direct_use"))
# cat.lab[, cat.label := stri_pad_left(cat.label, width = max(stri_width(cat.label)))]
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]

cat.lab <- 
  data.table(cat.label = c(
                            "IT, rec.; PA, cat. I-IV\n—against only PA, cat. I-IV",
                            "IT, rec.; PA, cat. V-VI\n—against only PA, cat. V-VI",
                            "IT, not rec.; PA, cat. I-IV\n—against only PA, cat. I-IV",
                            "IT, not rec.; PA, cat. V-VI\n—against only PA, cat. V-VI",
                            "PA, cat. I-IV; IT, rec.\n—against only IT, rec.",
                            "PA, cat. I-IV; IT, not rec.\n—against only IT not rec.",
                            "PA, cat. V-VI; IT, rec.\n—against only IT, rec.",
                            "PA, cat. V-VI; IT, not rec.\n—against only IT not rec."),
             mar_type = c(rep("it", 4), rep("pa", 4)),
             it_type = c(
                         "recognized", "recognized",
                         "not_recognized", "not_recognized",
                         "recognized", "not_recognized",
                         "recognized", "not_recognized"),
             pa_type = c(
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "indirect_use", "indirect_use",
                         "direct_use", "direct_use"))
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
             dist.label = c("Long-term disturbance", "Short-term disturbance"))
dist.lab[, dist.label := factor(dist.label, levels = dist.label)]

# mar.title <- "Absolute marginal\ndifference in\nforest loss risk\n(2011 – 2020)"
mar.title <- "Avoided disturbances 2011–2020\n(proportion of undisturbed TMF)"
it.title <- "Additional effect of indigenous tenure\nin mixed tenure regimes\n"
pa.title <- "Additional effect of areal protection\nin mixed tenure regimes\n"
it.title <- "Additional effect of\nindigenous tenure\nin mixed tenure regimes\n"
pa.title <- "Additional effect of\nareal protection\nin mixed tenure regimes\n"




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
    name.mar.ten.it.sam <- paste0(region, ".ten.it.ov.rds")
    name.mar.ten.pa.sam <- paste0(region, ".ten.pa.ov.rds")
    name.mar.ten.def.it <- paste0(region, ".def.ten.it.ov", hurr_suf_mar, ".rds")
    file.mar.ten.def.it <- paste0(path.marginal, region, "/", name.mar.ten.def.it)
    name.mar.ten.deg.it <- paste0(region, ".deg.ten.it.ov", hurr_suf_mar, ".rds")
    file.mar.ten.deg.it <- paste0(path.marginal, region, "/", name.mar.ten.deg.it)
    name.mar.ten.def.pa <- paste0(region, ".def.ten.pa.ov", hurr_suf_mar, ".rds")
    file.mar.ten.def.pa <- paste0(path.marginal, region, "/", name.mar.ten.def.pa)
    name.mar.ten.deg.pa <- paste0(region, ".deg.ten.pa.ov", hurr_suf_mar, ".rds")
    file.mar.ten.deg.pa <- paste0(path.marginal, region, "/", name.mar.ten.deg.pa)

    # Tenure categories by administrative areas

    message("Tenure category by administrative areas …")

    mar.sam <- readRDS(file.mar.sam)

    mar.ten.def.it <-
      readRDS(file.mar.ten.def.it) |>
      merge(mar.sam[[name.mar.ten.it.sam]])
    mar.ten.deg.it <-
      readRDS(file.mar.ten.deg.it) |>
      merge(mar.sam[[name.mar.ten.it.sam]])
    mar.ten.def.pa <-
      readRDS(file.mar.ten.def.pa) |>
      merge(mar.sam[[name.mar.ten.pa.sam]])
    mar.ten.deg.pa <-
      readRDS(file.mar.ten.deg.pa) |>
      merge(mar.sam[[name.mar.ten.pa.sam]])

    mar.ten.def.it[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "it")]
    mar.ten.def.pa[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "pa")]
    mar.ten.deg.it[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "it")]
    mar.ten.deg.pa[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "pa")]


    # Effect summaries. Combinations of tenure category and country with < 1000
    # observations are excluded.

    mar.ten <-
      rbind(mar.ten.deg.it, mar.ten.deg.pa, mar.ten.def.it, mar.ten.def.pa) |>
      _[,
        .(n.fac = unique(n.fac),
          n.cf = unique(n.cf),
          n.frac = unique(n.fac)/1e7,
          mar.mean = mean(marginal),
          mar.median = median(marginal),
          mar.sd = sd(marginal),
          mar.q2.5 = quantile(marginal, 0.025),
          mar.q97.5 = quantile(marginal, 0.975),
          mar.p.tost = max(sum(marginal > 0)/.N, sum(marginal < 0)/.N)),
        by = c("mar_type", "dist_type", "it_type", "pa_type", "adm0")]
    mar.ten[, mar.lab.mean := label_arc(mar.mean, 1, FALSE)]
    mar.ten[, ci_0 := ifelse(mar.q2.5 < 0 & mar.q97.5 > 0, "yes", "no")]
    mar.ten[ci_0 == "yes", mar.lab.mean := paste0("(", mar.lab.mean, ")")]

    ten.sum[[region]]$mar <-
      # expand.grid(cat.label = cat.lab[it_type != "none" & pa_type != "none", cat.label],
      expand.grid(cat.label = cat.lab[,cat.label],
                  reg.label = reg.lab[[region]]$reg.label,
                  dist.label = dist.lab$dist.label) |>
      as.data.table() |>
      merge(cat.lab, by = c("cat.label"), all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(dist.lab, by = "dist.label", all = TRUE) |> 
      merge(mar.ten[n.fac > 1000], by = c("mar_type", "dist_type", "it_type", "pa_type", "adm0"),
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
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_arc
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
      labs(title = "Primary forests",
           subtitle = reg.title,
           fill = mar.title,
           y = "Non-overlapping areas\n", x = NULL) +
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
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_arc
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
      labs(title = "All forests",
           subtitle = reg.title,
           fill = mar.title,
           y = "Non-overlapping areas\n", x = NULL) +
      ten_theme
}



## MARGINAL EFFECTS ############################################################

plots.it <- list()
plots.pa <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  plots.it[[region]]$ten.mar$def <-
    ten.sum[[region]]$mar[mar_type == "it" & dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_arc
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
      labs(title = "Primary forests",
           subtitle = reg.title,
           fill = mar.title,
           y = it.title, x = NULL) +
      ten_theme 

  plots.it[[region]]$ten.mar$deg <-
    ten.sum[[region]]$mar[mar_type == "it" & dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_arc
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
      labs(title = "All forests",
           subtitle = reg.title,
           fill = mar.title,
           y = it.title, x = NULL) +
      ten_theme 

 plots.pa[[region]]$ten.mar$def <-
    ten.sum[[region]]$mar[mar_type == "pa" & dist_type == "def"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_arc
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
      labs(title = "Primary forests",
           subtitle = reg.title,
           fill = mar.title,
           y = pa.title, x = NULL) +
      ten_theme 

  plots.pa[[region]]$ten.mar$deg <-
    ten.sum[[region]]$mar[mar_type == "pa" & dist_type == "deg"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                linewidth = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
                   linewidth = 0.3, colour = "grey5") +
      scale_fill_continuous_divergingx(
                                       palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_arc
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
      labs(title = "All forests",
           subtitle = reg.title,
           fill = mar.title,
           y = pa.title, x = NULL) +
      ten_theme 

}


## FIGURES #####################################################################

message("Assembling plots …")

p.pf <-
  ((plots.it$amz$ten.mar$def &
    theme(
          plot.title.position = "panel",
          axis.text.y = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0.5,
                                      size = rel(0.85)),
         plot.margin = unit(c(5, 5, 5, 0), "pt")
         )) +
   (plots.it$cam$ten.mar$def &
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(5, 5, 5, 5), "pt")
          ))
  ) /
  ((plots.pa$amz$ten.mar$def &
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.text.y = element_text(hjust = 0,
                                     margin = margin(r = 7.25)),
          axis.title.y = element_text(hjust = 0.5,
                                      size = rel(0.85)),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0, 5, 5, 0), "pt"),
          plot.tag = element_blank()
          )) +
    (plots.pa$cam$ten.mar$def &
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
 p.af <-
   ((plots.it$amz$ten.mar$deg &
     theme(
           plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0),
           axis.title.y = element_text(hjust = 0.5,
                                       # margin = margin(r = 18.1),
                                       size = rel(0.85)),
          plot.margin = unit(c(5, 5, 5, 0), "pt")
          )) +
    (plots.it$cam$ten.mar$deg &
     theme(axis.text.y = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_blank(),
           plot.margin = unit(c(5, 5, 5, 5), "pt")
           ))
   ) /
   ((plots.pa$amz$ten.mar$deg &
     theme(plot.title = element_blank(),
           plot.subtitle = element_blank(),
           axis.text.y = element_text(hjust = 0,
                                      margin = margin(r = 7.25)),
           axis.title.y = element_text(hjust = 0.5,
                                       size = rel(0.85)),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           plot.margin = unit(c(0, 5, 5, 0), "pt"),
           plot.tag = element_blank()
           )) +
     (plots.pa$cam$ten.mar$deg &
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
  wrap_plots(A = (p.pf + plot_layout(guides = "collect", axes = "collect")),
             B = (p.af + plot_layout(guides = "collect", axes = "collect")),
             nrow = 2) +
  plot_annotation(tag_levels = list(c("A", "", "", "", "B", "", "", ""))) &
  theme(legend.justification = c(0,0.9))
png(file.fig.ten, width = 7, height = 6.75, unit = "in", res = 600)
p.ten
dev.off()

png(file.fig.ten.deg, width = 7, height = 3.5, unit = "in", res = 600)
p.pf +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()


png(file.fig.ten.def, width = 7, height = 3.5, unit = "in", res = 600)
p.af +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()




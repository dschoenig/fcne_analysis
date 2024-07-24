args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(ggplot2)
library(ggdist)
library(patchwork)
library(colorspace)
library(stringi)

source("utilities.R")

hurr_type <- tolower(as.character(args[1]))

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
if(hurr_type == "no_otto") {
  hurr_suf <- ".no_otto"
} else {
  hurr_suf <- ""
}

file.data.vis <- paste0(path.data.vis, "tenure_adm_mar", hurr_suf, ".rds")
file.fig.ten <- paste0(path.figures, "ten.mar", hurr_suf, ".png")
file.fig.ten.pf <- paste0(path.figures, "ten.mar.pf", hurr_suf, ".png")
file.fig.ten.af <- paste0(path.figures, "ten.mar.af", hurr_suf, ".png")

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

for.lab <-
  data.table(for_type = c("primary", NA),
             for.label = c("Primary forests", "All forests"))
for.lab[, for.label := factor(for.label, levels = for.label)]

mar.title <- "Absolute marginal\ndifference in\nforest loss risk\n(2011 – 2020)"
it.title <- "Additional effect of indigenous tenure\nin mixed tenure regimes\n"
pa.title <- "Additional effect of areal protection\nin mixed tenure regimes\n"
it.title <- "Additional effect of\nindigenous tenure\nin mixed tenure regimes\n"
pa.title <- "Additional effect of\nareal protection\nin mixed tenure regimes\n"




## EFFECT OF TENURE BY ADMINISTRATIVE AREA, FOREST TYPE ########################

if(!file.exists(file.data.vis)) {

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
    name.mar.ten.af.it <- paste0(region, ".ten.af.it.ov", hurr_suf_mar, ".rds")
    file.mar.ten.af.it <- paste0(path.marginal, region, "/", name.mar.ten.af.it)
    name.mar.ten.pf.it <- paste0(region, ".ten.pf.it.ov", hurr_suf_mar, ".rds")
    file.mar.ten.pf.it <- paste0(path.marginal, region, "/", name.mar.ten.pf.it)
    name.mar.ten.af.pa <- paste0(region, ".ten.af.pa.ov", hurr_suf_mar, ".rds")
    file.mar.ten.af.pa <- paste0(path.marginal, region, "/", name.mar.ten.af.pa)
    name.mar.ten.pf.pa <- paste0(region, ".ten.pf.pa.ov", hurr_suf_mar, ".rds")
    file.mar.ten.pf.pa <- paste0(path.marginal, region, "/", name.mar.ten.pf.pa)

    # Tenure categories by administrative areas

    message("Tenure category by administrative areas …")

    mar.sam <- readRDS(file.mar.sam)

    mar.ten.af.it <-
      readRDS(file.mar.ten.af.it) |>
      merge(mar.sam[[name.mar.ten.af.it]])
    mar.ten.pf.it <-
      readRDS(file.mar.ten.pf.it) |>
      merge(mar.sam[[name.mar.ten.pf.it]])
    mar.ten.af.pa <-
      readRDS(file.mar.ten.af.pa) |>
      merge(mar.sam[[name.mar.ten.af.pa]])
    mar.ten.pf.pa <-
      readRDS(file.mar.ten.pf.pa) |>
      merge(mar.sam[[name.mar.ten.pf.pa]])

    mar.ten.af.it[,
                  `:=`(for_type = factor(NA, levels = c("primary", "other")),
                       mar_type = "it")]
    mar.ten.af.pa[,
                  `:=`(for_type = factor(NA, levels = c("primary", "other")),
                       mar_type = "pa")]
    mar.ten.pf.it[,
                  `:=`(for_type = factor("primary", levels = c("primary", "other")),
                       mar_type = "it")]
    mar.ten.pf.pa[,
                  `:=`(for_type = factor("primary", levels = c("primary", "other")),
                       mar_type = "pa")]


    # Effect summaries. Combinations of tenure category and country with < 1000
    # observations are excluded.

    mar.ten <-
      rbind(mar.ten.pf.it, mar.ten.pf.pa, mar.ten.af.it, mar.ten.af.pa) |>
      _[,
        .(n = unique(n),
          n.frac = unique(n)/1e7,
          mar.mean = mean(marginal),
          mar.median = median(marginal),
          mar.sd = sd(marginal),
          mar.q2.5 = quantile(marginal, 0.025),
          mar.q97.5 = quantile(marginal, 0.975),
          mar.p.tost = max(sum(marginal > 0)/.N, sum(marginal < 0)/.N)),
        by = c("mar_type", "for_type", "it_type", "pa_type", "adm0")]
    mar.ten[, mar.lab.mean := label_arc(mar.mean, 1, FALSE)]
    mar.ten[, ci_0 := ifelse(mar.q2.5 < 0 & mar.q97.5 > 0, "yes", "no")]
    mar.ten[ci_0 == "yes", mar.lab.mean := paste0("(", mar.lab.mean, ")")]

    ten.sum[[region]]$mar <-
      # expand.grid(cat.label = cat.lab[it_type != "none" & pa_type != "none", cat.label],
      expand.grid(cat.label = cat.lab[,cat.label],
                  reg.label = reg.lab[[region]]$reg.label,
                  for.label = for.lab$for.label) |>
      as.data.table() |>
      merge(cat.lab, by = c("cat.label"), all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(for.lab, by = "for.label", all = TRUE) |> 
      merge(mar.ten[n > 1000], by = c("mar_type", "for_type", "it_type", "pa_type", "adm0"),
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

  plots[[region]]$ten.mar$primary <-

    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          for_type == "primary"] |>
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

  plots[[region]]$ten.mar$all <-
    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          is.na(for_type)] |>
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

  plots.it[[region]]$ten.mar$primary <-
    ten.sum[[region]]$mar[mar_type == "it" & for_type == "primary"] |>
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

  plots.it[[region]]$ten.mar$all <-
    ten.sum[[region]]$mar[mar_type == "it" & is.na(for_type)] |>
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

 plots.pa[[region]]$ten.mar$primary <-
    ten.sum[[region]]$mar[mar_type == "pa" & for_type == "primary"] |>
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

  plots.pa[[region]]$ten.mar$all <-
    ten.sum[[region]]$mar[mar_type == "pa" & is.na(for_type)] |>
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
  ((plots.it$amz$ten.mar$primary &
    theme(
          plot.title.position = "panel",
          axis.text.y = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0.5,
                                      size = rel(0.85)),
         plot.margin = unit(c(5, 5, 5, 0), "pt")
         )) +
   (plots.it$cam$ten.mar$primary &
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(5, 5, 5, 5), "pt")
          ))
  ) /
  ((plots.pa$amz$ten.mar$primary &
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
    (plots.pa$cam$ten.mar$primary &
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
   ((plots.it$amz$ten.mar$all &
     theme(
           plot.title.position = "panel",
           axis.text.y = element_text(hjust = 0),
           axis.title.y = element_text(hjust = 0.5,
                                       # margin = margin(r = 18.1),
                                       size = rel(0.85)),
          plot.margin = unit(c(5, 5, 5, 0), "pt")
          )) +
    (plots.it$cam$ten.mar$all &
     theme(axis.text.y = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_blank(),
           plot.margin = unit(c(5, 5, 5, 5), "pt")
           ))
   ) /
   ((plots.pa$amz$ten.mar$all &
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
     (plots.pa$cam$ten.mar$all &
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

png(file.fig.ten.pf, width = 7, height = 3.5, unit = "in", res = 600)
p.pf +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()


png(file.fig.ten.af, width = 7, height = 3.5, unit = "in", res = 600)
p.af +
plot_layout(guides = "collect") &
theme(legend.justification = c(0, 0.9))
dev.off()




library(data.table)
library(posterior)
library(sf)
library(ggplot2)
library(ggdist)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)

source("utilities.R")

# path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
path.effects <- paste0(path.base, "models/gam/effects/")
# path.effects <- paste0(path.base, "models/gam/effects/prim/")
path.figures <- paste0(path.base, "figures/")

file.data.vis <- paste0(path.data.vis, "tenure_adm.rds")

regions <- c("amz", "cam")
# regions <- c("cam")


## COLOURS AND LABELS

## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.plot <- col.div[c(3,17)]


# Labels for tenure categories and administrative regions

cat.lab <- 
  data.table(cat.label = c("Reference\n(non-IT, non-PA)",
                            "IT, recognized", "IT, not recognized",
                            "PA, indirect use", "PA, direct use",
                            "IT, recognized &\nPA, indirect use",
                            "IT, recognized &\nPA, direct use",
                            "IT, not recognized &\nPA, indirect use",
                            "IT, not recognized &\nPA, direct use"),
             it_type = c("none",
                         "recognized", "not_recognized",
                         NA, NA,
                         "recognized", "recognized",
                         "not_recognized", "not_recognized"),
             pa_type = c("none",
                         NA, NA,
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use"))
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



## EFFECT OF TENURE BY ADMINISTRATIVE AREA, FOREST TYPE ########################

ten.sum <- list()

for(i in seq_along(regions)) {
  
  region <- regions[i]

  message(paste0("Preparing data for region `", region, "` …"))
  
  file.riskchange.tenure <- paste0(path.effects, region, ".riskchange.tenure.rds")
  file.riskchange.tenure_areas <- paste0(path.effects, region, ".riskchange.tenure_areas.rds")


  # Tenure categories by administrative areas

  message("Tenure category by administrative areas …")

  rc.ten <- readRDS(file.riskchange.tenure)

  arc.ten.sum <-
    summarize_draws(rc.ten$arc,
                                  mean, 
                                  sd,
                                  \(x) quantile2(x, c(0.025, 0.975)))|>
                 as.data.table() |>
                 setnames("variable", "group.label") |>
                 merge(rc.ten$groups[, !"ids"],
                       by = "group.label")

  rm(rc.ten)

  ten.sum[[region]]$arc <-
    expand.grid(cat.label = cat.lab$cat.label,
                reg.label = reg.lab[[region]]$reg.label,
                for.label = for.lab$for.label) |>
    as.data.table() |>
    merge(cat.lab, by = "cat.label", all = TRUE) |>
    merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
    merge(for.lab, by = "for.label", all = TRUE) |> 
    merge(arc.ten.sum[n >= 1000], by = c("for_type", "it_type", "pa_type", "adm0"), all.x = TRUE)
}


plots <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  # Tenure categories by administrative areas

  message("Tenure category by administrative areas …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  plots[[region]]$ten.arc$primary <-
    # ggplot(ten.sum[[region]]$arc[it_type != "none" | pa_type != "none"]) +
    ggplot(ten.sum[[region]]$arc[(is.na(it_type) | is.na(pa_type)) &
                                 for_type == "primary"]) +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = label_arc(mean, 1, FALSE)), colour = "grey5",
                size = 2.5) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 8.5,
                   size = 0.3, colour = "grey5") +
      # geom_segment(x = 0.5, y = 8.5, xend = 9.5, yend = 8.5, size = 0.2, colour = "grey35") +
      # geom_segment(x = 0.5, y = 6.5, xend = 9.5, yend = 6.5, size = 1, colour = "grey50") +
      # geom_segment(x = 0.5, y = 4.5, xend = 9.5, yend = 4.5, size = 1, colour = "grey50") +
      scale_fill_continuous_divergingx(
                                      # palette = "Blue-Red 3",
                                      palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_arc
                                       # ,labels = c(paste0(seq(-15, 0, 5), "%"),
                                       #             paste0("+", seq(5, 15, 5), "%"))
                                       #             # "+5%", "≥ +10%"),
                                       # ,limits = c(-0.075, 0.075)
                                       # ,limits = c(-0.15, 0.15)
                                       ,limits = c(-0.15, 0.15)
                                       ,oob = scales::squish
                                       ,na.value = "grey95"
                                       ) +
      scale_colour_manual(values = c("0" = "grey5", "1" = "grey95")) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      guides(fill = guide_colorbar(ticks.colour = "grey35",
                                   ticks.linewidth = 1,
                                   frame.colour = "grey35",
                                   frame.linewidth = 1,
                                   barheight = 5,
                                   barwidth = 1,
                                   label.position = "left",
                                   label.hjust = 1,
                                   draw.ulim = FALSE,
                                   draw.llim = FALSE),
             colour = "none") +
      labs(fill = "Absolute change in\nforest loss risk",
           y = "Primary forests\n", x = reg.title) +
      theme_minimal(base_family = "IBMPlexSans") +
      theme(
            legend.position = "right",
            legend.justification = c(0,1),
            legend.spacing.y = unit(5, "mm"),
            legend.title = element_text(size = rel(0.75)),
            legend.text = element_text(size = rel(0.75)),
            axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0,
                                       colour = "grey5"),
            axis.text.y = element_text(hjust = 0, colour = "grey5"),
            # panel.background = element_rect(fill = "grey85", colour = "grey85", size = 2),
            panel.grid.major = element_blank()
            # axis.line = element_line(colour = "grey85")
      )

  plots[[region]]$ten.arc$other <-
    # ggplot(ten.sum[[region]]$arc[it_type != "none" | pa_type != "none"]) +
    ggplot(ten.sum[[region]]$arc[(is.na(it_type) | is.na(pa_type)) &
                                 is.na(for_type)]) +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = label_arc(mean, 1, FALSE)), colour = "grey5",
                size = 2.5) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 8.5,
                   size = 0.3, colour = "grey5") +
      # geom_segment(x = 0.5, y = 8.5, xend = 9.5, yend = 8.5, size = 0.2, colour = "grey35") +
      # geom_segment(x = 0.5, y = 6.5, xend = 9.5, yend = 6.5, size = 1, colour = "grey50") +
      # geom_segment(x = 0.5, y = 4.5, xend = 9.5, yend = 4.5, size = 1, colour = "grey50") +
      scale_fill_continuous_divergingx(
                                      # palette = "Blue-Red 3",
                                      palette = "Roma"
                                       ,rev = TRUE,
                                       ,breaks = round(seq(-0.15, 0.15, 0.05),2)
                                       ,labels = label_arc
                                       # ,labels = c(paste0(seq(-15, 0, 5), "%"),
                                       #             paste0("+", seq(5, 15, 5), "%"))
                                       #             # "+5%", "≥ +10%"),
                                       # ,limits = c(-0.075, 0.075)
                                       # ,limits = c(-0.15, 0.15)
                                       ,limits = c(-0.15, 0.15)
                                       ,oob = scales::squish
                                       ,na.value = "grey95"
                                       ) +
      scale_colour_manual(values = c("0" = "grey5", "1" = "grey95")) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      guides(fill = guide_colorbar(ticks.colour = "grey35",
                                   ticks.linewidth = 1,
                                   frame.colour = "grey35",
                                   frame.linewidth = 1,
                                   barheight = 5,
                                   barwidth = 1,
                                   label.position = "left",
                                   label.hjust = 1,
                                   draw.ulim = FALSE,
                                   draw.llim = FALSE),
             colour = "none") +
      labs(fill = "Absolute change in\nforest loss risk",
           y = "All forests\n", x = NULL) +
      theme_minimal(base_family = "IBMPlexSans") +
      theme(
            legend.position = "right",
            legend.justification = c(0,1),
            legend.spacing.y = unit(5, "mm"),
            legend.title = element_text(size = rel(0.75)),
            legend.text = element_text(size = rel(0.75)),
            # axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0,
            #                            colour = "grey5"),
            axis.text.x = element_blank(),
            axis.text.y = element_text(hjust = 0, colour = "grey5"),
            # panel.background = element_rect(fill = "grey85", colour = "grey85", size = 2),
            panel.grid.major = element_blank()
            # axis.line = element_line(colour = "grey85")
      )

}


combined.ten.arc <-
  with(plots,
       ((amz$ten.arc$primary + theme(axis.title.y = element_text(hjust = 0.5),
                                     axis.title.x = element_text(hjust = 0))) +
        (cam$ten.arc$primary + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.title.x = element_text(hjust = 0))) +
        # plot_layout(guides = "collect") +
        plot_annotation(tag_levels = NULL) &
        theme(legend.position = "right",
              legend.justification = c(0,1),
              legend.spacing.y = unit(5, "mm"),
              legend.title = element_text(size = rel(0.75)),
              legend.text = element_text(size = rel(0.75)))
       ) /
       ((amz$ten.arc$other + theme(axis.title.y = element_text(hjust = 0.5))) +
        (cam$ten.arc$other + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank()))
        # plot_layout(guides = "collect") +
       )) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = NULL) &
  theme(legend.position = "right",
        legend.justification = c(0,1),
        legend.spacing.y = unit(5, "mm"),
        legend.title = element_text(size = rel(0.9)),
        legend.text = element_text(size = rel(0.75)))

combined.ten.arc

# tiff(paste0(path.figures, "fig2.tif"), width = 8.25, height = 4, unit = "in", res = 300)
svg(paste0(path.figures, "ten.arc.svg"), width = 8.25, height = 3.5)
combined.ten.arc
dev.off()

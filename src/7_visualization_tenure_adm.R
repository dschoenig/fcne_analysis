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
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.tables <- paste0(path.base, "results/tables/")
if(!dir.exists(path.tables)) dir.create(path.tables, recursive = TRUE)

file.data.vis <- paste0(path.data.vis, "tenure_adm.rds")

regions <- c("amz", "cam")
# regions <- c("cam")


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
        legend.title = element_text(size = rel(0.9), hjust = 0,
                                    margin = margin(t = 3, b = 6)),
        legend.text = element_text(size = rel(0.8)),
        legend.spacing.y = unit(2, "pt"),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0,
                                   colour = "grey5"),
        axis.text.y = element_text(hjust = 0, colour = "grey5"),
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
  guides(fill = guide_colorbar(
                               ticks.colour = "grey5",
                               ticks.linewidth = 1,
                               frame.colour = "grey5",
                               frame.linewidth = 0.5,
                               barwidth = 0.5,
                               barheight = 5,
                               label.position = "left",
                               label.hjust = 1,
                               draw.ulim = FALSE,
                               draw.llim = FALSE
                               ))




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
                           "French\nGuiana", "Guyana", "Peru", "Suriname",
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


lab.wrap <- scales::label_wrap(10)


## EFFECT OF TENURE BY ADMINISTRATIVE AREA, FOREST TYPE ########################

if(!file.exists(file.data.vis)) {

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

    # Effect summaries. Combinations of tenure category and country with < 1000
    # observations are excluded.

    ten.sum[[region]]$arc <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label,
                  for.label = for.lab$for.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(for.lab, by = "for.label", all = TRUE) |> 
      merge(arc.ten.sum[n >= 1000], by = c("for_type", "it_type", "pa_type", "adm0"), all.x = TRUE)
    # Mark where CI includes 0:
    ten.sum[[region]]$arc[, ci_0 := ifelse(q2.5 < 0 & q97.5 > 0, "yes", "no")]

    # Overview of sample size per tenure category. All combinations are included.

    ten.sum[[region]]$n <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label,
                  for.label = for.lab$for.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(for.lab, by = "for.label", all = TRUE) |> 
      merge(rc.ten$groups[, !"ids"], by = c("for_type", "it_type", "pa_type", "adm0"), all.x = TRUE)
    ten.sum[[region]]$n[, samfrac := n / 1e7]
    
    rm(rc.ten)
  }

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(ten.sum = ten.sum) |>
  saveRDS(file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## TENURE EFFECTS BY COUNTRY ###################################################

plots <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  plots[[region]]$ten.arc$primary <-
    ten.sum[[region]]$arc[(is.na(it_type) | is.na(pa_type)) &
                          for_type == "primary"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = label_arc(mean, 1, FALSE),
                    fontface = ci_0),
                colour = "grey5", size = 2) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 8.5,
                   size = 0.3, colour = "grey5") +
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
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(subtitle = reg.title,
           fill = "Absolute difference\nin forest loss risk",
           y = "Primary forests\n", x = NULL) +
      ten_theme

  plots[[region]]$ten.arc$all <-
    ten.sum[[region]]$arc[(is.na(it_type) | is.na(pa_type)) &
                          is.na(for_type)] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = label_arc(mean, 1, FALSE),
                    fontface = ci_0),
                colour = "grey5", size = 2) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 8.5,
                   size = 0.3, colour = "grey5") +
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
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(fill = "Absolute difference\nin forest loss risk",
           y = "All forests\n", x = NULL) +
      ten_theme

}


## TENURE EFFECTS BY COUNTRY (OVERLAPPING AREAS) ###############################

plots.ov <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country (overlapping areas) …")

  sep.x <- nrow(reg.lab[[region]]) - 0.5

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  plots.ov[[region]]$ten.arc$primary <-
    ten.sum[[region]]$arc[it_type != "none" & pa_type != "none" &
                          for_type == "primary"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = label_arc(mean, 1, FALSE),
                    fontface = ci_0),
                colour = "grey5", size = 2) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 8.5,
                   size = 0.3, colour = "grey5") +
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
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(subtitle = reg.title,
           fill = "Absolute difference\nin forest loss risk",
           y = "Primary forests\n", x = NULL) +
      ten_theme

  plots.ov[[region]]$ten.arc$all <-
    ten.sum[[region]]$arc[it_type != "none" & pa_type != "none" &
                          is.na(for_type)] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = label_arc(mean, 1, FALSE),
                    fontface = ci_0),
                colour = "grey5", size = 2) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 8.5,
                   size = 0.3, colour = "grey5") +
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
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(fill = "Absolute difference\nin forest loss risk",
           y = "Primary forests\n", x = NULL) +
      ten_theme
}


## COMBINE INDIVIDUAL PLOTS ####################################################

combined.ten.arc <-

  with(plots,
       ((amz$ten.arc$primary & theme(axis.title.y = element_text(hjust = 0.5))) +
        (cam$ten.arc$primary & theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank()))
       ) /
       ((amz$ten.arc$all & theme(axis.title.y = element_text(hjust = 0.5),
                                 axis.text.x = element_blank())) +
        (cam$ten.arc$all & theme(axis.title.y = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.text.x = element_blank()))
       )) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = NULL) &
  theme(legend.justification = c(0,1))


svg(paste0(path.figures, "ten.arc.svg"), width = 6.7, height = 3)
combined.ten.arc
dev.off()

combined.ten.arc.ov <-
  with(plots.ov,
       ((amz$ten.arc$primary & theme(axis.title.y = element_text(hjust = 0.5))) +
        (cam$ten.arc$primary & theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank()))
       ) /
       ((amz$ten.arc$all & theme(axis.title.y = element_text(hjust = 0.5),
                                 axis.text.x = element_blank())) +
        (cam$ten.arc$all & theme(axis.title.y = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.text.x = element_blank()))
       )) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = NULL) &
  theme(legend.justification = c(0,1))

combined.ten.arc.ov <-
  with(plots.ov,
       ((amz$ten.arc$primary + theme(axis.title.y = element_text(hjust = 0.5),
                                     axis.title.x = element_text(hjust = 0))) +
        (cam$ten.arc$primary + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.title.x = element_text(hjust = 0)))
       ) /
       ((amz$ten.arc$other + theme(axis.title.y = element_text(hjust = 0.5))) +
        (cam$ten.arc$other + theme(axis.text.y = element_blank(),
                                   axis.title.y = element_blank()))
       )) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = NULL) &
  theme(legend.position = "right",
        legend.justification = c(0,1),
        legend.spacing.y = unit(5, "mm"),
        legend.title = element_text(size = rel(0.9)),
        legend.text = element_text(size = rel(0.75)))

svg(paste0(path.figures, "si.ten.arc.ov.svg"), width = 6.7, height = 3)
combined.ten.arc.ov
dev.off()


## LOOKUP TABLES ###############################################################

ten.arc.t <- list()

for(i in seq_along(regions)) {
  region <- regions[i]
  ten.arc.t[[region]] <- 
    ten.sum[[region]]$arc[it_type != "none" | pa_type != "none"
                    ][order(for.label, cat.label, reg.label),
                        .(for.label, cat.label, reg.label,
                          Mean = 
                            fifelse(is.na(mean),
                                    "--",
                                    label_arc(mean, 2, FALSE)),
                          CI =
                            fifelse(is.na(mean),
                                    "",
                                    paste0(" (", label_arc(q2.5, 2, FALSE),
                                           "; ", label_arc(q97.5, 2, FALSE), ")")))] |>
    melt(measure.vars = c("Mean", "CI"),
         variable.name = "stat",
         value.name = "risk") |>
    dcast(for.label + cat.label + stat ~ reg.label, value.var = "risk")
  setnames(ten.arc.t[[region]],
           c("for.label", "cat.label", "stat"),
           c("Forest type", "Tenure category", "Estimate"))
  fwrite(ten.arc.t[[region]], paste0(path.tables, region, ".ten.arc.csv"))
}


ten.n.t <- list()

for(i in seq_along(regions)) {
  region <- regions[i]
  ten.n.t[[region]] <- 
    ten.sum[[region]]$n[it_type != "none" | pa_type != "none"
                        ][order(for.label, cat.label, reg.label),
                          .(for.label, cat.label, reg.label,
                            N = 
                              fifelse(is.na(n), "--", as.character(n)),
                            Fraction =
                              fifelse(is.na(samfrac),
                                      "",
                                      paste0("(",
                                             format(round(samfrac * 100, 2),
                                                    nsmall = 2,
                                                    trim = TRUE),
                                             "%)"))
                            )] |>
    melt(measure.vars = c("N", "Fraction"),
         variable.name = "stat",
         value.name = "sample") |>
    dcast(for.label + cat.label + stat ~ reg.label, value.var = "sample")
  setnames(ten.n.t[[region]],
           c("for.label", "cat.label"),
           c("Forest type", "Tenure category"))
  fwrite(ten.n.t[[region]], paste0(path.tables, region, ".ten.n.csv"))
}

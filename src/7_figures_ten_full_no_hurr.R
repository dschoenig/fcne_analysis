library(data.table)
library(ggplot2)
library(ggdist)
library(patchwork)
library(colorspace)
library(stringi)

source("utilities.R")

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
path.marginal <- paste0(path.base, "models/marginal/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.tables <- paste0(path.base, "results/tables/")
if(!dir.exists(path.tables)) dir.create(path.tables, recursive = TRUE)

file.data.vis <- paste0(path.data.vis, "tenure_adm_no_hurr.rds")
file.fig.af <- paste0(path.figures, "ten.af.full.no_hurr.png")
file.fig.pf <- paste0(path.figures, "ten.pf.full.no_hurr.png")

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
                                    margin = margin(t = 0, b = 9),
                                    lineheight = rel(1.15)),
        legend.text = element_text(size = rel(0.65)),
        legend.spacing.y = unit(2, "pt"),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0,
                                   colour = "grey5"),
        axis.text.y = element_text(hjust = 0, colour = "grey5"),
        )


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

for.lab <-
  data.table(for_type = c("primary", NA),
             for.label = c("Primary forests", "All forests"))
for.lab[, for.label := factor(for.label, levels = for.label)]

mar.title <- "Absolute difference\nin forest loss risk\n(vs. non-IT, non-PA)"

## EFFECT OF TENURE BY ADMINISTRATIVE AREA, FOREST TYPE ########################

if(!file.exists(file.data.vis)) {

  ten.sum <- list()

  for(i in seq_along(regions)) {
    
    region <- regions[i]

    message(paste0("Preparing data for region `", region, "` …"))
   
    if(
    file.mar.sam <- paste0(path.marginal, region, "/", region, ".sam.rds")
    name.mar.ten.af <- paste0(region, ".ten.af.itpa.all.no_hurr.rds")
    file.mar.ten.af <- paste0(path.marginal, region, "/", name.mar.ten.af)
    name.mar.ten.pf <- paste0(region, ".ten.pf.itpa.all.no_hurr.rds")
    file.mar.ten.pf <- paste0(path.marginal, region, "/", name.mar.ten.pf)

    # Tenure categories by administrative areas

    message("Tenure category by administrative areas …")

    mar.sam <- readRDS(file.mar.sam)
    mar.ten.af <-
      readRDS(file.mar.ten.af) |>
      merge(mar.sam[[name.mar.ten.af]])
    mar.ten.pf <-
      readRDS(file.mar.ten.pf) |>
      merge(mar.sam[[name.mar.ten.pf]])

    mar.ten.af[, for_type := factor(NA, levels = c("primary", "other"))]
    mar.ten.pf[, for_type := factor("primary", levels = c("primary", "other"))]

    # Effect summaries. Combinations of tenure category and country with < 1000
    # observations are excluded.

    mar.ten <-
      rbind(mar.ten.pf, mar.ten.af) |>
      _[,
        .(n = unique(n),
          n.frac = unique(n)/1e7,
          mar.mean = mean(marginal),
          mar.median = median(marginal),
          mar.sd = sd(marginal),
          mar.q2.5 = quantile(marginal, 0.025),
          mar.q97.5 = quantile(marginal, 0.975),
          mar.p.tost = max(sum(marginal > 0)/.N, sum(marginal < 0)/.N)),
        by = c("for_type", "it_type", "pa_type", "adm0")]
    mar.ten[, mar.lab.mean := label_arc(mar.mean, 1, FALSE)]
    mar.ten[, ci_0 := ifelse(mar.q2.5 < 0 & mar.q97.5 > 0, "yes", "no")]
    mar.ten[ci_0 == "yes", mar.lab.mean := paste0("(", mar.lab.mean, ")")]

    ten.sum[[region]]$mar <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label,
                  for.label = for.lab$for.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(for.lab, by = "for.label", all = TRUE) |> 
      merge(mar.ten[n > 1000], by = c("for_type", "it_type", "pa_type", "adm0"),
            all.x = TRUE)

    # Mark where CI includes 0:
    ten.sum[[region]]$mar[, ci_0 := ifelse(mar.q2.5 < 0 & mar.q97.5 > 0, "yes", "no")]
    ten.sum[[region]]$mar[, mar.lab.shade := ifelse(abs(mar.mean) < 0.12, "dark", "light")]

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
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
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
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(subtitle = reg.title,
           fill = mar.title,
           y = "Primary forests\n", x = NULL) +
      ten_theme

  plots[[region]]$ten.mar$all <-
    ten.sum[[region]]$mar[((it_type == "none"  & pa_type != "none") |
                           (it_type != "none"  & pa_type == "none")) &
                          is.na(for_type)] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
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
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(subtitle = reg.title,
           fill = mar.title,
           y = "All forests\n", x = NULL) +
      ten_theme
}




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

  plots.ov[[region]]$ten.mar$primary <-
    ten.sum[[region]]$mar[it_type != "none"  & pa_type != "none" &
                          for_type == "primary"] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
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
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(subtitle = reg.title,
           fill = mar.title,
           y = "Primary forests\n", x = NULL) +
      ten_theme

  plots.ov[[region]]$ten.mar$all <-
    ten.sum[[region]]$mar[it_type != "none"  & pa_type != "none" &
                          is.na(for_type)] |>
    ggplot() +
      geom_tile(aes(x = reg.label, y = cat.label, fill = mar.mean),
                size = 1, colour = "white") +
      geom_text(aes(x = reg.label, y = cat.label,
                    label = mar.lab.mean,
                    fontface = ci_0,
                    colour = mar.lab.shade),
                size = 1.65) +
      geom_segment(x = sep.x, y = 0.5, xend = sep.x, yend = 4.5,
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
      scale_colour_manual(values = c(dark = "grey15", light = "grey85"),
                          guide = "none") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      coord_fixed() +
      ten_guide_fill +
      labs(subtitle = reg.title,
           fill = mar.title,
           y = "All forests\n", x = NULL) +
      ten_theme

}


p.pf <-
(((plots$amz$ten.mar$primary + labs(y = "Non-overlapping areas\n")) &
   theme(
         axis.text.y = element_text(hjust = 1),
         axis.title.y = element_text(hjust = 0.5,
                                     margin = margin(r = 18.1),
                                     size = rel(0.85)),
        plot.margin = unit(c(5, 5, 0, 5), "pt")
        )) +
 (plots$cam$ten.mar$primary &
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(5, 5, 0, 5), "pt")
        ))
) /
(((plots.ov$amz$ten.mar$primary + labs(y = "Overlapping areas\n")) &
   theme(title = element_blank(),
         axis.text.y = element_text(hjust = 1),
         axis.title.y = element_text(hjust = 0.5,
                                     size = rel(0.85)),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         plot.margin = unit(c(0, 5, 5, 5), "pt")
         )) +
 ((plots.ov$cam$ten.mar$primary &
   theme(title = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         plot.margin = unit(c(0, 5, 5, 5), "pt")
         ))
)) +
plot_layout(guides = "collect") +
plot_annotation(tag_levels = NULL,
                title = "(A) Primary forests",
                theme = theme(plot.title = element_text(family = "IBMPlexSans",
                                                        size = 8))) &
theme(legend.justification = c(0,1))
png(file.fig.pf, width = 7, height = 3, unit = "in", res = 600)
p.pf
dev.off()


p.all <-
(((plots$amz$ten.mar$all + labs(y = "Non-overlapping areas\n")) &
   theme(
         axis.text.y = element_text(hjust = 1),
         axis.title.y = element_text(hjust = 0.5,
                                     margin = margin(r = 18.1),
                                     size = rel(0.85)),
        plot.margin = unit(c(5, 5, 0, 5), "pt")
        )) +
 (plots$cam$ten.mar$all &
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(5, 5, 0, 5), "pt")
        ))
) /
(((plots.ov$amz$ten.mar$all + labs(y = "Overlapping areas\n")) &
   theme(title = element_blank(),
         axis.text.y = element_text(hjust = 1),
         axis.title.y = element_text(hjust = 0.5,
                                     size = rel(0.85)),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         plot.margin = unit(c(0, 5, 5, 5), "pt")
         )) +
 ((plots.ov$cam$ten.mar$all &
   theme(title = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         plot.margin = unit(c(0, 5, 5, 5), "pt")
         ))
)) +
plot_layout(guides = "collect") +
plot_annotation(tag_levels = NULL,
                title = "(B) All forests",
                theme = theme(plot.title = element_text(family = "IBMPlexSans",
                                                        size = 8))) &
theme(legend.justification = c(0,1))
png(file.fig.af, width = 7, height = 3, unit = "in", res = 600)
p.all
dev.off()

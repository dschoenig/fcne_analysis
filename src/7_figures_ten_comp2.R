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

file.data.vis <- paste0(path.data.vis, "tenure_comp_itpa", hurr_suf, ".rds")
file.fig.ten.comp <- paste0(path.figures, "ten.comp.itpa", hurr_suf, ".png")
file.table.ten.comp <- paste0(path.data.vis, "tenure_comp_itpa", hurr_suf, ".csv")

regions <- c("amz", "cam")


## COLOURS AND LABELS

## Colours for tenure categories
# col.cat <- c("#CC79A7", "#009E73")
# col.cat <- c("#984ea3", "#4daf4a")
# col.div <- diverging_hcl(20, palette = "Purple-Green")
# col.cat <- col.div[c(3, 17)]
# col.cat <- rep("#377eb8", 3)
# names(col.cat) <- c("Mixed tenure (all forms)\n—against only IT (all forms)",
#                     "Mixed tenure; IT, rec.\n—against only IT rec.",
#                     "Mixed tenure; IT, not rec.\n—against only IT not rec.")
col.cat <- rep("#377eb8", 7)
names(col.cat) <- c("Mixed tenure (all forms)\n—against only IT (all forms)",
                    "Mixed tenure; IT, rec.\n—against only IT rec.",
                    "Mixed tenure; IT, not rec.\n—against only IT not rec.",
                    "IT, rec.; PA, cat. I-IV\n—against only IT rec.",
                    "IT, rec.; PA, cat. V-VI\n—against only IT rec.",
                    "IT, not rec.; PA, cat. I-IV\n—against only IT not rec.",
                    "IT, not rec.; PA, cat. V-VI\n—against only IT not rec.")

base.size <- 7 
post_theme <-
  theme_light(base_family = "IBMPlexSans",
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
                                   size = rel(1.2),
                                   margin = margin(t = base.size/2)),
        axis.text.y = element_text(color = "black",
                                   size = rel(1.25),
                                   lineheight = rel(1.15),
                                   margin = margin(r = base.size/2),
                                   hjust = 0),
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
        plot.tag = element_text(face = "bold"),
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
  data.table(cat.label = c(
                           "Mixed tenure (all forms)\n—against only IT (all forms)",
                           "Mixed tenure; IT, rec.\n—against only IT rec.",
                           "Mixed tenure; IT, not rec.\n—against only IT not rec.",
                           "Mixed tenure (all forms)\n—against only PA (all forms)",
                           "Mixed tenure; PA, cat. I-IV\n—against only PA, cat. I-IV",
                           "Mixed tenure; PA, cat. V-VI\n—against only PA, cat. V-VI"
                           ),
             mar_type = c(rep("pa", 3), rep("it", 3)),
             it_type = c(
                         NA, "recognized", "not_recognized",
                         NA, NA, NA
                         ),
             pa_type = c(
                         NA, NA, NA,
                         NA, "indirect_use", "direct_use"
                         ))
# cat.lab[, cat.label := stri_pad_left(cat.label, width = max(stri_width(cat.label)))]
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]


cat.lab <- 
  data.table(cat.label = c(
                           "Mixed tenure (all forms)\n—against only IT (all forms)",
                           "Mixed tenure; IT, rec.\n—against only IT rec.",
                           "Mixed tenure; IT, not rec.\n—against only IT not rec.",
                           "IT, rec.; PA, cat. I-IV\n—against only IT rec.",
                           "IT, rec.; PA, cat. V-VI\n—against only IT rec.",
                           "IT, not rec.; PA, cat. I-IV\n—against only IT not rec.",
                           "IT, not rec.; PA, cat. V-VI\n—against only IT not rec."),
             mar_type = c(rep("pa", 7)),
             it_type = c(
                         NA, "recognized", "not_recognized",
                         "recognized", "recognized", "not_recognized", "not_recognized"
                         ),
             pa_type = c(
                         NA, NA, NA,
                         "indirect_use", "direct_use", "indirect_use", "direct_use"
                         ))
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
             dist.label = c("Long-term disturbance\n(deforestation)",
                            "Short-term disturbance\n(not followed by deforestation)"))
dist.lab[, dist.label := factor(dist.label, levels = dist.label)]


wrap_title <- function(x, width = 25, ...) {
  paste(stri_wrap(x,
                  width = width,
                  whitespace_only = TRUE),
        collapse = "\n")
}

area.title <- wrap_title("Area covered by least-disturbed TMF in 2010 (km²)")
mar.title.l <- "Absolut marginal difference in proportion of area affected (km²)"
mar.title <- wrap_title(mar.title.l)



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

    area.undist <-
      readRDS(file.area)$undist.ov

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
    mar.ten.deg.it[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "it")]
    mar.ten.def.pa[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "pa")]
    mar.ten.deg.pa[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "pa")]

    mar.post <-
      rbind(
            mar.ten.def.it,
            mar.ten.deg.it,
            mar.ten.def.pa,
            mar.ten.deg.pa
            ) |>
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
               by = c("dist_type", "mar_type", "it_type", "pa_type", "adm0")]
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

    ten.sum[[region]]$mar <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label,
                  dist.label = dist.lab$dist.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(dist.lab, by = "dist.label", all = TRUE) |> 
      merge(mar.ten[n.fac > 1000], by = c("dist_type", "mar_type", "it_type", "pa_type", "adm0"),
            all.x = TRUE)


    ten.sum[[region]]$post <-
      expand.grid(cat.label = cat.lab$cat.label,
                  reg.label = reg.lab[[region]]$reg.label,
                  dist.label = dist.lab$dist.label) |>
      as.data.table() |>
      merge(cat.lab, by = "cat.label", all = TRUE) |>
      merge(reg.lab[[region]], by = "reg.label", all = TRUE) |>
      merge(dist.lab, by = "dist.label", all = TRUE) |> 
      merge(mar.post[n.fac > 1000], by = c("dist_type", "mar_type", "it_type", "pa_type", "adm0"),
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


plots.mar <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  comp.mar.xlim <- list(amz = c(-0.03, 0.01), cam = c(-0.2, 0.1))
  comp.mar.sh <- c(amz = 0.75, cam = 0.875)

  plots.mar[[region]] <-
    # ten.sum[[region]]$post[mar_type == "pa"] |>
    ten.sum[[region]]$post[] |>
    ggplot() +
      stat_slab(aes(x = as.numeric(marginal), y = cat.label,
                    fill = cat.label, fill_ramp = after_stat(level)),
                .width = c(0.5, 0.9, 1),
                alpha = 0.8,
                height = 1,
                scale = 0.8,
                n = 1001,
                normalize = "all") +
      stat_spike(aes(x = as.numeric(marginal), y = cat.label,
                     linetype = after_stat(at)), at = c(Median = "median") ,
                 height = comp.mar.sh[region],
                 size = 0.8,
                 linewidth = 0.3,
                 # height = 0.8,
                 # normalize = "panels"
                 ) +
      geom_hline(yintercept = 1:length(col.cat), linewidth = 0.1) +
      geom_vline(xintercept = 0, linewidth = 0.2, linetype = "dashed") +
      scale_fill_manual(values = col.cat, guide = "none") +
      scale_fill_ramp_discrete(breaks = c(0.5, 0.9)) +
      scale_y_discrete(limits = rev) +
      scale_x_continuous(labels = scales::label_number(accuaracy = 1e3)) +
      guides(linetype = guide_legend(order = 1)) +
      coord_cartesian(xlim =  comp.mar.xlim[[region]]) +
      facet_grid(rows = vars(dist.label)) +
      labs(x = mar.title.l, y = "Direct tenure comparison",
           fill_ramp = "Credible interval", linetype = "Point estimate") +
      post_theme

}


# plots.mar$amz + plots.mar$cam + plot_layout(guides = "collect")

p.ten.comp <-
(plots.mar$amz + labs(subtitle = "Amazon") &
  theme(
        plot.margin = unit(c(0, 20, 0, 0), "pt"),
        plot.title.position = "plot")) +
(plots.mar$cam & labs(subtitle = "Central America") &
  theme(
        plot.margin = unit(c(15, 5, 0, 10), "pt"),
        plot.title.position = "plot")) +
plot_layout(nrow = 1, guides = "collect")

png(file.fig.ten.comp, width = 10, height = 4.25, unit = "in", res = 600)
p.ten.comp
dev.off()

tab.ten <-
  list(AMZ = ten.sum$amz$mar[, -c("ci_0")],
       CAM = ten.sum$cam$mar[, -c("ci_0")]) |>
  rbindlist(idcol = "study_region")
setorder(tab.ten, study_region, dist_type, adm0, it_type, pa_type)
fwrite(tab.ten, file.table.ten.comp)


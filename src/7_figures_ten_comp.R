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

# hurr_type <- "no_hurr"
# overwrite <- TRUE
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

file.data.vis <- paste0(path.data.vis, "tenure_comp", hurr_suf, ".rds")
file.fig.ten.comp <- paste0(path.figures, "ten.comp", hurr_suf, ".png")
file.fig.ten.comp.c <- paste0(path.figures, "ten.comp.c", hurr_suf, ".png")
file.table.ten.comp <- paste0(path.data.vis, "tenure_comp", hurr_suf, ".csv")

regions <- c("amz", "cam")


## COLOURS AND LABELS

## Colours for tenure categories
# col.cat <- c("#CC79A7", "#009E73")
# col.cat <- c("#984ea3", "#4daf4a")
col.div <- diverging_hcl(20, palette = "Purple-Green")
col.cat <- col.div[c(3, 17)]
names(col.cat) <- c("IL, recognized vs. IL, not recognized\n(incl. mixed regimes)",
                    "PA, category I-IV vs. PA, category V-VI\n(incl. mixed regimes)")

col.est <- c("#e41a1c", "#377eb8", "#984ea3")
names(col.est) <- c("Factual\n(observed)", "Counterfactual\n(expected)", "Marginal\ndifference")
col.est.pt <- col.est
col.est.pt[2] <- "#FFFFFF"
size.est.pt <- c(0.2, 0.4, 0.3)
names(size.est.pt) <- names(col.est)
size.est.pt.comp <- c(0.15, 0.3, 0.225)
names(size.est.pt.comp) <- names(col.est)
stroke.pt.comp <- 0.5
linewidth.comp <- 0.3

ten.cat.angle <- 45

est.lab <- names(col.est)
est.lab <- factor(est.lab, levels = est.lab)


base.size <- 7 
post_theme <-
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

# cat.lab <- 
#   data.table(cat.label = c(
#                            "IL, recognized\n(incl. mixed regimes)\n—against IL, not recognized",
#                            "IL, rec., outside PA\n—against IL not rec., outside PA",
#                            "IL, rec.; PA, cat. I-IV\n—against IL, not rec., PA, cat. I-IV",
#                            "IL, rec.; PA, cat. V-VI\n—against IL, not rec., PA, cat. V-VI",
#                            "IL, not recognized\n(including mixed regimes)\n—against IL, recognized",
#                            "IL, not rec., outside PA\n—against IL rec., outside PA",
#                            "IL, not rec.; PA, cat. I-IV\n—against IL, rec., PA, cat. I-IV",
#                            "IL, not rec.; PA, cat. V-VI\n—against IL, rec., PA, cat. V-VI",
#                            "PA, category I-IV\n(incl. mixed regimes)\n—against PA, category V-VI",
#                            "PA, cat. I-IV, outside IL\n—against PA, cat. V-VI, outside IL",
#                            "PA, cat. I-IV; IL, rec.\n—against PA, cat. V-VI; IL, rec.",
#                            "PA, cat. I-IV; IL, not rec.\n—against PA, cat. V-VI; IL, not rec.",
#                            "PA, category V-VI\n(including mixed regimes)\n—against PA, category I-IV",
#                            "PA, cat. V-VI, outside IL\n—against PA, cat. I-IV, outside IL",
#                            "PA, cat. V-VI; IL, rec.\n—against PA, cat. I-IV; IL, rec.",
#                            "PA, cat. V-VI; IL, not rec.\n—against PA, cat. I-IV; IL, not rec."
#                            ),
#              mar_type = c(rep("rec", 4), rep("nrec", 4), rep("ind", 4), rep("dir", 4)),
#              it_type = c(
#                          NA, "recognized", "recognized", "recognized",
#                          NA, "not_recognized", "not_recognized", "not_recognized",
#                          NA, "none", "recognized", "not_recognized",
#                          NA, "none", "recognized", "not_recognized"
#                          ),
#              pa_type = c(
#                          NA, "none", "indirect_use", "direct_use",
#                          NA, "none", "indirect_use", "direct_use",
#                          NA, "indirect_use", "indirect_use", "indirect_use",
#                          NA, "direct_use", "direct_use", "direct_use"
#                          ))
# # cat.lab[, cat.label := stri_pad_left(cat.label, width = max(stri_width(cat.label)))]
# cat.lab[, cat.label := factor(cat.label, levels = cat.label)]


cat.lab <- 
  data.table(cat.label = c(
                           "IL, recognized vs. IL, not recognized\n(incl. mixed regimes)",
                           "IL, recognized vs. IL, not recognized\n(outside PA)",
                           "IL, recognized vs. IL, not recognized\n(inside PA)",
                           # "IL, rec.; PA, cat. I-IV\n—vs. IL, not rec., PA, cat. I-IV",
                           # "IL, rec.; PA, cat. V-VI\n—vs. IL, not rec., PA, cat. V-VI",
                           "PA, category I-IV vs. PA, category V-VI\n(incl. mixed regimes)",
                           "PA, category I-IV vs. PA, category V-VI\n(outside IL)",
                           "PA, category I-IV vs. PA, category V-VI\n(inside IL)"
                           # "PA, cat. I-IV; IL, rec.\n—vs. PA, cat. V-VI; IL, rec.",
                           # "PA, cat. I-IV; IL, not rec.\n—vs. PA, cat. V-VI; IL, not rec."
                           ),
             mar_type = c(
                          "rec", "rec", "recov",
                          # "rec", "rec",
                          "ind", "ind", "indov"
                          # "ind", "ind"
                          ),
             it_type = c(
                         NA, "recognized", NA,
                         # "recognized", "recognized",
                         NA, "none", NA
                         # "recognized", "not_recongized"
                         ),
             pa_type = c(
                         NA, "none", NA,
                         # "indirect_use", "direct_use",
                         NA, "indirect_use", NA
                         # "indirect_use", "indirect_use"
                         ))
# cat.lab[, cat.label := stri_pad_left(cat.label, width = max(stri_width(cat.label)))]
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]
cat.lab <- cat.lab[c(1, 6)]

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
dist.lab.2l <- c("Long-term disturbance\n(deforestation)",
                 "Short-term disturbance\n(not followed by deforestation)")
dist.lab.2l <- factor(dist.lab.2l, levels = dist.lab.2l)
names(dist.lab.2l) <- dist.lab$dist.label

wrap_title <- function(x, width = 25, ...) {
  paste(stri_wrap(x,
                  width = width,
                  whitespace_only = TRUE),
        collapse = "\n")
}

area.title <- wrap_title("Area covered by least-disturbed TMF in 2010 (km²)")
mar.title.l <- "Absolut marginal difference in area affected by disturbances (km²)"
mar.title.2l <- "Absolut marginal difference in\narea affected by disturbances (km²)"
mar.title <- wrap_title(mar.title.l)
mar.title.def.l <- "Absolute marginal difference in proportion of area affected"
mar.title.def.2l <- "Absolute marginal difference in\nproportion of area affected"
mar.title.def <- wrap_title(mar.title.def.l)
mar.title.deg.l <- "Absolute marginal difference in proportion of area affected"
mar.title.deg.2l <- "Absolute marginal difference in\nproportion of area affected"
mar.title.deg <- wrap_title(mar.title.deg.l)
mar.title.def.av.l <- "Absolute marginal difference in area affected (km²)"
mar.title.def.av.2l <- "Absolute marginal difference\nin area affected (km²)"
mar.title.def.av <- wrap_title(mar.title.def.av.l)
mar.title.deg.av.l <- "Absolute marginal difference in area affected (km²)"
mar.title.deg.av.2l <- "Absolute marginal difference\nin area affected (km²)"
mar.title.deg.av <- wrap_title(mar.title.deg.av.l)
abs.title.def <- "Proportion of area affected"
abs.title.deg <- "Proportion of area affected"
abs.area.title.def <- "Area affected (km²)"
abs.area.title.deg <- "Area affected (km²)"


## EFFECT OF TENURE BY ADMINISTRATIVE AREA, FOREST TYPE ########################

if(!file.exists(file.data.vis) | overwrite == TRUE) {

  ten.sum <- list()

  for(i in seq_along(regions)) {
    
    region <- regions[i]

    reg.title <- switch(region,
                        cam = "Central America",
                        amz = "Amazon")

    message(paste0("Preparing data for region `", region, "` …"))
   
    if(region == "cam") {
      hurr_suf_mar <- hurr_suf
    } else {
      hurr_suf_mar <- ""
    }

    file.area <- paste0(path.data.proc, region, ".sumstats.area", hurr_suf_mar, ".rds")
    file.mar.sam <- paste0(path.marginal, region, "/", region, ".sam.rds")
    name.mar.ten_comp.rec.sam <- paste0(region, ".ten_comp.rec.all", hurr_suf_mar, ".rds")
    name.mar.ten_comp.recov.sam <- paste0(region, ".ten_comp.rec.ov", hurr_suf_mar, ".rds")
    name.mar.ten_comp.ind.sam <- paste0(region, ".ten_comp.ind.all", hurr_suf_mar, ".rds")
    name.mar.ten_comp.indov.sam <- paste0(region, ".ten_comp.ind.ov", hurr_suf_mar, ".rds")
    name.mar.ten_comp.def.rec <- paste0(region, ".def.ten_comp.rec.all", hurr_suf_mar, ".rds")
    file.mar.ten_comp.def.rec <- paste0(path.marginal, region, "/", name.mar.ten_comp.def.rec)
    name.mar.ten_comp.deg.rec <- paste0(region, ".deg.ten_comp.rec.all", hurr_suf_mar, ".rds")
    file.mar.ten_comp.deg.rec <- paste0(path.marginal, region, "/", name.mar.ten_comp.deg.rec)
    name.mar.ten_comp.def.recov <- paste0(region, ".def.ten_comp.rec.ov", hurr_suf_mar, ".rds")
    file.mar.ten_comp.def.recov <- paste0(path.marginal, region, "/", name.mar.ten_comp.def.recov)
    name.mar.ten_comp.deg.recov <- paste0(region, ".deg.ten_comp.rec.ov", hurr_suf_mar, ".rds")
    file.mar.ten_comp.deg.recov <- paste0(path.marginal, region, "/", name.mar.ten_comp.deg.recov)
    name.mar.ten_comp.def.ind <- paste0(region, ".def.ten_comp.ind.all", hurr_suf_mar, ".rds")
    file.mar.ten_comp.def.ind <- paste0(path.marginal, region, "/", name.mar.ten_comp.def.ind)
    name.mar.ten_comp.deg.ind <- paste0(region, ".deg.ten_comp.ind.all", hurr_suf_mar, ".rds")
    file.mar.ten_comp.deg.ind <- paste0(path.marginal, region, "/", name.mar.ten_comp.deg.ind)
    name.mar.ten_comp.def.indov <- paste0(region, ".def.ten_comp.ind.ov", hurr_suf_mar, ".rds")
    file.mar.ten_comp.def.indov <- paste0(path.marginal, region, "/", name.mar.ten_comp.def.indov)
    name.mar.ten_comp.deg.indov <- paste0(region, ".deg.ten_comp.ind.ov", hurr_suf_mar, ".rds")
    file.mar.ten_comp.deg.indov <- paste0(path.marginal, region, "/", name.mar.ten_comp.deg.indov)

    # Tenure categories by administrative areas

    message("Tenure category by administrative areas …")

    area.sum <- readRDS(file.area)

    area.undist <- area.sum$undist.comp
    area.undist.ov <- area.sum$undist.ov

    mar.sam <- readRDS(file.mar.sam)

    mar.ten_comp.def.rec <-
      readRDS(file.mar.ten_comp.def.rec) |>
      merge(mar.sam[[name.mar.ten_comp.rec.sam]])
    mar.ten_comp.deg.rec <-
      readRDS(file.mar.ten_comp.deg.rec) |>
      merge(mar.sam[[name.mar.ten_comp.rec.sam]])
    mar.ten_comp.def.recov <-
      readRDS(file.mar.ten_comp.def.recov) |>
      merge(mar.sam[[name.mar.ten_comp.recov.sam]])
    mar.ten_comp.deg.recov <-
      readRDS(file.mar.ten_comp.deg.recov) |>
      merge(mar.sam[[name.mar.ten_comp.recov.sam]])
    mar.ten_comp.def.ind <-
      readRDS(file.mar.ten_comp.def.ind) |>
      merge(mar.sam[[name.mar.ten_comp.ind.sam]])
    mar.ten_comp.deg.ind <-
      readRDS(file.mar.ten_comp.deg.ind) |>
      merge(mar.sam[[name.mar.ten_comp.ind.sam]])
    mar.ten_comp.def.indov <-
      readRDS(file.mar.ten_comp.def.indov) |>
      merge(mar.sam[[name.mar.ten_comp.indov.sam]])
    mar.ten_comp.deg.indov <-
      readRDS(file.mar.ten_comp.deg.indov) |>
      merge(mar.sam[[name.mar.ten_comp.indov.sam]])

    mar.ten_comp.def.rec[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "rec")]
    mar.ten_comp.deg.rec[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "rec")]
    mar.ten_comp.def.recov[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "recov")]
    mar.ten_comp.deg.recov[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "recov")]
    mar.ten_comp.def.ind[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "ind")]
    mar.ten_comp.deg.ind[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "ind")]
    mar.ten_comp.def.indov[,
                   `:=`(dist_type = factor("def", levels = c("def", "deg")),
                        mar_type = "indov")]
    mar.ten_comp.deg.indov[,
                   `:=`(dist_type = factor("deg", levels = c("def", "deg")),
                        mar_type = "indov")]

    mar.post.nov <-
      rbind(
            mar.ten_comp.def.rec,
            mar.ten_comp.deg.rec,
            mar.ten_comp.def.ind,
            mar.ten_comp.deg.ind
            ) |>
      merge(area.undist[, -"area.rel"], by = c("mar_type", "it_type", "pa_type", "adm0"))
    mar.post.ov <-
      rbind(
            mar.ten_comp.def.recov,
            mar.ten_comp.deg.recov,
            mar.ten_comp.def.indov,
            mar.ten_comp.deg.indov
            ) |>
      merge(area.undist.ov[, -"area.rel"], by = c("it_type", "pa_type", "adm0"))
    mar.post <- rbind(mar.post.nov, mar.post.ov)
    mar.post[, `:=`(area.prop.mar = marginal * area,
                    area.prop.fac = factual * area,
                    area.prop.cf = counterfactual * area)]


    agg.cols <- c("dist_type", "mar_type", "it_type", "pa_type", "adm0")
    mar.post.hdi <-
      mar.post[,
               c(
                 hdci2(marginal, .width = c(0.9), "mar."),
                 hdci2(marginal, .width = c(0.5), "mar."),
                 hdci2(factual, .width = c(0.9), "fac."),
                 hdci2(factual, .width = c(0.5), "fac."),
                 hdci2(counterfactual, .width = c(0.9), "cf."),
                 hdci2(counterfactual, .width = c(0.5), "cf."),
                 hdci2(as.numeric(area.prop.mar), .width = c(0.9), "area.mar."),
                 hdci2(as.numeric(area.prop.mar), .width = c(0.5), "area.mar."),
                 hdci2(as.numeric(area.prop.fac), .width = c(0.9), "area.fac."),
                 hdci2(as.numeric(area.prop.fac), .width = c(0.5), "area.fac."),
                 hdci2(as.numeric(area.prop.cf), .width = c(0.9), "area.cf."),
                 hdci2(as.numeric(area.prop.cf), .width = c(0.5), "area.cf.")
                 ),
               by = agg.cols]
    mar.post.n <-
      mar.post[,
               .(
                 n.fac = unique(n.fac),
                 n.cf = unique(n.cf),
                 n.frac = unique(n.fac)/1e7),
               by = agg.cols]
    mar.ten <-
      mar.post[,
               .(
                 mar.mean = mean(marginal),
                 mar.median = median(marginal),
                 mar.sd = sd(marginal),
                 # mar.mad = mad(marginal),
                 # mar.q5 = quantile(marginal, 0.05),
                 # mar.q25 = quantile(marginal, 0.25),
                 # mar.q75 = quantile(marginal, 0.75),
                 # mar.q95 = quantile(marginal, 0.95),
                 fac.mean = mean(factual),
                 fac.median = median(factual),
                 fac.sd = sd(factual),
                 # fac.mad = mad(factual),
                 # fac.q5 = quantile(factual, 0.05),
                 # fac.q25 = quantile(factual, 0.25),
                 # fac.q75 = quantile(factual, 0.75),
                 # fac.q95 = quantile(factual, 0.95),
                 cf.mean = mean(counterfactual),
                 cf.median = median(counterfactual),
                 cf.sd = sd(counterfactual),
                 # cf.mad = mad(counterfactual),
                 # cf.q5 = quantile(counterfactual, 0.05),
                 # cf.q25 = quantile(counterfactual, 0.25),
                 # cf.q75 = quantile(counterfactual, 0.75),
                 # cf.q95 = quantile(counterfactual, 0.95),
                 area.mar.mean = mean(area.prop.mar),
                 area.mar.median = median(area.prop.mar),
                 area.mar.sd = sd(area.prop.mar),
                 # area.mar.mad = mad(area.prop.mar),
                 # area.mar.q5 = quantile(area.prop.mar, 0.05),
                 # area.mar.q25 = quantile(area.prop.mar, 0.25),
                 # area.mar.q75 = quantile(area.prop.mar, 0.75),
                 # area.mar.q95 = quantile(area.prop.mar, 0.95),
                 area.fac.mean = mean(area.prop.fac),
                 area.fac.median = median(area.prop.fac),
                 area.fac.sd = sd(area.prop.fac),
                 # area.fac.mad = mad(area.prop.fac),
                 # area.fac.q5 = quantile(area.prop.fac, 0.05),
                 # area.fac.q25 = quantile(area.prop.fac, 0.25),
                 # area.fac.q75 = quantile(area.prop.fac, 0.75),
                 # area.fac.q95 = quantile(area.prop.fac, 0.95),
                 area.cf.mean = mean(area.prop.cf),
                 area.cf.median = median(area.prop.cf),
                 area.cf.sd = sd(area.prop.cf)
                 # area.cf.mad = mad(area.prop.cf),
                 # area.cf.q5 = quantile(area.prop.cf, 0.05),
                 # area.cf.q25 = quantile(area.prop.cf, 0.25),
                 # area.cf.q75 = quantile(area.prop.cf, 0.75),
                 # area.cf.q95 = quantile(area.prop.cf, 0.95),
                 # mar.prob.pos = sum(marginal > 0)/.N,
                 # mar.prob.neg = sum(marginal < 0)/.N
                 ),
               by = agg.cols] |>
    merge(mar.post.hdi, by = agg.cols) |>
    merge(mar.post.n, by = agg.cols)
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
    mar.ten[, ci_0 := ifelse(mar.hdi90l < 0 & mar.hdi90u > 0, "yes", "no")]
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


    ten.sum[[region]]$post[, study.label := factor(reg.title, levels = c("Amazon", "Central America"))]

  }

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(ten.sum = ten.sum) |>
  saveRDS(file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## AVOIDED DISTURBANCES (AREA) ########################################


plots.area.av <- list()
ten.sum.av.l <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  comp.av.xlim <- list(amz = c(-5e4, 5e3), cam = c(-3e3, 1e3))
  comp.av.sh <- c(amz = 0.575, cam = 0.725)

  # plots.area.av[[region]] <-
  #   ten.sum[[region]]$post[
  #                          # is.na(it_type) & is.na(pa_type) &
  #                          is.na(adm0) &
  #                          # mar_type %in% c("rec", "ind")] |>
  #                          mar_type %in% c("rec", "recov", "ind", "indov")] |>
  #   ggplot() +
  #     stat_slab(aes(x = as.numeric(area.prop.mar), y = cat.label,
  #                   group = cat.label,
  #                   fill = cat.label, fill_ramp = after_stat(level)),
  #               .width = c(0.5, 0.9, 1),
  #               alpha = 0.8,
  #               height = 1,
  #               scale = 0.8,
  #               n = 1001,
  #               # normalize = "groups",
  #               point_interval = median_hdci) +
  #     stat_spike(aes(x = as.numeric(area.prop.mar), y = cat.label,
  #                    linetype = after_stat(at)), at = c(Median = "median") ,
  #                height = comp.av.sh[region],
  #                size = 0.8,
  #                linewidth = 0.3,
  #                # height = 0.8,
  #                normalize = "all"
  #                ) +
  #     geom_hline(yintercept = 1, linewidth = 0.1) +
  #     geom_hline(yintercept = 2, linewidth = 0.1) +
  #     geom_vline(xintercept = 0, linewidth = 0.2, linetype = "dashed") +
  #     scale_fill_manual(values = col.cat, guide = "none") +
  #     scale_fill_ramp_discrete(breaks = c(0.5, 0.9)) +
  #     scale_y_discrete(limits = rev) +
  #     scale_x_continuous(labels = scales::label_number(accuracy = 1e2)) +
  #     guides(linetype = guide_legend(order = 1)) +
  #     coord_cartesian(xlim =  comp.av.xlim[[region]]) +
  #     facet_grid(rows = vars(dist.label)) +
  #     labs(x = mar.title.l, y = "Direct tenure comparison",
  #          fill_ramp = "Uncertainty interval", linetype = "Point estimate") +
  #     post_theme


  plots.area.av[[region]] <-
    ten.sum[[region]]$post[is.na(adm0) & mar_type %in% c("rec")] |>
    ggplot() +
      stat_slab(aes(x = as.numeric(area.prop.mar), y = cat.label,
                    group = cat.label,
                    fill = cat.label,
                    fill_ramp = after_stat(level)),
                # fill = "#854B8F",
                .width = c(0.5, 0.9, 1),
                alpha = 0.8,
                height = 1,
                scale = 0.65,
                n = 1001,
                # normalize = "panels",
                point_interval = median_hdci) +
      stat_spike(aes(x = as.numeric(area.prop.mar), y = cat.label,
                     linetype = after_stat(at)), at = c(Median = "median") ,
                 height = comp.av.sh[region],
                 size = 0.3,
                 linewidth = 0.15,
                 # height = 0.8,
                 # normalize = "all"
                 ) +
      geom_hline(yintercept = 1, linewidth = 0.1) +
      # geom_hline(yintercept = 2, linewidth = 0.1) +
      geom_vline(xintercept = 0, linewidth = 0.2, linetype = "dashed") +
      scale_fill_manual(values = col.cat, guide = "none") +
      scale_fill_ramp_discrete(breaks = c(0.5, 0.9)) +
      # scale_y_discrete(limits = rev) +
      scale_x_continuous(labels = scales::label_number(accuracy = 1e2)) +
      guides(linetype = guide_legend(order = 1)) +
      coord_cartesian(xlim =  comp.av.xlim[[region]]) +
      facet_grid(rows = vars(dist.label), cols = vars (study.label), scales = "free_x") +
      labs(x = mar.title.l, y = "IL, recognized vs. IL, not recognized\n(incl. mixed regimes)",
           fill_ramp = "Uncertainty interval", linetype = "Point estimate") +
      post_theme +
      theme(axis.text.y = element_blank(),
            strip.text.y = element_text(size = rel(0.6),
                                        margin = margin(
                                                        base.size/2,
                                                        base.size/2,
                                                        base.size/2,
                                                        base.size/2
                                                        )),
            strip.text.x = element_text(size = rel(0.7),
                                        margin = margin(
                                                        base.size/2,
                                                        base.size/2,
                                                        base.size/2,
                                                        base.size/2
                                                        )))

  ten.sum.av.l[[region]] <-
    ten.sum[[region]]$mar |>
         melt(measure.vars = list(est.median = c("area.fac.median", "area.cf.median", "area.mar.median"),
                                  # est.q5 = c("area.fac.q5", "area.cf.q5", "area.mar.q5"),
                                  # est.q95 = c("area.fac.q95", "area.cf.q95", "area.mar.q95")),
                                  est.hdi90l = c("area.fac.hdi90l", "area.cf.hdi90l", "area.mar.hdi90l"),
                                  est.hdi90u = c("area.fac.hdi90u", "area.cf.hdi90u", "area.mar.hdi90u")),
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
  # col.to.num <- paste0("est.", c("median", "q5", "q95"))
  col.to.num <- paste0("est.", c("median", "hdi90l", "hdi90u"))
  ten.sum.av.l[[region]][, (col.to.num) := lapply(.SD, as.numeric), .SDcols = col.to.num]


}


## AVOIDED DISTURBANCES (PROPORTION) ###################################



ten.sum.l <- list()
ten.post.l <- list()

plots.prop.av <- list()

for(i in seq_along(regions)) {

  region <- regions[i]

  message(paste0("Preparing plots for region `", region, "` …"))

  message("Tenure category by country …")

  reg.title <- switch(region,
                      cam = "Central America",
                      amz = "Amazon")

  comp.prop.xlim <- list(amz = c(-0.035, 0.005), cam = c(-0.11, 0.03))
  comp.prop.sh <- c(amz = 0.75, cam = 0.875)

  # plots.prop.av[[region]] <-
  #   ten.sum[[region]]$post[
  #                          # is.na(it_type) & is.na(pa_type) &
  #                          is.na(adm0) &
  #                          # mar_type %in% c("rec", "ind")] |>
  #                          mar_type %in% c("rec", "recov", "ind", "indov")] |>
  #   ggplot() +
  #     stat_slab(aes(x = as.numeric(marginal), y = cat.label,
  #                   fill = cat.label, fill_ramp = after_stat(level)),
  #               .width = c(0.5, 0.9, 1),
  #               alpha = 0.8,
  #               height = 1,
  #               scale = 0.8,
  #               n = 1001,
  #               normalize = "all",
  #               point_interval = ggdist::median_hdci) +
  #     stat_spike(aes(x = as.numeric(marginal), y = cat.label,
  #                    linetype = after_stat(at)), at = c(Median = "median") ,
  #                height = comp.av.sh[region],
  #                size = 0.8,
  #                linewidth = 0.3,
  #                # height = 0.8,
  #                # normalize = "all"
  #                ) +
  #     geom_hline(yintercept = 1, linewidth = 0.1) +
  #     geom_hline(yintercept = 2, linewidth = 0.1) +
  #     geom_vline(xintercept = 0, linewidth = 0.2, linetype = "dashed") +
  #     # scale_fill_manual(values = col.cat, guide = "none") +
  #     scale_fill_ramp_discrete(breaks = c(0.5, 0.9)) +
  #     scale_y_discrete(limits = rev) +
  #     scale_x_continuous(labels = scales::label_number(accuracy = 1, scale = 100,
  #                                                      style_positive = "plus",
  #                                                      suffix = " pp.")) +
  #     guides(linetype = guide_legend(order = 1)) +
  #     coord_cartesian(xlim =  comp.prop.xlim[[region]]) +
  #     facet_grid(rows = vars(dist.label)) +
  #     labs(x = mar.title.l, y = "Direct tenure comparison",
  #          fill_ramp = "Credible interval", linetype = "Point estimate") +
  #     post_theme


  ten.sum.l[[region]] <-
    ten.sum[[region]]$mar |>
         melt(measure.vars = list(est.median = c("fac.median", "cf.median", "mar.median"),
                                  est.hdi90l = c("fac.hdi90l", "cf.hdi90l", "mar.hdi90l"),
                                  est.hdi90u = c("fac.hdi90u", "cf.hdi90u", "mar.hdi90u")),
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


  ten.post.l[[region]] <-
    ten.sum[[region]]$post |>
         melt(measure.vars = c("factual", "counterfactual", "marginal"),
             variable.name = "est_type",
             value.name = "estimate")
  ten.post.l[[region]][, est.label := est.lab[as.integer(est_type)]]
  ten.post.l[[region]][, est.group := factor(fifelse(est_type == "3",
                                                    "Attributed effect",
                                                    "Comparison against reference"),
                                            levels = c("Comparison against reference",
                                                       "Attributed effect"))]

  reg.lev <- levels(ten.post.l[[region]]$reg.label)
  reg.lev.new <- c(reg.title, reg.lev[1:(length(reg.lev)-1)])
  ten.post.l[[region]][reg.label == "Region", reg.label := reg.title]
  ten.post.l[[region]][, reg.label := factor(reg.label, levels = reg.lev.new)]

}




## CONDENSED VERSION OF TENURE EFFECTS (PROPORTIONAL AND AREA)

area.c <-
  rbind(ten.sum.av.l$amz[
                         # is.na(it_type) & is.na(pa_type) &
                         is.na(adm0) &
                         mar_type %in% c("rec", "recov", "ind", "indov")
                         ][order(-est.label)],
        ten.sum.av.l$cam[
                         # is.na(it_type) & is.na(pa_type) &
                         is.na(adm0) &
                         mar_type %in% c("rec", "recov", "ind", "indov")
                         ][order(-est.label)])
area.c[, dist.label2 := dist.lab.2l[as.character(dist.label)]]
setorder(area.c, -est_type)

prop.c <-
  rbind(ten.sum.l$amz[
                      # is.na(it_type) & is.na(pa_type) &
                      is.na(adm0) &
                      mar_type %in% c("rec", "recov", "ind", "indov")
                      ][order(-est.label)],
        ten.sum.l$cam[
                      # is.na(it_type) & is.na(pa_type) &
                      is.na(adm0) &
                      mar_type %in% c("rec", "recov", "ind", "indov")
                      ][order(-est.label)])
prop.c[, dist.label2 := dist.lab.2l[as.character(dist.label)]]
setorder(prop.c, -est_type)

p.area.mar.c <-
  area.c[est_type %in% c("3")] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = cat.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 1),
                       expand = expansion(mult = c(0.2, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    expand_limits(y = 0) +
    facet_grid(rows = vars(reg.label), cols = vars(dist.label2), scales = "free_y") +
    labs(colour = "", fill = "", y = mar.title.def.av.2l, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))


p.area.comp.c <-
  area.c[est_type %in% c("1", "2")] |>
  ggplot() +
    geom_pointrange(aes(x = cat.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 1),
                       expand = expansion(mult = c(0.2, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    expand_limits(y = 0) +
    facet_grid(rows = vars(reg.label), cols = vars(dist.label2), scales = "free_y") +
    labs(colour = "", fill = "", y = abs.area.title.def, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))

p.area.mar.c <-
  area.c[est_type %in% c("3")] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = cat.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 1),
                       expand = expansion(mult = c(0.2, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    expand_limits(y = 0) +
    facet_grid(rows = vars(reg.label), cols = vars(dist.label2), scales = "free_y") +
    labs(colour = "", fill = "", y = mar.title.def.av.2l, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))


# ten.sum$cam$post[dist_type == "deg" & mar_type == "ind" & is.na(adm0) & group.id == 1,
#                  hdi(as.numeric(area.prop.mar), .width = 0.9)]
# ten.sum$cam$post[dist_type == "deg" & mar_type == "ind" & is.na(adm0) & group.id == 1,
#                  quantile(as.numeric(area.prop.mar), c(0.05, 0.95))]

# ten.sum$amz$post[dist_type == "def" & mar_type == "rec" & is.na(adm0) & group.id == 1,
#                  hdci(as.numeric(area.prop.mar), .width = 0.9)]
# ten.sum$amz$post[dist_type == "def" & mar_type == "rec" & is.na(adm0) & group.id == 1,
#                  quantile(as.numeric(area.prop.mar), c(0.025, 0.975))]



p.prop.comp.c <-
  prop.c[est_type %in% c("1", "2")] |>
  ggplot() +
    geom_pointrange(aes(x = cat.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
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
    expand_limits(y = 0) +
    facet_grid(rows = vars(reg.label), cols = vars(dist.label2), scales = "free_y") +
    labs(colour = "", fill = "", y = abs.title.def, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))


p.prop.mar.c <-
  prop.c[est_type %in% c("3")] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = cat.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 1, scale = 100,
                                                    style_positive = "plus", suffix = " pp."),
                       expand = expansion(mult = c(0.2, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    expand_limits(y = 0) +
    facet_grid(rows = vars(reg.label), cols = vars(dist.label2), scales = "free_y") +
    labs(colour = "", fill = "", y = mar.title.def.2l, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = ten.cat.angle, hjust = 1))




# ten.sum$amz$post[mar_type %in% c("rec", "ind") & dist_type == "def" &is.na(it_type) & is.na(pa_type) & is.na(adm0),
#                  .(mar_type, .draw, area.prop.mar)] |>
# dcast(.draw ~ mar_type) |>
# _[, sum(as.numeric(rec) < -1e4)/.N]

# plots.area.av$amz + plots.area.av$cam + plot_layout(guides = "collect")

p.ten.comp <-
(plots.area.av$amz &
  theme(
        plot.margin = unit(c(0, base.size, 0, 0), "pt"),
        plot.title.position = "plot",
        strip.text.y = element_blank())) +
(plots.area.av$cam &
  theme(
        # plot.margin = unit(c(15, 5, 0, 10), "pt"),
        plot.title.position = "plot")) +
plot_layout(nrow = 1, guides = "collect", axes = "collect") &
theme(
      axis.title = element_text(size = rel(0.7)),
      axis.title.y = element_text(margin = margin(r = 0.5*base.size), size = rel(0.9)),
      axis.title.x = element_text(size = rel(0.9)),
      # axis.text.y = element_text(size = rel(0.7), margin = margin(r = base.size/4)),
      axis.text.x = element_text(size = rel(0.8)),
      legend.text = element_text(size = rel(0.7)),
      legend.key.size = unit(base.size, "pt"),
      # legend.spacing.y = unit(0, "pt"),
      legend.title = element_text(size = rel(0.7)),
      legend.margin = margin(0, 0, 0, 0),
      plot.tag = element_text(size = rel(1),
                              family = "IBMPlexSansCondensed",
                              face = "bold",
                              margin = margin(
                                              0,
                                              base.size/2,
                                              base.size,
                                              0
                                              )))


p.area.prop <-
(
 # ((p.area.comp.c &
 #   theme(
 #        plot.margin = unit(c(10, 5, 0, 10), "pt")/2,
 #         )) +
 #  (p.area.mar.c &
 #   theme(
 #        plot.margin = unit(c(10, 5, 0, 25), "pt")/2,
 #         ))) /
 ((p.prop.comp.c &
   theme(
        plot.margin = unit(c(10, 10, 0, 10), "pt")/2,
         )) +
  (p.prop.mar.c &
   theme(
        plot.margin = unit(c(10, 10, 0, 10), "pt")/2,
         )))) +
# plot_layout(guides = "collect", tag_level = "new") +
# plot_annotation(tag_levels = "A") &
plot_layout(guides = "collect") &
theme(
      axis.title = element_text(size = rel(0.7)),
      axis.title.y = element_text(margin = margin(r = 0.5*base.size), size = rel(0.9)),
      axis.text.y = element_text(size = rel(0.7),
                                 margin = margin(r = base.size/4),
                                 hjust = 1),
      axis.text.x = element_text(size = rel(0.8)),
      legend.text = element_text(size = rel(0.7)),
      legend.key.size = unit(base.size, "pt"),
      legend.spacing.y = unit(0, "pt"),
      legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      strip.text.x = element_text(size = rel(0.6),
                                margin = margin(
                                                base.size/2,
                                                base.size/2,
                                                base.size/2,
                                                base.size/2
                                                )),
      strip.text.y = element_text(size = rel(0.7),
                                margin = margin(
                                                base.size/2,
                                                base.size/2,
                                                base.size/2,
                                                base.size/2
                                                )),
      plot.tag = element_text(size = rel(1),
                              family = "IBMPlexSansCondensed",
                              face = "bold",
                              margin = margin(
                                              0,
                                              base.size/2,
                                              base.size,
                                              0
                                              ))
      )



p.comb <-
  ((p.ten.comp +
   theme(plot.margin = margin(b = base.size))
   ) /
  p.area.prop) +
plot_layout(heights = c(1, 1)) +
plot_annotation(tag_levels = list(c("A", "", "B", "C")))

png(file.fig.ten.comp.c, width = 5.5, height = 5.65, unit = "in", res = 600)
p.comb
dev.off()

tab.ten <-
  list(amz = ten.sum$amz$mar[, -c("ci_0")],
       cam = ten.sum$cam$mar[, -c("ci_0")]) |>
  rbindlist(idcol = "study_region")
setorder(tab.ten, study_region, dist_type, adm0, it_type, pa_type)
fwrite(tab.ten, file.table.ten.comp)


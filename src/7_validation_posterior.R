args <- commandArgs(trailingOnly = TRUE)
library(data.table)
library(stringi)
library(ggplot2)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)

source("utilities.R")


region <- tolower(as.character(args[1]))
model_resp <- tolower(as.character(args[2]))

# region <- "amz"
# model_resp <- "def"


path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.gam <- paste0(path.base, "models/gam/")
path.agg <- paste0(path.gam, "agg/", region, "/")
path.mar <- paste0(path.base, "models/marginal/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

# file.model <- paste0(path.gam, region, ".m1.", model_resp, ".rds")
# file.post <- paste0(path.gam, region, ".m1.", model_resp, ".post.rds")
file.agg.all <- paste0(path.agg, region, ".", model_resp, ".all.rds")
file.agg.noitpa <- paste0(path.agg, region, ".", model_resp, ".noitpa.rds")
file.mar.itpa <- paste0(path.mar, region, ".", model_resp, ".ten.itpa.all.rds")
file.res <- paste0(path.gam, region, ".m1.", model_resp, ".res.rds")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")

file.fig.ppc.adm <- paste0(path.figures, "ppc.adm.", model_resp, ".", region, ".png")
file.fig.ppc.itpa <- paste0(path.figures, "ppc.itpa.", model_resp, ".", region, ".png")


p.title <-
  paste0(switch(region, amz = "Amazon, ", cam = "Central America, "),
         switch(model_resp, def = "long-term disturbance", deg = "short-term disturbance"))

base.size <- 7 
# base.size <- 10
ppc_theme <-
  theme_light(base_family = "IBMPlexSansCondensed",
              base_size = base.size) +
  theme(
        axis.line.x = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.line.y = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.title.x = element_text(size = rel(0.9),
                                    lineheight = rel(1.15),
                                    margin = margin(t = base.size/2)),
        axis.title.y = element_text(size = rel(0.9),
                                    margin = margin(r = base.size/2)),
        axis.text.x = element_text(color = "black",
                                   size = rel(0.9),
                                   margin = margin(t = base.size/2)),
        axis.text.y = element_text(color = "black",
                                   size = rel(0.9),
                                   lineheight = rel(1.15),
                                   margin = margin(r = base.size/2),
                                   hjust = 0),
        axis.ticks = element_line(colour = "grey30"),
        legend.title = element_text(size = rel(1),
                                    margin = margin(b = base.size/2)),
        legend.position = "right",
        legend.justification = "center",
        legend.key.size = unit(base.size*1.5, "pt"),
        legend.text = element_text(size = rel(1), margin = margin(l = base.size/3,
                                                                  t = base.size/2,
                                                                  b = base.size/2)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing.x = unit(base.size*2, "pt"),
        panel.spacing.y = unit(base.size, "pt"),
        plot.margin = margin(3, base.size, 3, base.size),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0,
                                  # face = "bold",
                                  face = "plain",
                                  size = rel(1.5),
                                  margin = margin(l = 0, b = base.size*2, t = base.size/3)),
        plot.subtitle = element_text(size = rel(1),
                                     margin = margin(b = base.size)),
        plot.tag = element_text(face = "bold"),
        strip.text = element_text(size = rel(0.8),
                                  lineheight = rel(1),
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


cat.lab <- 
  data.table(itpa.label = c(
                           "IL, recognized", "IL, not recognized",
                           "PA, category I-IV", "PA, category V-VI",
                           "IL, rec.; PA, cat. I-IV",
                           "IL, rec.; PA, cat. V-VI",
                           "IL, not rec.; PA, cat. I-IV",
                           "IL, not rec.; PA, cat. V-VI",
                           "Reference"
                           ),
             it_type = c(
                         "recognized", "not_recognized",
                         "none", "none",
                         "recognized", "recognized",
                         "not_recognized", "not_recognized",
                         "none"
                         ),
             pa_type = c(
                         "none", "none",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "none"))
cat.lab[, itpa.label := factor(itpa.label, levels = itpa.label)]


reg.lab <- list()
reg.lab$amz <-
  data.table(adm0 = c("BOL", "BRA", "COL", "ECU",
                      "GUF", "GUY", "PER", "SUR",
                      "VEN", NA),
             reg.label = c("Bolivia", "Brazil", "Colombia", "Ecuador",
                           "French Guiana", "Guyana", "Peru", "Suriname",
                           "Venezuela", 
                           "Amazon")
             )
reg.lab$cam <-
  data.table(adm0 = c("BLZ", "CRI", "GTM", "HND",
                      "MEX", "NIC", "PAN", "SLV",
                      NA),
             reg.label = c("Belize", "Costa Rica", "Guatemala", "Honduras",
                           "Mexico", "Nicaragua", "Panama", "El Salvador",
                           "Central America")
             )
reg.lab$amz[, reg.label := factor(reg.label, levels = reg.label)]
reg.lab$cam[, reg.label := factor(reg.label, levels = reg.label)]



var.resp <-
  switch(model_resp,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

data <- readRDS(file.data)
adm.all <- readRDS(file.agg.all)
mar.itpa <- readRDS(file.mar.itpa)
adm.noitpa <-
  readRDS(file.agg.noitpa) |>
  _[,
    .(.draw,
      adm0,
      it_type = factor("none", levels = levels(mar.itpa$it_type), ordered = TRUE),
      pa_type = factor("none", levels = levels(mar.itpa$pa_type), ordered = TRUE),
      var.col),
    env = list(var.col = var.resp)]


## POSTERIER PREDICTIVE CHECKS #########################################

adm.resp.obs <-
  rbind(data[,
             .(var.col = mean(var.col)),
             env = list(var.col = var.resp)],
        data[,
             .(var.col = mean(var.col)),
             by = .(adm0),
             env = list(var.col = var.resp)],
        fill = TRUE) |>
  merge(reg.lab[[region]], by = "adm0")

p.ppc.adm <-
  merge(adm.all, reg.lab[[region]], by = "adm0") |>
  ggplot() +
    geom_histogram(aes(x = !!sym(var.resp)),
                   bins = 100,
                   colour = "#5f98c6") +
    geom_vline(data = adm.resp.obs, aes(xintercept = !!sym(var.resp)),
               colour = "#e41a1c", linewidth = 1, alpha = 0.5) +
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.1), add = 0.003)) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.35))) +
    facet_wrap(vars(reg.label), ncol = 4, scales = "free") +
    labs(x = "Disturbance probability") +
    ppc_theme +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

png(file.fig.ppc.adm, width = 7, height = 4, unit = "in", res = 600)
p.ppc.adm + plot_annotation(subtitle = p.title, theme = ppc_theme)
dev.off()



itpa.resp.obs <-
  data[,
       .(var.col = mean(var.col)),
       by = .(it_type, pa_type),
       env = list(var.col = var.resp)] |>
  merge(cat.lab)

itpa.all <-
  mar.itpa[is.na(adm0) & !is.na(it_type) & !is.na(pa_type), 
           .(.draw,
             adm0,
             it_type,
             pa_type,
             var.col = factual),
           env = list(var.col = var.resp)] |>
  rbind(adm.noitpa[is.na(adm0)]) |>
  merge(cat.lab, by = c("it_type", "pa_type"))

p.ppc.itpa <-

  itpa.all |>
  ggplot() +
    geom_histogram(aes(x = !!sym(var.resp)),
                   bins = 50,
                   # binwidth = 0.00005,
                   fill = "#5f98c6",
                   colour = NA) +
    # geom_density(aes(x = !!sym(var.resp)),
    #              fill = "#5f98c6",
    #              colour = NA,
    #              bw = "sj-dpi") +
    geom_vline(data = itpa.resp.obs, aes(xintercept = !!sym(var.resp)),
               colour = "#e41a1c", linewidth = 1, alpha = 0.5) +
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.1), add = 0.003)) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.35))) +
    facet_wrap(vars(itpa.label),
               ncol = 4, scales = "free") +
    labs(x = "Disturbance probability") +
    ppc_theme +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

png(file.fig.ppc.itpa, width = 7, height = 4, unit = "in", res = 600)
p.ppc.itpa + plot_annotation(subtitle = p.title, theme = ppc_theme)
dev.off()


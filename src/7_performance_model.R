args <- commandArgs(trailingOnly = TRUE)
library(data.table)
library(stringi)
library(ggplot2)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)
library(pROC)

source("utilities.R")


region <- tolower(as.character(args[1]))
model_resp <- tolower(as.character(args[2]))

# region <- "amz"
# model_resp <- "def"

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.gam <- paste0(path.base, "models/gam/")
path.data.vis <- paste0(path.data, "visualization/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

file.res <- paste0(path.gam, region, ".m1.", model_resp, ".res.rds")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")

file.fig.auc <- paste0(path.figures, "auc.", model_resp, ".", region, ".png")


p.title <-
  paste0(switch(region, amz = "Amazon, ", cam = "Central America, "),
         switch(model_resp, def = "long-term disturbance", deg = "short-term disturbance"))

base.size <- 7 
# base.size <- 10
res_theme <-
  theme_light(base_family = "IBMPlexSans",
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
        # panel.spacing.x = unit(base.size*2, "pt"),
        panel.spacing.y = unit(base.size, "pt"),
        plot.margin = margin(3, 3, 3, 3),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0,
                                  # face = "bold",
                                  face = "plain",
                                  size = rel(1.5),
                                  margin = margin(l = 0, b = base.size*2, t = base.size/3)),
        plot.subtitle = element_text(size = rel(1),
                                     margin = margin(b = 14)),
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


# Load model, posterior draws, and data
# gam <- readRDS(file.model)
# post <- readRDS(file.post)
res <- readRDS(file.res)
data <- readRDS(file.data)

var.resp <-
  switch(model_resp,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

data.res <- merge(data, res)


## AUC based on posterior predictive medians ##########################

mod.resp <- data.res[,as.numeric(var.col), env = list(var.col = var.resp)]
mod.pred <- data.res[,fitted]
mod.roc <- roc(response = mod.resp, predictor = mod.pred)

auc.lab <- paste0("AUC ", format(round(auc(mod.roc), 2), nsmall = 2))

p.roc <-
  with(mod.roc, data.frame(spec = specificities, sens = sensitivities)) |>
  ggplot() +
    geom_abline(slope = 1, intercept = 1, linewidth = 0.3, colour = "grey35") +
    geom_line(aes(x = spec, y = sens)) +
    geom_text(label = auc.lab, x = 0, y = 0.1, hjust = 1,
              family = "IBMPlexSans", fontface = "plain",
              size = base.size, size.unit = "pt") +
    coord_fixed() +
    scale_x_reverse() +
    labs(x = "Specificity", y = "Sensitivity",
         subtitle = p.title) +
    res_theme

png(file.fig.auc, width = 2.5, height = 2.5, unit = "in", res = 600)
p.roc
dev.off()


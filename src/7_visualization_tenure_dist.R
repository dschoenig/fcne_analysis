library(data.table)
library(posterior)
library(sf)
library(ggplot2)
library(ggdist)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)
library(KernSmooth)

source("utilities.R")

# path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
path.effects <- paste0(path.base, "models/gam/effects/")
path.figures <- paste0(path.base, "results/figures/")

file.data.vis <- paste0(path.data.vis, "tenure_adm_dist.rds")

regions <- c("amz", "cam")


## COLOURS AND LABELS ##########################################################

# Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.plot <- col.div[c(3,17)]


## EFFECT OF TENURE BY ADMINISTRATIVE AREA, FOREST TYPE ########################

ten.sum <- list()

for(i in seq_along(regions)) {
  
  region <- regions[i]

  message(paste0("Preparing data for region `", region, "` …"))
  
  file.riskchange.tenure_areas <- paste0(path.effects, region, ".riskchange.tenure_areas.rds")
  
  # Distribution of areas per tenure category

  message("Distribution of areas per tenure category …")
  
  rc.ten_areas <- readRDS(file.riskchange.tenure_areas)

  types <- c("it_type == 'recognized'",
             "it_type == 'not_recognized'",
             "pa_type == 'direct_use'",
             "pa_type == 'indirect_use'")
  names(types) <- c("it.rec", "it.notrec", "pa.ind", "pa.dir")

  densities <- list()


  for(j in seq_along(types)) {
    selg <- rc.ten_areas$groups[eval(parse(text = types[j])), area.label]

    x <- seq(-1, 1, by = 0.001)

    dens <- apply(rc.ten_areas$arc[,selg], 1,
                  \(draw) density(draw, from = min(x), to = max(x),
                                  # n = length(x), bw = "nrd")$y)
                                  n = length(x), bw = "SJ")$y)
                  # \(draw) KernSmooth::bkde(draw, gridsize = 2001,
                  #                    range.x = c(-1, 1))$y)
                  # \(draw) density_SJ(draw, gridsize = 2001,
                  #                    range.x = c(-1, 1))$y)
    row.names(dens) <- 1:nrow(dens)

    dens <- as_draws_matrix(t(dens))
    dens.sum <-
      summary(dens, mean, \(x) quantile2(x, c(0.025, 0.975))) |>
      as.data.table() |>
      setnames("variable", "point")
    dens.sum[, point := as.integer(as.character(point))]

    densities[[j]] <- 
      merge(dens.sum,
            data.table(point = 1:length(x),
                       arc = x),
            by = "point")
  }
  names(densities) <- names(types)
  
  ten.sum[[region]]$arc.dens <- rbindlist(densities, idcol = "type")
}


combined.ten.arc.dens <- 
  list(amz = ten.sum$amz$arc.dens,
       cam = ten.sum$cam$arc.dens) |>
  rbindlist(idcol = "region") |>
  ggplot(aes(x = arc, y = mean, ymin = q2.5, ymax = q97.5)) +
  geom_ribbon(aes(fill = type), alpha = 0.2, show.legend = FALSE) +
  geom_line(aes(col = type, linetype = type)) +
  scale_colour_manual(aesthetics = c("colour", "fill"),
                      values = c.plot[c(1, 1, 2, 2)],
                      breaks = c("it.rec", "it.notrec", "pa.ind", "pa.dir"),
                      labels = c("IT, recognized", "IT, not recognized",
                                 "PA, indirect use", "PA, direct use")) +
  scale_linetype_manual(values = c(it.rec = "solid", 
                                   it.notrec = "dashed",
                                   pa.ind = "solid",
                                   pa.dir = "dashed"),
                        labels = c("IT, recognized", "IT, not recognized",
                                   "PA, indirect use", "PA, direct use")) +
  geom_vline(xintercept = 0, colour = "grey50") +
  # scale_y_continuous(limits = c(0, max(densities$q97.5))
  #                    # , breaks = 0, labels = NULL) +
  #                    ) +
  scale_x_continuous(limits = c(-0.25, 0.4), labels = label_arc) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 15, 5)) +
  labs(y = "Distribution of\nindividual areas (density)\n",
       x = "\nAbsolute change in forest loss risk",
       fill = NULL, colour = NULL, linetype = NULL) +
  theme_minimal(base_family = "IBMPlexSans") +
  theme(
        legend.position = "right",
        legend.justification = c(0,1),
        # legend.spacing.y = unit(5, "mm"),
        axis.line = element_line(colour = "grey5"),
        axis.ticks = element_line(colour = "grey5"),
        axis.title.y = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        panel.spacing.x = unit(10, "mm"),
        strip.text = element_text(hjust = 0, face = "bold", size = rel(1))
        ) +
  facet_grid(cols = vars(region),
             labeller = labeller(region = c(amz = "Amazon",
                                            cam = "Central America")))

# tiff(paste0(path.figures, "fig2.tif"), width = 8.25, height = 4, unit = "in", res = 300)
svg(paste0(path.figures, "ten.arc.dens.svg"), width = 8.25, height = 3.5)
combined.ten.arc.dens
dev.off()


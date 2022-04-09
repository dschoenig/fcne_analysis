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

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
path.effects <- paste0(path.base, "models/gam/effects/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.tables <- paste0(path.base, "results/tables/")
if(!dir.exists(path.tables)) dir.create(path.tables, recursive = TRUE)
path.geo <- paste0(path.base, "results/geo/")
if(!dir.exists(path.geo)) dir.create(path.geo, recursive = TRUE)

file.data.vis <- paste0(path.data.vis, "tenure_adm_dist.rds")

regions <- c("amz", "cam")


## COLOURS AND LABELS ##########################################################

# Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.plot <- col.div[c(3,17)]

dens_theme <-  
  theme_minimal(base_family = "IBMPlexSans", base_size = 7) +
  theme(
        panel.grid = element_blank(),
        panel.spacing.x = unit(10, "mm"),
        legend.position = "right",
        legend.justification = c(0,1),
        legend.title = element_text(size = rel(0.9), hjust = 0,
                                    margin = margin(t = 3, b = 6)),
        legend.text = element_text(size = rel(0.8)),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(12, "pt"),
        axis.line = element_line(colour = "grey5"),
        axis.ticks = element_line(colour = "grey5"),
        strip.text = element_text(size = rel(1), hjust = 0,
                                  margin = margin(b = 3, t = 6))
        )

cat.lab <- data.table(type = c("it.rec", "it.notrec",
                              "pa.ind", "pa.dir"),
                      label = c("IT, recognized", "IT, not recognized",
                                "PA, indirect use", "PA, direct use"))


ranges <- 
  data.table(interval.label = c("[-100, 0)",
                                "(0, +100]",
                                "[-100, -20)",
                                "[-20, -10)",
                                "[-10, -5)",
                                "[-5, 0)",
                                "(0, +5]",
                                "(+5, +10]",
                                "(+10, +20]",
                                "(+20, 100]"),
             expr = c(
                      "arc >= -1 & arc < 0",
                      "arc > 0 & arc <= 1",
                      "arc >= -1 & arc < -0.2",
                      "arc >= -0.2 & arc < -0.1",
                      "arc >= -0.1 & arc < -0.05",
                      "arc >= -0.05 & arc < 0",
                      "arc > 0 & arc <= 0.05",
                      "arc > 0.05 & arc <= 0.1",
                      "arc > 0.1 & arc <= 0.2",
                      "arc > 0.2 & arc <= 1") 
        )
ranges[, interval.label := factor(interval.label, levels = interval.label)]


## EFFECT OF TENURE BY ADMINISTRATIVE AREA, FOREST TYPE ########################


if(!file.exists(file.data.vis)) {

  ten.sum <- list()

  for(i in seq_along(regions)) {
    
    region <- regions[i]

    message(paste0("Preparing data for region `", region, "` …"))
    
    file.riskchange.tenure_areas <- paste0(path.effects, region, ".riskchange.tenure_areas.rds")
    file.areas.it.geo <- paste0(path.data.raw, region, ".indterr.gpkg")
    file.areas.pa.geo <- paste0(path.data.raw, region, ".pareas.gpkg")

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


    # Proportion of areas by range
   
    arc.prop <- list()
    for(j in 1:nrow(ranges)) {
      arc.prop[[j]] <- 
        ten.sum[[region]]$arc.dens[eval(parse(text = ranges$expr[j])),
                                   lapply(.SD, \(x) sum(x * 0.001)),
                                   by = type,
                                   .SDcols = c("mean", "q2.5", "q97.5")]
    }
    names(arc.prop) <- ranges$interval.label
    
    arc.prop <- rbindlist(arc.prop, idcol = "interval.label")
    arc.prop[, interval.label := factor(interval.label,
                                      levels = levels(ranges$interval.label))]
    arc.prop <- 
      merge(arc.prop, cat.lab, sort = FALSE) |>
      setnames("label", "type.label") |>
      setcolorder(c("interval.label", "type", "type.label"))
    arc.prop[, type.label := factor(type.label, levels  = cat.lab$label)]

    ten.sum[[region]]$arc.prop <- arc.prop[order(interval.label, type.label)]

    
    message("Absolute risk change by area …")
  
    areas.it <-
      read_sf(file.areas.it.geo, fid_column_name = "it.id") |>
      as.data.table()
    areas.it[, it.id := as.integer(it.id)]
    areas.pa <-
      read_sf(file.areas.pa.geo, fid_column_name = "pa.id") |>
      as.data.table()
    areas.pa[, pa.id := as.integer(pa.id)]

    groups.it <-
      merge(rc.ten_areas$groups[,!"ids"], areas.it,
            by = c("it.id", "adm0"), all = FALSE, sort = FALSE)
    groups.pa <-
          merge(rc.ten_areas$groups[,!"ids"], areas.pa,
                by = c("pa.id", "adm0"), all = FALSE, sort = FALSE)

    arc.it.sum <-
      rc.ten_areas$arc[,groups.it$area.label] |>
      summarize_draws(mean, 
                      sd,
                      \(x) quantile2(x, c(0.025, 0.975)))|>
      as.data.table() |>
      setnames("variable", "area.label") |>
      merge(groups.it,
            by = "area.label", sort = FALSE)
    arc.pa.sum <-
      rc.ten_areas$arc[,groups.pa$area.label] |>
      summarize_draws(mean, 
                      sd,
                      \(x) quantile2(x, c(0.025, 0.975)))|>
      as.data.table() |>
      setnames("variable", "area.label") |>
      merge(groups.pa,
            by = "area.label", sort = FALSE)

    ten.sum[[region]]$arc.it <-
      arc.it.sum[,.(area.id, area.label, it.id, adm0, it_type, name, landmark_cat,
                    geom, mean, sd, q2.5, q97.5)]
    ten.sum[[region]]$arc.pa <-
      arc.pa.sum[,.(area.id, area.label, pa.id, adm0, pa_type, name, iucn_cat,
                    geom, mean, sd, q2.5, q97.5)]

  }

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(ten.sum = ten.sum) |>
  saveRDS(file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

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
                      breaks = cat.lab$type,
                      labels = cat.lab$label) +
  scale_linetype_manual(values = c("solid", "dashed",
                                   "solid", "dashed"),
                        breaks = cat.lab$type,
                        labels = cat.lab$label) +
  geom_vline(xintercept = 0, colour = "grey50") +
  scale_x_continuous(limits = c(-0.25, 0.4), labels = label_arc) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 15, 5)) +
  labs(y = "Distribution of\nindividual areas (density)\n",
       x = "\nAbsolute change in forest loss risk",
       fill = NULL, colour = NULL, linetype = NULL) +
  dens_theme +
  facet_grid(cols = vars(region),
             labeller = labeller(region = c(amz = "Amazon",
                                            cam = "Central America")))

# tiff(paste0(path.figures, "fig2.tif"), width = 8.25, height = 4, unit = "in", res = 300)
svg(paste0(path.figures, "ten.arc.dens.svg"), width = 6.7, height = 2)
combined.ten.arc.dens
dev.off()


ten.arc.prop.t <- list()
for(i in seq_along(regions)) {
  region <- regions[i]
  ten.arc.prop.t[[region]] <-
    ten.sum[[region]]$arc.prop[order(interval.label),
                            .(interval.label, type.label,
                              Mean = 
                                fifelse(is.na(mean),
                                        "--",
                                        label_per(mean, 2, FALSE)),
                              CI =
                                fifelse(is.na(mean),
                                        "",
                                        paste0(" (", label_per(q2.5, 2, FALSE),
                                               "; ", label_per(q97.5, 2, FALSE),
                                               ")")))] |>
    melt(measure.vars = c("Mean", "CI"),
         variable.name = "stat",
         value.name = "arc") |>
    dcast(interval.label + stat ~ type.label, value.var = "arc") |>
    setnames(c("interval.label", "stat"),
             c("Interval", "Estimate"))
  fwrite(ten.arc.prop.t[[region]], paste0(path.tables, region, ".ten.arc.prop.csv"))
}

for(i in seq_along(regions)) {
  region <- regions[i]
  st_as_sf(ten.sum[[region]]$arc.it[it_type == "recognized"]) |>
  write_sf(paste0(path.geo, region, ".arc.gpkg"), layer = "it_type.recognized")
  st_as_sf(ten.sum[[region]]$arc.it[it_type == "not_recognized"]) |>
  write_sf(paste0(path.geo, region, ".arc.gpkg"), layer = "it_type.not_recognized")
  st_as_sf(ten.sum[[region]]$arc.pa[pa_type == "indirect_use"]) |>
  write_sf(paste0(path.geo, region, ".arc.gpkg"), layer = "pa_type.indirect_use")
  st_as_sf(ten.sum[[region]]$arc.pa[pa_type == "direct_use"]) |>
  write_sf(paste0(path.geo, region, ".arc.gpkg"), layer = "pa_type.direct_use")
}

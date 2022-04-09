library(data.table)
library(posterior)
library(sf)
library(ggplot2)
library(ggdist)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)
library(stringi)

source("utilities.R")

# path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.som <- paste0(path.base, "models/som/")
path.effects <- paste0(path.base, "models/gam/effects/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

file.data.vis <- paste0(path.data.vis, "cov.rds")

regions <- c("amz", "cam")


cat.lab <- 
  data.table(cat.label = c("All forests", "Primary forests",
                           "IT, recognized", "IT, not recognized",
                           "PA, indirect use", "PA, direct use"),
             it_type = c(NA, NA,
                         "recognized", "not_recognized",
                         NA, NA),
             pa_type = c(NA, NA,
                         NA, NA,
                         "indirect_use", "direct_use"),
             for_type = c(NA, "primary",
                          NA, NA,
                          NA, NA))
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]

som_theme <-  
  theme_minimal(base_family = "IBMPlexSans",
                base_size = 7) +
  theme(
        plot.margin = margin(9, 9, 9, 9),
        # plot.tag = element_text(margin = margin(b = 3, t = 6),
        panel.background = element_rect(fill = "grey50", colour = NA),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.justification = c(0,1),
        legend.title = element_text(size = rel(0.9), hjust = 0),
        legend.text = element_text(size = rel(0.8)
                                   # , margin = margin(r = 0)
                                   ),
        # legend.spacing.x = unit(5, "mm"),
        legend.spacing.y = unit(5, "mm"),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "grey5"),
        axis.title.x = element_text(margin = margin(t = 6)),
        axis.title.y = element_text(margin = margin(r = 6)),
        strip.text = element_text(size = rel(1), hjust = 0,
                                  margin = margin(b = 3, t = 6))
        )


som_guide_fill <-
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

  cov.scales <-
    list(
         amz =
           list(
                tri = list(title = "Terrain ruggedness index",
                           trans = scales::yj_trans(0),
                           breaks = c(0, 3, 10, 30, 100),
                           limits = c(0, 100),
                           labels = scales::label_comma(big.mark =" ", accuracy = 0.1)),
                dist_set.km = list(
                                   title = "Distance to settlements (km)",
                                   trans = scales::identity_trans(),
                                   breaks = seq(0, 3e2, 1e2),
                                   limits = c(0, 3e2),
                                   labels = scales::label_comma(big.mark =" ")),
                dist_roads.km = list(
                                     title = "Distance to roads (km)",
                                     trans = scales::identity_trans(),
                                     breaks = seq(0, 2e2, 5e1),
                                     limits = c(0, 2e2),
                                     labels = scales::label_comma(big.mark =" ")),
                dist_rivers.km = list(
                                      title = "Distance to rivers (km)",
                                      trans = scales::identity_trans(),
                                      breaks = seq(0, 6, 2),
                                      limits = c(0, 6),
                                      labels = scales::label_comma(big.mark =" ")),
                dens_pop = list(
                                title = "Population density (individuals / km²)",
                                trans = scales::yj_trans(0),
                                breaks = c(0, 10^(1:4)),
                                limits = c(0, 1e4),
                                labels = scales::label_comma(big.mark =" ")),
                dens_roads = list(
                                  title = "Road density (m / km²)",
                                  trans = scales::yj_trans(0),
                                  breaks = c(0, 10^(1:4)),
                                  limits = c(0, 1e4),
                                  labels = scales::label_comma(big.mark =" "))
                ),
         cam =
           list(
                tri = list(title = "Terrain ruggedness index",
                           trans = scales::yj_trans(0),
                           breaks = c(0, 3, 10, 30, 100),
                           limits = c(0, 75),
                           labels = scales::label_comma(big.mark =" ", accuracy = 0.1)),
                dist_set.km = list(
                                   title = "Distance to settlements (km)",
                                   trans = scales::identity_trans(),
                                   breaks = seq(0, 100, 20),
                                   limits = c(0, 80),
                                   labels = scales::label_comma(big.mark =" ")),
                dist_roads.km = list(
                                     title = "Distance to roads (km)",
                                     trans = scales::identity_trans(),
                                     breaks = seq(0, 80, 20),
                                     limits = c(0, 80),
                                     labels = scales::label_comma(big.mark =" ")),
                dist_rivers.km = list(
                                      title = "Distance to rivers (km)",
                                      trans = scales::identity_trans(),
                                      breaks = seq(0, 10, 2.5),
                                      limits = c(0, 10),
                                      labels = scales::label_comma(big.mark =" ")),
                dens_pop = list(
                                title = "Population density (individuals / km²)",
                                trans = scales::yj_trans(0),
                                breaks = c(0, 10^(1:4)),
                                limits = c(0, 1e4),
                                labels = scales::label_comma(big.mark =" ")),
                dens_roads = list(
                                  title = "Road density (m / km²)",
                                  trans = scales::yj_trans(0),
                                  breaks = c(0, 10^(1:5)),
                                  limits = c(0, 3e5),
                                  labels = scales::label_comma(big.mark =" "))
                )
         )


title.wrap <- scales::label_wrap(20)



som.sum <- list()

for(i in seq_along(regions)){

  region <- regions[i]

  message(paste0("Preparing data for region `", region, "` …"))

  file.som.fit <- paste0(path.som, region, ".som.1e6.rds")
  file.data.proc <- paste0(path.data.proc, region, ".data.fit.proc.rds")
  file.risk.ten_cov <- paste0(path.effects, region, ".risk.tenure_cov.rds")
  file.riskchange.ten_cov <- paste0(path.effects, region, ".riskchange.tenure_cov.rds")


  # Forest loss risk

  message("Forest loss risk …")

  r.som <- readRDS(file.risk.ten_cov)

  categories <- 
    data.table(
      name = c("for_type.all",
               "for_type.primary"),
      subset = c('cat == "all" & is.na(for_type) & is.na(it_type) & is.na(pa_type)',
                 'cat == "for_p" & for_type == "primary" & is.na(it_type) & is.na(pa_type)')
    )

  r.sum <- list()

  for(j in 1:nrow(categories)) {
   
    r.cat <- r.som$r[[categories$name[j ]]]
    som.units <- r.som$som.units[eval(parse(text = categories$subset[j]))]
    som.units[, som_bmu := as.character(som_bmu)]

    r.som.dt <-
    as_draws_df(r.cat) |>
    as.data.table() |>
    melt(measure.vars = 1:ncol(r.cat),
         variable.name = "som_bmu",
         value.name = "risk")
    r.som.dt[,
             som_bmu := stri_split_fixed(som_bmu, ":", n = 2, simplify = TRUE)[,1]]

    r.sum[[categories$name[j]]] <-
      r.som.dt[,
           .(mean = mean(risk),
             sd = sd(risk),
             q2.5 = quantile(risk, 0.025),
             q97.5 = quantile(risk, 0.975)),
           by = som_bmu] |>
      merge(som.units[, -c("ids", "cat")], by = "som_bmu") |>
      setorder(som_x, som_y) |>
      setcolorder(c("som_x", "som_y", "som_bmu"))

  }

  som.sum[[region]]$r <- 
  rbindlist(r.sum, idcol = "cat") |>
  merge(cat.lab, by = c("for_type", "it_type", "pa_type"),
        all.x = TRUE, all.y = FALSE, sort = FALSE)

  
  # Absolute change in risk (compared to baseline)

  message("Absolute risk change …")
  
  rc.som <- readRDS(file.riskchange.ten_cov)

  categories <- 
    c("it_type.recognized", "it_type.not_recognized",
      "pa_type.indirect_use", "pa_type.direct_use")

  arc.sum <- list()

  for(j in seq_along(categories)) {
   
    rc.cat <- rc.som[[categories[j]]]
    som.units <- rc.cat$som.units
    som.units[, som_bmu := as.character(som_bmu)]

    rc.som.dt <-
    as_draws_df(rc.cat$arc) |>
    as.data.table() |>
    melt(measure.vars = 1:ncol(rc.cat$arc),
         variable.name = "som_bmu",
         value.name = "arc")
    rc.som.dt[,
             som_bmu := stri_split_fixed(som_bmu, ":", n = 2, simplify = TRUE)[,1]]

    arc.sum[[categories[j]]] <-
      rc.som.dt[,
           .(mean = mean(arc),
             sd = sd(arc),
             q2.5 = quantile(arc, 0.025),
             q97.5 = quantile(arc, 0.975)),
           by = som_bmu] |>
      merge(som.units[, -c("ids", "cat")], by = "som_bmu") |>
      setorder(som_x, som_y) |>
      setcolorder(c("som_x", "som_y", "som_bmu"))
  }

  som.sum[[region]]$arc <- 
  rbindlist(arc.sum, idcol = "cat") |>
  merge(cat.lab, by = c("for_type", "it_type", "pa_type"),
        all.x = TRUE, all.y = FALSE, sort = FALSE)
 
  
  # SOM unit coordinates in full covariate space

  message("Covariates …")

  som.fit <- readRDS(file.som.fit)
  
  bmu.coord <- as.data.table(som.fit$grid$pts)
  setnames(bmu.coord, c("x", "y"), c("som_x", "som_y"))
  bmu.coord[, som_bmu := as.character(1:nrow(bmu.coord))]

  codes <- som.fit$codes[[1]]
  cols <- colnames(codes)
  cols.z <- paste0(colnames(codes), ".z")
  colnames(codes) <- cols.z
  codes <- as.data.table(codes)
  codes[,
        (cols) :=
          as.data.table(t(apply(.SD, 1,
                                \(x) (x * som.fit$scale$sd) + som.fit$scale$mean))),
        .SD = cols.z]
  cov.trans.km <- c("dist_set", "dist_roads", "dist_rivers")
  codes[,
        (paste0(cov.trans.km, ".km")) :=
        lapply(.SD, \(x) x/1000),
        .SDcols = cov.trans.km]
  codes <- cbind(codes, bmu.coord)
  codes[, (cols) := lapply(.SD, \(x) ifelse(x < 0, 0, x)), .SDcols = cols]

  som.sum[[region]]$cov <- codes


  # Number of samples

  message("No. of samples …")

  data.proc <- readRDS(file.data.proc)

  sum.vars <- c("for_type", "it_type", "pa_type")

  n.sum <- list()

  for(i in seq_along(sum.vars)) {
    # var.sel <- paste0("is.na(", sum.vars[i], ")")
    n.sum[[i]] <- 
      data.proc[,
                .(n = .N),
                by = c(sum.vars[i], "som_x", "som_y", "som_bmu")]
  }
  
  n.sum[[length(sum.vars) + 1]] <-
      data.proc[,
                .(n = .N),
                by = c("som_x", "som_y", "som_bmu")]

  som.sum[[region]]$n <-
  rbindlist(n.sum, fill = TRUE) |>
  subset((it_type != "none" | is.na(it_type)) &
         (pa_type != "none" | is.na(pa_type)) &
         (for_type != "other" | is.na(for_type))) |>
  setcolorder(c("for_type", "it_type", "pa_type", "som_x", "som_y", "som_bmu")) |>
  merge(cat.lab, by = c("for_type", "it_type", "pa_type"),
        all.x = TRUE, all.y = FALSE, sort = FALSE) |>
  setorder("cat.label", "som_x", "som_y", "som_bmu")

}


somvis <- list()

for(i in seq_along(regions)) {
    
  region <- regions[i]

  message(paste0("Preparing SOM visualization for region `", region, "` …"))

  # Risk

  somvis[[region]]$r <-
    som.sum[[region]]$r |>
    ggplot() +
      geom_tile(aes(x = som_x, y = som_y, fill = q97.5)) +
      scale_fill_continuous_sequential(palette = "Viridis",
                                       rev = FALSE,
                                       limits = c(0,0.4),
                                       labels = scales::label_percent()) +
      scale_x_continuous(limits = c(1, 100),
                         breaks = c(25, 50, 75, 100)-0.5,
                         labels = c(25, 50, 75, 100),
                         expand = expansion(0, 0)) +
      scale_y_continuous(limits = c(1, 100),
                         breaks = c(25, 50, 75, 100)-0.5,
                         labels = c(25, 50, 75, 100),
                         expand = expansion(0, 0)) +
      coord_fixed() +
      som_guide_fill +
      labs(fill = title.wrap("Forest loss risk"), x = "SOM 1", y = "SOM 2") +
      facet_wrap(vars(cat.label), ncol = 2) +
      som_theme

  # Absolute risk change

  somvis[[region]]$arc <-
    som.sum[[region]]$arc |>
    ggplot() +
      geom_tile(aes(x = som_x, y = som_y, fill = mean)) +
      scale_fill_continuous_divergingx(palette = "Roma",
                                       ,rev = TRUE
                                       # , trans = scales::yj_trans(0)
                                       ,breaks = seq(-0.2, 0.2, 0.05),
                                       ,labels = c("≤ -20%", "", "-10%", "", 0,
                                                   "", "+10%", "", "≥ +20%"),
                                       ,limits = c(-0.20, 0.20)
                                       ,oob = scales::squish
                                       ) +
      scale_x_continuous(limits = c(1, 100),
                         breaks = c(25, 50, 75, 100)-0.5,
                         labels = c(25, 50, 75, 100),
                         expand = expansion(0, 0)) +
      scale_y_continuous(limits = c(1, 100),
                         breaks = c(25, 50, 75, 100)-0.5,
                         labels = c(25, 50, 75, 100),
                         expand = expansion(0, 0)) +
      coord_fixed() +
      som_guide_fill +
      labs(fill = title.wrap("Absolute change in forest loss risk"),
           x = "SOM 1", y = "SOM 2") +
      facet_wrap(vars(cat.label), ncol = 2) +
      som_theme

  # Covariates
 
  cov.scale <- cov.scales[[region]]
  covariates <- names(cov.scale)

  for(j in seq_along(covariates)) {
    somvis[[region]]$cov[[covariates[j]]] <-
      melt(som.sum[[region]]$cov,
           id.vars = c("som_x", "som_y", "som_bmu"),
           variable.name = "covariate") |>
      subset(covariate == covariates[j]) |>
      ggplot() +
        geom_tile(aes(x = som_x, y = som_y, fill = value)) +
        scale_fill_continuous_sequential(palette = "Viridis"
                                         ,rev = FALSE,
                                         ,trans = cov.scale[[covariates[j]]]$trans
                                         ,breaks = cov.scale[[covariates[j]]]$breaks
                                         ,limits = cov.scale[[covariates[j]]]$limits
                                         ,labels = cov.scale[[covariates[j]]]$labels
                                         ,name = title.wrap(cov.scale[[covariates[j]]]$title)
                                         ,oob = scales::squish
                                         ) +
       scale_x_continuous(limits = c(1, 100),
                          breaks = c(25, 50, 75, 100)-0.5,
                          labels = c(25, 50, 75, 100),
                          expand = expansion(0, 0)) +
       scale_y_continuous(limits = c(1, 100),
                          breaks = c(25, 50, 75, 100)-0.5,
                          labels = c(25, 50, 75, 100),
                          expand = expansion(0, 0)) +
       coord_fixed() +
       som_guide_fill +
       labs(x = "SOM 1", y = "SOM 2") +
       som_theme
  }


  # Number of samples
  somvis[[region]]$n <-
    som.sum[[region]]$n |>
    ggplot() +
      geom_tile(aes(x = som_x, y = som_y, fill = n)) +
      scale_fill_continuous_sequential(palette = "Viridis",
                                       rev = FALSE) +
      scale_x_continuous(limits = c(1, 100),
                         breaks = c(25, 50, 75, 100)-0.5,
                         labels = c(25, 50, 75, 100),
                         expand = expansion(0, 0)) +
      scale_y_continuous(limits = c(1, 100),
                         breaks = c(25, 50, 75, 100)-0.5,
                         labels = c(25, 50, 75, 100),
                         expand = expansion(0, 0)) +
      coord_fixed() +
      som_guide_fill +
      labs(fill = title.wrap("No. of samples"),
           x = "SOM 1", y = "SOM 2") +
      facet_wrap(vars(cat.label), ncol = 2) +
      som_theme

}

for(i in seq_along(regions)) {
  region <- regions[i]
  som.r.arc.combined <-
    with(somvis[[region]],
         r / arc) +
    plot_layout(heights = c(1,2.1)) +
    plot_annotation(tag_levels = "A") &
    som_theme
  tiff(paste0(path.figures, "si.som.r.arc.", region, ".tif"),
       width = 6.7, height = 9, unit = "in", res = 300)
  print(som.r.arc.combined)
  dev.off()
}

for(i in seq_along(regions)) {
  region <- regions[i]
  som.cov.combined <-
    with(somvis[[region]]$cov,
         tri + dist_set.km + dist_roads.km + dist_rivers.km + dens_pop + dens_roads +
         plot_layout(ncol = 2, nrow = 3) +
         plot_annotation(tag_levels = "A") &
         som_theme)
  tiff(paste0(path.figures, "si.som.cov.", region, ".tif"),
       width = 6.7, height = 7.5, unit = "in", res = 300)
  print(som.cov.combined)
  dev.off()
}




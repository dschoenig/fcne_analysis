source("utilities.R")

library(data.table)
library(sf)
library(units)

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
file.data.amz <- paste0(path.data.proc, "amz.data.fit.proc.rds")
file.data.cam <- paste0(path.data.proc, "cam.data.fit.proc.rds")
file.stats.amz <- paste0(path.data.proc, "amz.sumstats.proc.rds")
file.stats.cam <- paste0(path.data.proc, "cam.sumstats.proc.rds")
file.lim.amz <- paste0(path.data.raw, "amz.limit.gpkg")
file.lim.cam <- paste0(path.data.raw, "cam.limit.gpkg")

file.area.amz <- paste0(path.data.proc, "amz.sumstats.area.rds")
file.area.cam <- paste0(path.data.proc, "cam.sumstats.area.rds")
file.area.cam.nh <- paste0(path.data.proc, "cam.sumstats.area.no_hurr.rds")

stats.amz <- readRDS(file.stats.amz)
stats.cam <- readRDS(file.stats.cam)
lim.amz <- st_read(file.lim.amz)
lim.cam <- st_read(file.lim.cam)

n.amz <- nrow(stats.amz)
n.cam <- nrow(stats.cam)
a.amz <- set_units(lim.amz$area/1e6, km^2)
a.cam <- set_units(lim.cam$area/1e6, km^2)
p.amz <- a.amz/n.amz
p.cam <- a.cam/n.cam
n.tmf.amz <- stats.amz[tmf_annual_2010 == 1, .N]
n.tmf.cam <- stats.cam[tmf_annual_2010 == 1, .N]
tmf.amz <- n.tmf.amz * p.amz
tmf.cam <- n.tmf.cam * p.cam

# Exclude points outside of analyzed countries
stats.amz <-stats.amz[!is.na(adm0)]
stats.cam <-stats.cam[!is.na(adm0)]

a.tmf.amz <- stats.amz[tmf_annual_2010 == 1, .N*p.amz]
a.tmf.cam <- stats.cam[tmf_annual_2010 == 1, .N*p.cam]


stats.amz[,
          `:=`(
               deforestation = fifelse((tmf_annual_2010 == 1) &
                                       (tmf_def > 2010 & tmf_def <= 2020),
                                       TRUE, FALSE),
               degradation = fifelse((tmf_annual_2010 == 1) &
                                     ((tmf_deg > 2010 & tmf_deg <= 2020) &
                                      (tmf_def <= 2010 | tmf_def > 2020)),
                                     TRUE, FALSE),
               disturbance = fifelse((tmf_annual_2010 == 1) &
                                     ((tmf_def > 2010 & tmf_def <= 2020) |
                                      (tmf_deg > 2010 & tmf_deg <= 2020)),
                                     TRUE, FALSE))]
stats.cam[,
          `:=`(
               deforestation = fifelse((tmf_annual_2010 == 1) &
                                       (tmf_def > 2010 & tmf_def <= 2020),
                                       TRUE, FALSE),
               degradation = fifelse((tmf_annual_2010 == 1) &
                                     ((tmf_deg > 2010 & tmf_deg <= 2020) &
                                      (tmf_def <= 2010 | tmf_def > 2020)),
                                     TRUE, FALSE),
               disturbance = fifelse((tmf_annual_2010 == 1) &
                                     ((tmf_def > 2010 & tmf_def <= 2020) |
                                      (tmf_deg > 2010 & tmf_deg <= 2020)),
                                     TRUE, FALSE))]


# Sanity checks
sum(stats.amz$deforestation) + sum(stats.amz$degradation) == sum(stats.amz$disturbance)
sum(stats.cam$deforestation) + sum(stats.cam$degradation) == sum(stats.cam$disturbance)


# Determine aggregation for maps (same as for counterfactuals)
data.amz <- readRDS(file.data.amz)
data.cam <- readRDS(file.data.cam)
map.res <- list(amz = 1e4, cam = 5e3)
map.anchor <-
  list(
       amz = c(ea_east = floor(min(data.amz$ea_east / map.res$amz)) * map.res$amz,
               ea_north = floor(min(data.amz$ea_north / map.res$amz)) * map.res$amz),
       cam = c(ea_east = floor(min(data.cam$ea_east / map.res$cam)) * map.res$cam,
               ea_north = floor(min(data.cam$ea_north / map.res$cam)) * map.res$cam))
# rm(data.amz, data.cam)
# gc()


stats.amz <-
  bin_cols(stats.amz,
           columns = c("ea_east", "ea_north"),
           bin.res = rep(map.res$amz, 2),
           bin.min = map.anchor$amz,
           append = TRUE)
stats.cam <-
  bin_cols(stats.cam,
           columns = c("ea_east", "ea_north"),
           bin.res = rep(map.res$cam, 2),
           bin.min = map.anchor$cam,
           append = TRUE)


areas.undist.px.amz <-
  stats.amz[,
            .(area = (map.res$amz/1e3)^2 * sum(tmf_annual_2010 == 1)/.N),
            by = .(ea_east.bin, ea_north.bin)]
areas.undist.px.amz[, area := set_units(area, "km^2")]

areas.undist.px.cam <-
  stats.cam[,
            .(area = (map.res$cam/1e3)^2 * sum(tmf_annual_2010 == 1)/.N),
            by = .(ea_east.bin, ea_north.bin)]
areas.undist.px.cam[, area := set_units(area, "km^2")]

areas.undist.px.cam.nh <-
  stats.cam[hurr_lf == FALSE,
            .(area = (map.res$cam/1e3)^2 * sum(tmf_annual_2010 == 1)/.N),
            by = .(ea_east.bin, ea_north.bin)]
areas.undist.px.cam.nh[, area := set_units(area, "km^2")]


areas.undist.amz <-
  list(
       stats.amz[tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz)] ,
       stats.amz[tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz),
                 by = .(it_type)] ,
       stats.amz[tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz),
                 by = .(pa_type)],
       stats.amz[tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz),
                 by = .(it_type, pa_type)],
       stats.amz[tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz),
                 by = .(it_type, pa_type, adm0)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.amz, adm0, it_type, pa_type)


areas.undist.cam <-
  list(
       stats.cam[tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam)] ,
       stats.cam[tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam),
                 by = .(it_type)] ,
       stats.cam[tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam),
                 by = .(pa_type)],
       stats.cam[tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam),
                 by = .(it_type, pa_type)],
       stats.cam[tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam),
                 by = .(it_type, pa_type, adm0)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.cam, adm0, it_type, pa_type)

areas.undist.cam.nh <-
  list(stats.cam[hurr_lf == FALSE & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam)],
       stats.cam[hurr_lf == FALSE & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam),
                 by = .(it_type)],
       stats.cam[hurr_lf == FALSE & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam),
                 by = .(pa_type)],
       stats.cam[hurr_lf == FALSE & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam),
                 by = .(it_type, pa_type)],
       stats.cam[hurr_lf == FALSE & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = (.N*p.cam)/a.tmf.cam),
                 by = .(it_type, pa_type, adm0)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.cam.nh, adm0, it_type, pa_type)



areas.undist.ov.amz <-
  list(stats.amz[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz)],
       stats.amz[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz),
                 by = .(it_type)],
       stats.amz[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz),
                 by = .(pa_type)],
       stats.amz[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz),
                 by = .(it_type, pa_type)],
       stats.amz[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = (.N*p.amz)/a.tmf.amz),
                 by = .(it_type, pa_type, adm0)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.ov.amz, adm0, it_type, pa_type)

areas.undist.ov.cam <-
  list(stats.cam[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam)],
       stats.cam[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam),
                 by = .(it_type)],
       stats.cam[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam),
                 by = .(pa_type)],
       stats.cam[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam),
                 by = .(it_type, pa_type)],
       stats.cam[overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam),
                 by = .(it_type, pa_type, adm0)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.ov.cam, adm0, it_type, pa_type)

areas.undist.ov.cam.nh <-
  list(stats.cam[hurr_lf == FALSE & overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam)],
       stats.cam[hurr_lf == FALSE & overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam),
                 by = .(it_type)],
       stats.cam[hurr_lf == FALSE & overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam),
                 by = .(pa_type)],
       stats.cam[hurr_lf == FALSE & overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam),
                 by = .(it_type, pa_type)],
       stats.cam[hurr_lf == FALSE & overlap != "none" & tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N*p.cam),
                 by = .(it_type, pa_type, adm0)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.ov.cam.nh, adm0, it_type, pa_type)


adm0.rec.amz <-
  dcast(data.amz, adm0 ~ it_type, fun.aggregate = length) |>
  _[recognized > 0 & not_recognized > 0, adm0]
adm0.ind.amz <-
  dcast(data.amz, adm0 ~ pa_type, fun.aggregate = length) |>
  _[indirect_use > 0 & direct_use > 0, adm0]
adm0.rec.cam <-
  dcast(data.cam, adm0 ~ it_type, fun.aggregate = length) |>
  _[recognized > 0 & not_recognized > 0, adm0]
adm0.ind.cam <-
  dcast(data.cam, adm0 ~ pa_type, fun.aggregate = length) |>
  _[indirect_use > 0 & direct_use > 0, adm0]
adm0.rec.cam.nh <-
  dcast(data.cam[hurr_lf == FALSE], adm0 ~ it_type, fun.aggregate = length) |>
  _[recognized > 0 & not_recognized > 0, adm0]
adm0.ind.cam.nh <-
  dcast(data.cam[hurr_lf == FALSE], adm0 ~ pa_type, fun.aggregate = length) |>
  _[indirect_use > 0 & direct_use > 0, adm0]


areas.undist.comp.amz <-
list(
     stats.amz[adm0 %in% adm0.rec.amz & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz)],
     stats.amz[adm0 %in% adm0.rec.amz & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
     by = .(it_type, pa_type)],
     stats.amz[adm0 %in% adm0.rec.amz & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
               by = .(adm0)],
     stats.amz[adm0 %in% adm0.rec.amz & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
     by = .(it_type, pa_type, adm0)],
     stats.amz[adm0 %in% adm0.rec.amz & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz)],
     stats.amz[adm0 %in% adm0.rec.amz & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
     by = .(it_type, pa_type)],
     stats.amz[adm0 %in% adm0.rec.amz & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
               by = .(adm0)],
     stats.amz[adm0 %in% adm0.rec.amz & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
               by = .(it_type, pa_type, adm0)],
     stats.amz[adm0 %in% adm0.ind.amz & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz)],
     stats.amz[adm0 %in% adm0.ind.amz & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
               by = .(it_type, pa_type)],
     stats.amz[adm0 %in% adm0.ind.amz & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
               by = .(adm0)],
     stats.amz[adm0 %in% adm0.ind.amz & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
               by = .(it_type, pa_type, adm0)],
     stats.amz[adm0 %in% adm0.ind.amz & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz)],
     stats.amz[adm0 %in% adm0.ind.amz & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
               by = .(it_type, pa_type)],
     stats.amz[adm0 %in% adm0.ind.amz & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
               by = .(adm0)],
     stats.amz[adm0 %in% adm0.ind.amz & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.amz,
                 area.rel = (.N*p.amz)/a.tmf.amz),
     by = .(it_type, pa_type, adm0)]) |>
rbindlist(fill = TRUE)
setorder(areas.undist.comp.amz, adm0, it_type, pa_type)

areas.undist.comp.cam <-
list(
     stats.cam[adm0 %in% adm0.rec.cam & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam)],
     stats.cam[adm0 %in% adm0.rec.cam & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
     by = .(it_type, pa_type)],
     stats.cam[adm0 %in% adm0.rec.cam & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(adm0)],
     stats.cam[adm0 %in% adm0.rec.cam & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
     by = .(it_type, pa_type, adm0)],
     stats.cam[adm0 %in% adm0.rec.cam & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam)],
     stats.cam[adm0 %in% adm0.rec.cam & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
     by = .(it_type, pa_type)],
     stats.cam[adm0 %in% adm0.rec.cam & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(adm0)],
     stats.cam[adm0 %in% adm0.rec.cam & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
     by = .(it_type, pa_type, adm0)],
     stats.cam[adm0 %in% adm0.ind.cam & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam)],
     stats.cam[adm0 %in% adm0.ind.cam & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
     by = .(it_type, pa_type)],
     stats.cam[adm0 %in% adm0.ind.cam & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(adm0)],
     stats.cam[adm0 %in% adm0.ind.cam & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
     by = .(it_type, pa_type, adm0)],
     stats.cam[adm0 %in% adm0.ind.cam & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam)],
     stats.cam[adm0 %in% adm0.ind.cam & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
     by = .(it_type, pa_type)],
     stats.cam[adm0 %in% adm0.ind.cam & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(adm0)],
     stats.cam[adm0 %in% adm0.ind.cam & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
     by = .(it_type, pa_type, adm0)]) |>
rbindlist(fill = TRUE)
setorder(areas.undist.comp.cam, adm0, it_type, pa_type)


areas.undist.comp.cam.nh <-
list(stats.cam[adm0 %in% adm0.rec.cam.nh & 
               hurr_lf == FALSE & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam)],
     stats.cam[adm0 %in% adm0.rec.cam.nh & 
               hurr_lf == FALSE & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(it_type, pa_type)],
     stats.cam[adm0 %in% adm0.rec.cam.nh & 
               hurr_lf == FALSE & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(adm0)],
     stats.cam[adm0 %in% adm0.rec.cam.nh & 
               hurr_lf == FALSE & it_type == "recognized" & tmf_annual_2010 == 1,
               .(mar_type = "rec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(it_type, pa_type, adm0)],
     stats.cam[adm0 %in% adm0.rec.cam.nh & 
               hurr_lf == FALSE & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam)],
     stats.cam[adm0 %in% adm0.rec.cam.nh & 
               hurr_lf == FALSE & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(it_type, pa_type)],
     stats.cam[adm0 %in% adm0.rec.cam.nh & 
               hurr_lf == FALSE & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(adm0)],
     stats.cam[adm0 %in% adm0.rec.cam.nh & 
               hurr_lf == FALSE & it_type == "not_recognized" & tmf_annual_2010 == 1,
               .(mar_type = "nrec",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(it_type, pa_type, adm0)],
     stats.cam[adm0 %in% adm0.ind.cam.nh &
               hurr_lf == FALSE & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam)],
     stats.cam[adm0 %in% adm0.ind.cam.nh &
               hurr_lf == FALSE & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(it_type, pa_type)],
     stats.cam[adm0 %in% adm0.ind.cam.nh &
               hurr_lf == FALSE & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(adm0)],
     stats.cam[adm0 %in% adm0.ind.cam.nh &
               hurr_lf == FALSE & pa_type == "indirect_use" & tmf_annual_2010 == 1,
               .(mar_type = "ind",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(it_type, pa_type, adm0)],
     stats.cam[adm0 %in% adm0.ind.cam.nh &
               hurr_lf == FALSE & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam)],
     stats.cam[adm0 %in% adm0.ind.cam.nh &
               hurr_lf == FALSE & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(it_type, pa_type)],
     stats.cam[adm0 %in% adm0.ind.cam.nh &
               hurr_lf == FALSE & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(adm0)],
     stats.cam[adm0 %in% adm0.ind.cam.nh &
               hurr_lf == FALSE & pa_type == "direct_use" & tmf_annual_2010 == 1,
               .(mar_type = "dir",
                 area = .N*p.cam,
                 area.rel = (.N*p.cam)/a.tmf.cam),
               by = .(it_type, pa_type, adm0)]) |>
rbindlist(fill = TRUE)
setorder(areas.undist.comp.cam.nh, adm0, it_type, pa_type)



areas.amz <-
  list(undist = areas.undist.amz,
       undist.ov = areas.undist.ov.amz,
       undist.comp = areas.undist.comp.amz,
       undist.px = areas.undist.px.amz)
areas.cam <-
  list(undist = areas.undist.cam,
       undist.ov = areas.undist.ov.cam,
       undist.comp = areas.undist.comp.cam,
       undist.px = areas.undist.px.cam)
areas.cam.nh <-
  list(undist = areas.undist.cam.nh,
       undist.ov = areas.undist.ov.cam.nh,
       undist.comp = areas.undist.comp.cam.nh,
       undist.px = areas.undist.px.cam.nh)

saveRDS(areas.amz, file.area.amz)
saveRDS(areas.cam, file.area.cam)
saveRDS(areas.cam.nh, file.area.cam.nh)



# Individual estimates

stats.amz[,
          .(area = .N*p.amz,
            area.rel = (.N*p.amz)/a.tmf.amz),
          by = "tmf_annual_2010"]

stats.cam[,
          .(area = .N*p.cam,
            area.rel = (.N*p.cam)/a.tmf.cam),
          by = "tmf_annual_2010"]

stats.amz[,
          .(area = .N * p.amz,
            area.rel = (.N*p.amz)/a.tmf.amz),
          by = "deforestation"]
stats.cam[,
          .(area = .N * p.cam,
            area.rel = (.N*p.cam)/a.tmf.cam),
          by = "deforestation"]

stats.amz[,
          .(area = .N * p.amz,
            area.rel = (.N*p.amz)/a.tmf.amz),
          by = "degradation"]
stats.cam[,
          .(area = .N * p.cam,
            area.rel = (.N*p.cam)/a.tmf.cam),
          by = "degradation"]

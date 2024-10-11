slource("utilities.R")

library(data.table)
library(sf)
library(units)

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
file.stats.amz <- paste0(path.data.proc, "amz.sumstats.proc.rds")
file.stats.cam <- paste0(path.data.proc, "cam.sumstats.proc.rds")
file.lim.amz <- paste0(path.data.raw, "amz.limit.gpkg")
file.lim.cam <- paste0(path.data.raw, "cam.limit.gpkg")

file.area.amz <- paste0(path.data.proc, "amz.sumstats.area.rds")
file.area.cam <- paste0(path.data.proc, "cam.sumstats.area.rds")

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


areas.undist.amz <-
  list(stats.amz[tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = .N/n.amz)],
       stats.amz[tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = .N/n.amz),
                 by = .(it_type, pa_type)],
       stats.amz[tmf_annual_2010 == 1,
                 .(area = .N*p.amz,
                   area.rel = .N/n.amz),
                 by = .(it_type, pa_type, adm0)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.amz, adm0, it_type, pa_type)

areas.undist.cam <-
  list(stats.cam[tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N/n.cam)],
       stats.cam[tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N/n.cam),
                 by = .(it_type, pa_type)],
       stats.cam[tmf_annual_2010 == 1,
                 .(area = .N*p.cam,
                   area.rel = .N/n.cam),
                 by = .(it_type, pa_type, adm0)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.cam, adm0, it_type, pa_type)

areas.amz <- list(undist = areas.undist.amz)
areas.cam <- list(undist = areas.undist.cam)

saveRDS(areas.amz, file.area.amz)
saveRDS(areas.cam, file.area.cam)


stats.amz[,
          .(area = .N*p.amz,
            area.rel = .N/n.amz),
          by = "tmf_annual_2010"]

stats.cam[,
          .(area = .N*p.cam,
            area.rel = .N/n.cam),
          by = "tmf_annual_2010"]

stats.amz[,
          .(area = .N * p.amz,
            area.rel = .N/n.amz,
            area.rel.tmf = .N/n.tmf.amz),
          by = "deforestation"]
stats.cam[,
          .(area = .N * p.cam,
            area.rel = .N/n.cam,
            area.rel.tmf = .N/n.tmf.cam),
          by = "deforestation"]

stats.amz[,
          .(area = .N * p.amz,
            area.rel = .N/n.amz,
            area.rel.tmf = .N/n.tmf.amz),
          by = "degradation"]
stats.cam[,
          .(area = .N * p.cam,
            area.rel = .N/n.cam,
            area.rel.tmf = .N/n.tmf.cam),
          by = "degradation"]

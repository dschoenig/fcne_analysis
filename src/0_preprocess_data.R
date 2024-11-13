args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(sf)

region <- tolower(as.character(args[1]))
# region <- "amz"

## Paths
path.data.raw <- "../data/raw/"
path.data.aux <- "../data/auxiliary/"
path.data.int <- "../data/intermediate/"
path.data.proc <- "../data/processed/"

if(!dir.exists(path.data.int)){
  dir.create(path.data.int, recursive = TRUE)
}
if(!dir.exists(path.data.proc)){
  dir.create(path.data.proc, recursive = TRUE)
}

n.fit <- 1e7
n.val <- 2e6


message(paste0("Processing region '", region, "` …")) 

file.data.raw <- paste0(path.data.raw, region, ".1120.vars.csv")
file.data.df <- paste0(path.data.raw, region, ".1120.df.csv")
file.stats <- paste0(path.data.raw, region, ".sumstats_2015.csv")
file.areas.it <- paste0(path.data.raw, region, ".indterr.csv")
file.areas.it_pts <- paste0(path.data.raw, region, ".1120.indterr_pts.csv")
file.areas.pa <- paste0(path.data.raw, region, ".pareas.csv")
file.areas.pa_pts <- paste0(path.data.raw, region, ".1120.pareas_pts.csv")

file.data.fit.int <- paste0(path.data.int, region, ".data.fit.int.rds")
file.data.val.int <- paste0(path.data.int, region, ".data.val.int.rds")
file.df.proc <- paste0(path.data.proc, region, ".df.proc.rds")
file.areas.out <- paste0(path.data.proc, region, ".areas.it_pa.rds")
file.stats.proc <- paste0(path.data.proc, region, ".sumstats.proc.rds")

vars <- fread(file.data.raw, 
              na.strings = "",
              key = "id")

vars[, 
     `:=`(
          deforestation = fifelse(tmf_def > 0 & tmf_def <= 2020,
                                  TRUE, FALSE),
          degradation = fifelse((tmf_deg > 2010 & tmf_deg <= 2020) &
                                (tmf_def == 0 | tmf_def > 2020),
                                TRUE, FALSE),
          disturbance = fifelse((tmf_def > 2010 & tmf_def <= 2020) |
                                (tmf_deg > 2010 & tmf_deg <= 2020),
                                TRUE, FALSE),
          it = fifelse(it == "t", TRUE, FALSE),
          it_type = factor(it_type,
                           levels = c("none", "recognized", "not_recognized"),
                           ordered = TRUE),
          pa = fifelse(pa == "t", TRUE, FALSE),
          pa_type = factor(pa_type,
                           levels = c("none", "indirect_use", "direct_use"),
                           ordered = TRUE),
          adm0 = factor(adm0),
          driver = factor(fcase(is.na(driver), "not_identified",
                                driver == 1, "commodity",
                                driver == 2, "shifting_cultivation",
                                driver == 3, "forestry",
                                driver == 4, "wildfires",
                                driver == 5, "urbanization"),
                          levels = c("not_identified",
                                     "commodity", "shifting_cultivation",
                                     "forestry", "wildfires", "urbanization"))
          )
     ]

vars[is.na(it_type), it_type := "none"]
vars[is.na(pa_type), pa_type := "none"]
vars[pa_type != "none" & it_type != "none",
     overlap := paste(it_type, pa_type, sep = ":")]
vars[is.na(overlap), overlap := "none"]
vars[, overlap := factor(overlap,
                         levels = c("none",
                                    "recognized:indirect_use",
                                    "recognized:direct_use",
                                    "not_recognized:indirect_use",
                                    "not_recognized:direct_use"),
                         ordered = TRUE)]

vars.sel <-
  c("id", "adm0",
    "deforestation", "degradation", "disturbance",
    "tmf_def", "tmf_deg",
    "it", "it_type", "pa", "pa_type", "overlap",
    "elevation", "slope", "sx", "cmi_min",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_roads", "dens_pop", "travel_time",
    "driver",
    "lon", "lat",
    "ed_east", "ed_north", "ea_east", "ea_north")

vars <- vars[, ..vars.sel]


# Subsample 12 million observations with complete covariate
# information and split into fit and validation sets
set.seed(18470611)
sam.idx <- sample(na.omit(vars)$id, n.fit+n.val)

# Remove samples with incomplete covariate information 
data.int <- vars[.(sam.idx), on = "id"]


# For Central America only: Flag points close to landfall of hurricanes
# Otto (2016), Eta (2020), and Iota (2020)

if(region == "cam") {

  files.hurr <- paste0(path.data.aux,
                       c("al162016_lin.gpkg", "al292020_lin.gpkg", "al312020_lin.gpkg"))
  file.lim <- paste0(path.data.raw, region, ".limit.gpkg")
  file.lf <- paste0(path.data.proc, "cam.hurr.landfall.gpkg")

  crs.cam.ed <-
    st_crs('PROJCS["Central_America_Equidistant_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Equidistant_Conic"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900001"]]')

  crs.cam.ea <- 
    st_crs('PROJCS["Central_America_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900002"]]')

  lim <- st_read(file.lim)
  lim.line <- st_cast(st_exterior_ring(lim), "MULTILINESTRING")

  # Determine easternmost landfall for each hurricane

  lf.l <- list()

  for(i in seq_along(files.hurr)) {

    hurr.traj <-
      st_read(files.hurr[i]) |>
      st_transform(4326)

    hurr.int <-
      st_intersection(lim.line[, "geom"], hurr.traj[hurr.traj$SS >=3, "geom"]) |>
      st_cast("POINT")

    lf.id <- which.max(st_coordinates(hurr.int)[,1])

    lf.l[[i]] <-
      st_transform(hurr.int[lf.id,], crs.cam.ed) |>
      st_buffer(dist = 3e4, nQuadSegs = 100)

  }

  lf <- do.call(rbind, lf.l)

  st_transform(lf, 4326) |>
  st_write(file.lf, append = FALSE)

  pts.cri_nic <-
    data.int[adm0 %in% c("CRI", "NIC"), .(id, ed_east, ed_north)] |>
    st_as_sf(coords = c("ed_east", "ed_north")) |>
    st_set_crs(crs.cam.ed)
    
  pts.lf <-
    st_intersection(pts.cri_nic, lf)
  idx.lf <- sort(unique(pts.lf$id))

  data.int[id %in% idx.lf, hurr_lf := TRUE]
  data.int[is.na(hurr_lf), hurr_lf := FALSE]

}


# Process drought and fire data

# data.int.all <- readRDS(file.data.int)
data.df <-
  fread(file.data.df, na.strings = "", key = "id") |>
  _[data.int[, .(id)], on = "id"]

fire_cols <- paste0("fire_", 2011:2020)
di_cols <- paste0("di12_", 2013:2020)

data.df[,
        (fire_cols) := lapply(.SD, \(x) fifelse(x == "t", TRUE, FALSE)),
        .SDcols = fire_cols]

data.df[, fire := apply(.SD, 1, any), .SDcols = fire_cols]
data.df[, di12_min := apply(.SD, 1, min, na.rm = TRUE), .SDcols = di_cols]

# Flag severe and extreme drought over 12 months
data.df[, `:=`(drought_mod = FALSE,
               drought_sev = FALSE)]
data.df[di12_min <= -1, drought_mod := TRUE]
data.df[di12_min <= -1.5, drought_sev := TRUE]

saveRDS(data.df, file.df.proc)


data.int <- merge(data.int, data.df[, .(id, fire, drought_mod, drought_sev)])
setcolorder(data.int, c("drought_mod", "drought_sev", "fire"), after = "disturbance")


# Assign samples to fit and validation set
data.fit.int <- data.int[.(sam.idx[1:n.fit]), on = "id"]
data.val.int <- data.int[.(sam.idx[(n.fit+1):(n.fit + n.val)]), on = "id"]

print(nrow(na.omit(data.fit.int)) == n.fit)
print(nrow(na.omit(data.val.int)) == n.val)
print(length(which(data.val.int$id %in% data.fit.int$id)) == 0)

saveRDS(data.fit.int, file.data.fit.int)
saveRDS(data.val.int, file.data.val.int)

# df <- readRDS(file.data.fit.int)
# dv <- readRDS(file.data.val.int)
# all(df$id == data.fit.int$id)
# all(dv$id == data.val.int$id)

areas.it_pts <- fread(file.areas.it_pts)
areas.it <- fread(file.areas.it)

areas.it_pts <- areas.it_pts[pt_id %in% data.fit.int$id,
                             .(ids = list(pt_id)),
                             it_id]

areas.it <- areas.it[it_id %in% areas.it_pts$it_id]
areas.it[landmark_cat %in% c("acknowledged, documented", "acknowledged, not documented"),
         it_type := "recognized"]
areas.it[landmark_cat %in% c("not acknowledged, formal land claim submitted", 
                             "not acknowledged, customary tenure", "indicative areas"),
         it_type := "not_recognized"]

areas.it <- merge(areas.it, areas.it_pts, by = "it_id")


areas.pa_pts <- fread(file.areas.pa_pts)
areas.pa <- fread(file.areas.pa)

areas.pa_pts <- areas.pa_pts[pt_id %in% data.fit.int$id,
                             .(ids = list(pt_id)),
                             pa_id]

areas.pa <- areas.pa[pa_id %in% areas.pa_pts$pa_id]
areas.pa[iucn_cat %in% c("Ia", "Ib", "II", "III", "IV"),
         pa_type := "indirect_use"]
areas.pa[iucn_cat %in% c("V", "VI"),
         pa_type := "direct_use"]

areas.pa <- merge(areas.pa, areas.pa_pts, by = "pa_id")


areas.it_pa <- rbind(areas.it[, !"landmark_cat"], areas.pa[, !"iucn_cat"],
                     fill = TRUE)

areas.it_pa[,
            `:=`(it_type = factor(it_type,
                                  levels = c("none", "recognized", "not_recognized"),
                                  ordered = TRUE),
                 pa_type = factor(pa_type,
                                  levels = c("none", "indirect_use", "direct_use"),
                                  ordered = TRUE))]

areas.it_pa$area.id <- seq.int(nrow(areas.it_pa))
setnames(areas.it_pa, c("it_id", "pa_id"), c("it.id", "pa.id"))
setcolorder(areas.it_pa, c("area.id", "it.id", "pa.id", "adm0", "it_type", "pa_type", "ids"))
setkey(areas.it_pa, "area.id")

saveRDS(areas.it_pa, file.areas.out)

rm(data.int, data.fit.int, data.val.int, vars,
   areas.it, areas.it_pts, areas.pa, areas.pa_pts, areas.it_pa)


stats <- fread(file.stats, 
               na.strings = "",
               key = "id")

stats[, 
      `:=`(it = fifelse(it == "t", TRUE, FALSE),
           it_type = factor(it_type,
                            levels = c("none", "recognized", "not_recognized"),
                            ordered = TRUE),
           pa = fifelse(pa == "t", TRUE, FALSE),
           pa_type = factor(pa_type,
                            levels = c("none", "indirect_use", "direct_use"),
                            ordered = TRUE),
           adm0 = factor(adm0)
           )
     ]

stats[is.na(it_type), it_type := "none"]
stats[is.na(pa_type), pa_type := "none"]
stats[pa_type != "none" & it_type != "none",
      overlap := paste(it_type, pa_type, sep = ":")]
stats[is.na(overlap), overlap := "none"]
stats[, overlap := factor(overlap,
                          levels = c("none",
                                     "recognized:indirect_use",
                                     "recognized:direct_use",
                                     "not_recognized:indirect_use",
                                     "not_recognized:direct_use"),
                          ordered = TRUE)]


if(region == "cam") {
  pts.stats.cri_nic <-
    stats[adm0 %in% c("CRI", "NIC"), .(id, ea_east, ea_north)] |>
    st_as_sf(coords = c("ea_east", "ea_north")) |>
    st_set_crs(crs.cam.ea)
    
  pts.stats.lf <-
    st_intersection(pts.stats.cri_nic, st_transform(lf, crs.cam.ea))
  idx.stats.lf <- sort(unique(pts.lf$id))

  stats[id %in% idx.stats.lf, hurr_lf := TRUE]
  stats[is.na(hurr_lf), hurr_lf := FALSE]
}

stats.sel <- 
  c("id", "adm0",
    "tmf_annual_2010", "tmf_def", "tmf_deg",
    "it", "it_type", "pa", "pa_type", "overlap",
    "hurr_lf",
    "ea_east", "ea_north")

stats <- stats[, ..stats.sel]

saveRDS(stats, file.stats.proc)


library(data.table)
library(sf)

## Folders
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

regions <- c("amz", "cam")
# regions <- c("cam")


for(i in 1:length(regions)) {
  
  message(paste0("Processing region '", regions[i], "` …")) 

  file.data.raw <- paste0(path.data.raw, regions[i], ".1120.vars.csv")
  file.stats <- paste0(path.data.raw, regions[i], ".sumstats_2015.csv")
  file.areas.it <- paste0(path.data.raw, regions[i], ".indterr.csv")
  file.areas.it_pts <- paste0(path.data.raw, regions[i], ".1120.indterr_pts.csv")
  file.areas.pa <- paste0(path.data.raw, regions[i], ".pareas.csv")
  file.areas.pa_pts <- paste0(path.data.raw, regions[i], ".1120.pareas_pts.csv")

  file.data.fit.int <- paste0(path.data.int, regions[i], ".data.fit.int.rds")
  file.data.val.int <- paste0(path.data.int, regions[i], ".data.val.int.rds")
  file.areas.out <- paste0(path.data.proc, regions[i], ".areas.it_pa.rds")
  file.stats.proc <- paste0(path.data.proc, regions[i], ".sumstats.proc.csv")

  vars <- fread(file.data.raw, 
                na.strings = "",
                key = "id")

  vars[, 
       `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
            primary_forest = ifelse(primary_forest == "t", TRUE, FALSE),
            for_type = factor(ifelse(primary_forest == "t",
                                     "primary", "other"),
                              levels = c("other", "primary"),
                              ordered = TRUE),
            it = ifelse(it == "t", TRUE, FALSE),
            it_type = factor(it_type,
                             levels = c("none", "recognized", "not_recognized"),
                             ordered = TRUE),
            pa = ifelse(pa == "t", TRUE, FALSE),
            pa_type = factor(pa_type,
                             levels = c("none", "indirect_use", "direct_use"),
                             ordered = TRUE),
            adm0 = factor(adm0)
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

  setcolorder(vars,
              c("id", "adm0",
                "forestloss", "lossyear", "cover",
                "primary_forest", "for_type",
                "it", "it_type", "pa", "pa_type", "overlap",
                "elevation", "slope", "tri", "sx",
                "dist_set", "dist_roads", "dist_rivers",
                "dens_roads", "dens_pop",
                "lon", "lat",
                "ed_east", "ed_north", "ea_east", "ea_north"))


  # Subsample 12 million observations with complete covariate
  # information and split into fit and validation sets
  set.seed(18470611)
  sam.idx <- sample(na.omit(vars)$id, n.fit+n.val)

  # Remove samples with incomplete covariate information 
  data.int <- vars[.(sam.idx), on = "id"]

  # For Central America only: Flag points close to hurricane Otto (2016)

  if(regions[i] == "cam") {

    file.otto <- paste0(path.data.aux, "al162016_lin.gpkg")

    crs.cam.ed <-
      st_crs('PROJCS["Central_America_Equidistant_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Equidistant_Conic"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900001"]]')

    traj.otto <-
      st_read(file.otto) |>
      st_transform(crs.cam.ed)
    traj.otto.buf <-
      traj.otto[traj.otto$SS > 0,] |>
      st_union() |>
      st_as_sf() |>
      st_buffer(dist = 5e4)
    pts.cri_nic <-
      data.int[adm0 %in% c("CRI", "NIC"), .(id, ed_east, ed_north)] |>
      st_as_sf(coords = c("ed_east", "ed_north")) |>
      st_set_crs(crs.cam.ed)
    pts.otto <-
      st_intersection(pts.cri_nic, traj.otto.buf)
    
    data.int[id %in% pts.otto$id, hurr_otto := TRUE]
    data.int[is.na(hurr_otto), hurr_otto := FALSE]

  }

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
        `:=`(primary_forest = ifelse(primary_forest == "t", TRUE, FALSE),
             for_type = factor(ifelse(primary_forest == "t",
                                      "primary", "other"),
                               levels = c("other", "primary"),
                               ordered = TRUE),
             it = ifelse(it == "t", TRUE, FALSE),
             it_type = factor(it_type,
                              levels = c("none", "recognized", "not_recognized"),
                              ordered = TRUE),
             pa = ifelse(pa == "t", TRUE, FALSE),
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

  setcolorder(stats,
              c("id", "adm0",
                "dm", "lossyear", "cover",
                "primary_forest", "for_type",
                "it", "it_type", "pa", "pa_type", "overlap",
                "ea_east", "ea_north"))

  saveRDS(stats, file.stats.proc)

  rm(stats)

}

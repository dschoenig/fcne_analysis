library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.data <- "../data/"
path.data.raw <- paste0(path.data, "raw/")
path.data.int <- paste0(path.data, "intermediate/")
path.data.proc <- paste0(path.data, "processed/")

# region <- "amz"
region <- "cam"


file.data.raw <- paste0(path.data.raw, region, ".1120.vars.csv")

vars <- fread(file.data.raw, 
              na.strings = "",
              key = "id")

vars

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
  switch(region,
         amz = c("id", "adm0",
                 "deforestation", "degradation", "disturbance", "sci",
                 "tmf_def", "tmf_deg",
                 "it", "it_type", "pa", "pa_type", "overlap",
                 "elevation", "slope", "sx",
                 "dist_set", "dist_roads", "dist_rivers",
                 "dens_roads", "dens_pop", "travel_time",
                 "cmi_min",
                 "driver",
                 "lon", "lat",
                 "ed_east", "ed_north", "ea_east", "ea_north"),
         cam = c("id", "adm0",
                 "deforestation", "degradation", "disturbance", "sci",
                 "tmf_def", "tmf_deg",
                 "it", "it_type", "pa", "pa_type", "overlap",
                 "elevation", "slope", "sx",
                 "dist_set", "dist_roads", "dist_rivers",
                 "dens_roads", "dens_pop", "travel_time",
                 "cmi_min",
                 "driver",
                 "lon", "lat",
                 "ed_east", "ed_north", "ea_east", "ea_north",
                 "hurr_lf"))

vars <- vars[, ..vars.sel]


# Add variable to existing data set

datasets <- c("fit", "val")
vars.add <- c("cmi_min", "sci")
vars.join <- c("id", vars.add)
vars.old.int <- vars.sel[!vars.sel %in% vars.add]
vars.old.proc <- c(vars.old.int, "som_bmu", "som_x", "som_y")


for(i in seq_along(datasets)) {

  message(paste0("Processing dataset `", datasets[i], "` for region '`", region, "` …"))

  file.data.int <- paste0(path.data.int, region, ".data.", datasets[i], ".int.rds")
  file.data.proc <- paste0(path.data.proc, region, ".data.", datasets[i], ".proc.rds")
  
  data.int <- readRDS(file.data.int)[, ..vars.old.int]
  data.proc <- readRDS(file.data.proc)[, ..vars.old.proc]

  int.merged <-
    merge(data.int, vars[, ..vars.join],
          by = "id", all.x = TRUE, all.y = FALSE, sort = FALSE)

  proc.merged <-
    merge(data.proc, vars[, ..vars.join],
          by = "id", all.x = TRUE, all.y = FALSE, sort = FALSE)

  if(region == "cam") {
    vars.sel.int <-
      c("id", "adm0",
        "deforestation", "degradation", "disturbance", "sci",
        "tmf_def", "tmf_deg",
        "it", "it_type", "pa", "pa_type", "overlap",
        "elevation", "slope", "sx",
        "dist_set", "dist_roads", "dist_rivers",
        "dens_roads", "dens_pop", "travel_time",
        "cmi_min",
        "driver",
        "lon", "lat",
        "ed_east", "ed_north", "ea_east", "ea_north",
        "hurr_lf")
    vars.sel.proc <-
      c("id", "adm0",
        "deforestation", "degradation", "disturbance", "sci",
        "tmf_def", "tmf_deg",
        "it", "it_type", "pa", "pa_type", "overlap",
        "elevation", "slope", "sx",
        "dist_set", "dist_roads", "dist_rivers",
        "dens_roads", "dens_pop", "travel_time",
        "cmi_min",
        "driver",
        "lon", "lat",
        "ed_east", "ed_north", "ea_east", "ea_north",
        "hurr_lf",
        "som_bmu", "som_x", "som_y")
  } else {
    vars.sel.int <-
      c("id", "adm0",
        "deforestation", "degradation", "disturbance", "sci",
        "tmf_def", "tmf_deg",
        "it", "it_type", "pa", "pa_type", "overlap",
        "elevation", "slope", "sx",
        "dist_set", "dist_roads", "dist_rivers",
        "dens_roads", "dens_pop", "travel_time",
        "cmi_min",
        "driver",
        "lon", "lat",
        "ed_east", "ed_north", "ea_east", "ea_north")
    vars.sel.proc <-
      c("id", "adm0",
        "deforestation", "degradation", "disturbance", "sci",
        "tmf_def", "tmf_deg",
        "it", "it_type", "pa", "pa_type", "overlap",
        "elevation", "slope", "sx",
        "dist_set", "dist_roads", "dist_rivers",
        "dens_roads", "dens_pop", "travel_time",
        "cmi_min",
        "driver",
        "lon", "lat",
        "ed_east", "ed_north", "ea_east", "ea_north",
        "som_bmu", "som_x", "som_y")
  }

  print(data.int)
  print(int.merged)
  print(data.proc)
  print(proc.merged)

  message("Saving new data files …")

  saveRDS(int.merged[, ..vars.sel.int], file.data.int)
  saveRDS(proc.merged[, ..vars.sel.proc], file.data.proc)

}

args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.som <- "../models/som/"
path.data <- "../data/"
path.data.int <- paste0(path.data, "intermediate/")

path.data.proc <- paste0(path.data, "processed/")
if(!dir.exists(path.data.proc)){
  dir.create(path.data, recursive = TRUE)
}


region <- tolower(as.character(args[1]))
# region <- "amz"

## Map covariates to SOM #######################################################

datasets <- c("fit", "val")

file.areas.itpa <- paste0(path.data.proc, region, ".areas.it_pa.rds")
file.areas.proc <- paste0(path.data.proc, region, ".data.areas.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")

areas.itpa <- readRDS(file.areas.itpa)
areas.itpa[!is.na(it_type), area.label := paste0("it.", it.id)]
areas.itpa[!is.na(pa_type), area.label := paste0("pa.", pa.id)]
setcolorder(areas.itpa, c("area.id", "area.label"))


som.fit <- readRDS(file.som)

for(i in seq_along(datasets)) {
  file.data.int <- paste0(path.data.int, region, ".data.", datasets[i], ".int.rds")
  file.data.proc <- paste0(path.data.proc, region, ".data.", datasets[i], ".proc.rds")

  message(paste0("Embedding observations from `", file.data.int, "` …"))

  # a <- Sys.time()

  # data.int <- readRDS(file.data.int)

  # embedded <-
  #   egp_embed(data.int[,
  #                      .(tri, dist_set, dist_roads,
  #                        dist_rivers, dens_pop, dens_roads)],
  #             som.fit,
  #             vars = c("tri", "dist_set", "dist_roads",
  #                      "dist_rivers", "dens_pop", "dens_roads"),
  #             scale = TRUE,
  #             bmu.name = "som_bmu",
  #             coord = TRUE,
  #             coord.names = c("som_x", "som_y"),
  #             list = TRUE)

  # b <- Sys.time()
  # print(b-a)

  # data.int[,
  #          `:=`(som_bmu = embedded$som_bmu,
  #               som_x = embedded$som_x,
  #               som_y = embedded$som_y)
  #          ]

  # saveRDS(data.int, file.data.proc)

  if(datasets[i] == "fit") {


  data.int <- readRDS(file.data.proc)

    message("Matching observations to individual areas …")

    areas.itpa.idx <-
      areas.itpa[,
                  .(id = unlist(ids)),
                  by = .(area.id, it.id, pa.id)
                  ][id %in% data.int$id]

    data.areas <- data.int[areas.itpa.idx, on = "id"]
    data.full.ref <- data.int[it_type == "none" & pa_type == "none"]
    data.it_mar.areas <- data.areas[!is.na(it.id)]
    data.it_mar.ref <- data.int[it_type == "none"]
    data.pa_mar.areas <- data.areas[!is.na(pa.id)]
    data.pa_mar.ref <- data.int[pa_type == "none"]

    data.areas[, `:=`(comp = "full", comp.type = "fac")]
    data.full.ref[, `:=`(comp = "full", comp.type = "cf")]
    data.it_mar.areas[, `:=`(comp = "it_mar", comp.type = "fac")]
    data.it_mar.ref[, `:=`(comp = "it_mar", comp.type = "cf")]
    data.pa_mar.areas[, `:=`(comp = "pa_mar", comp.type = "fac")]
    data.pa_mar.ref[, `:=`(comp = "pa_mar", comp.type = "cf")]

    data.comp <-
      rbind(data.areas, data.full.ref,
            data.it_mar.areas, data.it_mar.ref,
            data.pa_mar.areas, data.pa_mar.ref,
            fill = TRUE) |>
      _[,
        .(comp, comp.type,
          id,
          area.id, it.id, pa.id,
          for_type,
          it_type, pa_type, overlap,
          ed_east, ed_north,
          som_bmu)]

    data.comp[, uid := 1:.N]
    data.comp[, `:=`(comp = factor(comp, levels = c("full", "it_mar", "pa_mar")),
                     comp.type = factor(comp.type, levels = c("cf", "fac")))]
    setcolorder(data.comp, c("uid", "comp", "comp.type", "area.id", "it.id", "pa.id", "id"))

    saveRDS(data.comp, file.areas.proc)

  }

}

# Add variable to existing data set
# for(i in seq_along(datasets)) {

#   file.data.int <- paste0(path.data.int, region, ".data.", datasets[i], ".int.rds")
#   file.data.proc <- paste0(path.data.proc, region, ".data.", datasets[i], ".proc.rds")
  
#   data.int <- readRDS(file.data.int)
#   data.proc <- readRDS(file.data.proc)

#   all(data.int$id == data.proc$id)

#   merged <- merge(data.proc, data.int[, .(id, for_type)],
#                   by = "id", all = TRUE, sort = FALSE)

#   setcolorder(merged,
#               c("id", "adm0",
#                 "forestloss", "lossyear",
#                 "primary_forest", "for_type",
#                 "it", "it_type", "pa", "pa_type", "overlap",
#                 "tri", "dist_set", "dist_roads", "dist_rivers",
#                 "dens_roads", "dens_pop",
#                 "lon", "lat",
#                 "ed_east", "ed_north", "ea_east", "ea_north"))

#   saveRDS(merged, file.data.proc)
# }

# variance_explained(som.fit)
# quantization_error(som.fit)
# topological_error(som.fit)

# unit_diff(data.int, unit.var = "som_bmu", group.var = "adm0")

# data.int



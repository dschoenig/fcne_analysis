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
n.cores <- as.integer(args[2])
region <- "cam"
n.cores <- 4

## Map covariates to SOM #######################################################

datasets <- c("fit", "val")

file.som <- paste0(path.som, region, ".som.1e6.rds")
som.fit <- readRDS(file.som)

for(i in seq_along(datasets)) {
  file.data.int <- paste0(path.data.int, region, ".data.", datasets[i], ".int.rds")
  file.data.proc <- paste0(path.data.proc, region, ".data.", datasets[i], ".proc.rds")
 

  message(paste0("Embedding observations from `", file.data.int, "` …"))

  data.int <- readRDS(file.data.int)
  
  mapped <- 
    data.int[, .(tri, dist_set, dist_roads, dist_rivers, dens_pop, dens_roads)] |>
    scale_data_som(som = som.fit) |>
    embed_som(som = som.fit,
              grid.coord = TRUE)

  data.int[,
           `:=`(som_bmu = mapped$bmu[,1],
                som_x = mapped$grid.coordinates$bmu.1[,"x"],
                som_y = mapped$grid.coordinates$bmu.1[,"y"])
           ]
  
  data.int <- readRDS(file.data.proc)
  data.int <- data.int[1:1000]
  
  # DETERMINE BASELINE BMUs

  bl.bmu <- data.int[it_type == "none" & pa_type == "none", .(n = .N), som_bmu]
  
  # If BMU contains a sufficient number of baseline observations, it is set to
  # be the baseline BMU.
  data.int[som_bmu %in% bl.bmu[n >= 100, som_bmu] &
           (pa_type != "none" | it_type != "none"),
           som_bmu.bl := som_bmu]

  # Observations to which multiple baseline BMUs have to be assigned (due to a
  # lack of baseline observations)
  
  data.mult.bmu.bl <-
    data.int[!som_bmu %in% bl.bmu[n >= 100, som_bmu] &
             (pa_type != "none" | it_type != "none")]

  data.mult.bmu.bl$som_bmu.bl <-
    scale_data_som(data.mult.bmu.bl[, .(tri, dist_set, dist_roads,
                                        dist_rivers, dens_pop, dens_roads)],
                   som.fit) |>
    bmu_match_reference(som.fit, bl.bmu[, .(som_bmu, n)], 100, 4)

  data.int <- merge(data.int, data.mult.bmu.bl[, .(id, som_bmu.bl)],
                    by = "id", all = TRUE)
  
  data.int[som_bmu %in% bl.bmu[n >= 100, som_bmu] &
           (pa_type != "none" | it_type != "none"),
           som_bmu.bl := som_bmu]
      


  saveRDS(data.int, file.data.proc)

}

args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
area_type <- tolower(as.character(args[3]))
hurr_type <- tolower(as.character(args[4]))

# n.threads <- 1
# region <- "amz"
# area_type <- "rec"
# hurr_type <- NA

setDTthreads(n.threads)

if(is.na(hurr_type)) {
  hurr_type <- "hurr"
}
if(hurr_type == "no_hurr" & region == "cam") {
  hurr_suf <- ".no_hurr"
} else {
  hurr_suf <- ""
}


paste0("Settings: ", paste(area_type, hurr_type, sep = ", ")) |>
message()

path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/cf/", region, "/")
if(!dir.exists(path.cf))
  dir.create(path.cf, recursive = TRUE)

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")
file.out <- paste0(path.cf, region, ".ten_comp.exp.", area_type, ".all", hurr_suf, ".rds")

data <- readRDS(file.data)
som.fit <- readRDS(file.som)

# Establish geographic range for comparisons (using entire study region)
pts.bb <-
  st_multipoint(x = as.matrix(data[, .(ed_east, ed_north)]), dim = "XY") |>
  st_minimum_bounding_circle() |>
  st_bbox()
geo.range <- pts.bb[["xmax"]] - pts.bb[["xmin"]]
rm(pts.bb)
silence <- gc()


if(region == "cam" & hurr_type == "no_hurr") {
  data <- data[hurr_lf == FALSE]
}


if(area_type %in% c("rec", "nrec")) {
  data.cf <-
    copy(data[it_type != "none",
              .(id, adm0,
                it_type, pa_type, 
                som_bmu, ed_east, ed_north)])
  rm(data)
  silence <- gc()
  comp.by <- c("pa_type", "adm0")
  if(area_type == "rec") {
  cf.ids <- data.cf[it_type == "not_recognized", id]  
  fac.ids <-
    data.cf[unique(data.cf[it_type == "not_recognized", .(pa_type, adm0)]),
            on = c("pa_type", "adm0")
            ][it_type == "recognized", id]
  }
  if(area_type == "nrec") {
  cf.ids <- data.cf[it_type == "recognized", id]  
  fac.ids <-
    data.cf[unique(data.cf[it_type == "recognized", .(pa_type, adm0)]),
            on = c("pa_type", "adm0")
            ][it_type == "not_recognized", id]
  }
}

if(area_type %in% c("ind", "dir")) {
  data.cf <-
    copy(data[pa_type != "none",
              .(id, adm0,
                it_type, pa_type, 
                som_bmu, ed_east, ed_north)])
  rm(data)
  silence <- gc()
  comp.by <- c("it_type", "adm0")
  if(area_type == "ind") {
  cf.ids <- data.cf[pa_type == "direct_use", id]  
  fac.ids <-
    data.cf[unique(data.cf[pa_type == "direct_use", .(it_type, adm0)]),
            on = c("it_type", "adm0")
            ][pa_type == "indirect_use", id]
  }
  if(area_type == "dir") {
  cf.ids <- data.cf[pa_type == "indirect_use", id]  
  fac.ids <-
    data.cf[unique(data.cf[pa_type == "indirect_use", .(it_type, adm0)]),
            on = c("it_type", "adm0")
            ][pa_type == "direct_use", id]
  }
}

group.var1 <- "pa_type"
group.var2 <- "it_type"
group.by <- list(NULL,
                 "adm0",
                 c(group.var1, group.var2),
                 c("adm0", group.var1, group.var2))

paste0("No. of data: ", nrow(data.cf)) |>
message()

paste0("No. of factual observations: ", length(fac.ids)) |>
message()

paste0("No. of counterfactual observations: ", length(cf.ids)) |>
message()

paste0("Compared by: ", paste(comp.by, collapse = ", ")) |>
message()

message("Grouped by: ")
print(group.by)

paste0("Counterfactual will be saved as ", file.out) |>
message()

message("Defining counterfactual …")



source("utilities.R")
a <- Sys.time()
cf.def <-
  egp_define_counterfactual(data = data.cf,
                            som = som.fit,
                            cf.ids = cf.ids,
                            fac.ids = fac.ids,
                            compare.by = comp.by,
                            group.by = group.by,
                            som.var = "som_bmu",
                            geo.vars = c("ed_east", "ed_north"),
                            geo.kernel = "matern32",
                            geo.range = geo.range,
                            id.var = "id",
                            group.name = "group.id",
                            unit.name = "cf.unit",
                            assign.name = "assigned",
                            assign.cat = c("counterfactual", "factual"),
                            n.min = 1,
                            nb.strategy = "expand",
                            deg.max = NULL,
                            agg.size = 1e6,
                            progress = TRUE)
b <- Sys.time()

print(b-a)

message("Saving output …")

saveRDS(cf.def, file.out)
gc()

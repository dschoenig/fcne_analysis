args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
area_type <- tolower(as.character(args[3]))
hurr_type <- tolower(as.character(args[4]))

# region <- "amz"
# area_type <- "it"
# hurr_type <- NA

setDTthreads(n.threads)

if(is.na(hurr_type)) {
  hurr_type <- "otto"
}
if(hurr_type == "no_otto" & region == "cam") {
  hurr_suf <- ".no_otto"
} else {
  hurr_suf <- ""
}

map.res <- switch(region,
                  amz = 1e4,
                  cam = 5e3)

paste0("Settings: ", paste(area_type, map.res, hurr_type, sep = ", ")) |>
message()

path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/cf/", region, "/")
if(!dir.exists(path.cf))
  dir.create(path.cf, recursive = TRUE)

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")
file.out <- paste0(path.cf, region, ".geo.",
                   area_type, hurr_suf, ".rds")

data <- readRDS(file.data)
som.fit <- readRDS(file.som)


if(region == "cam" & hurr_type == "no_otto") {
  data <- data[hurr_otto == FALSE]
}


map.anchor <- c(ea_east = floor(min(data$ea_east / map.res)) * map.res,
                ea_north = floor(min(data$ea_north / map.res)) * map.res)

data <-
  bin_cols(data,
           columns = c("ea_east", "ea_north"), bin.res = rep(map.res, 2),
           bin.min = map.anchor, append = TRUE)


if(area_type == "it") {
  cf.ids <- data[it_type == "none", id]  
  fac.ids <- data[it_type != "none" & pa_type != "none", id]  
  comp.by <- c("pa_type")
}
if(area_type == "pa") {
  cf.ids <- data[pa_type == "none", id]  
  fac.ids <- data[it_type != "none" & pa_type != "none", id]  
  comp.by <- c("it_type")
}
if(area_type == "itpa") {
  cf.ids <- data[it_type == "none" & pa_type == "none", id]  
  fac.ids <- data[it_type != "none" | pa_type != "none", id]  
  comp.by <- NULL
}

group.by <- list(c("ea_east.bin", "ea_north.bin"))

paste0("No. of data: ", nrow(data)) |>
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

data.cf <-
  copy(data[, .(id, adm0,
                it_type, pa_type,
                som_bmu, ed_east, ed_north,
                ea_east.bin, ea_north.bin)])
rm(data)
silence <- gc()

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
                            geo.range = NULL,
                            id.var = "id",
                            group.name = "group.id",
                            unit.name = "cf.unit",
                            assign.name = "assigned",
                            assign.cat = c("counterfactual", "factual"),
                            n.min = 1,
                            nb.strategy = "sequential",
                            deg.max = NULL,
                            agg.size = 1e6,
                            progress = TRUE)
b <- Sys.time()

print(b-a)

message("Saving output …")

saveRDS(cf.def, file.out)
gc()

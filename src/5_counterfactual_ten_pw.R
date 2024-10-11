args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
area_type <- tolower(as.character(args[3]))
ov_type <- tolower(as.character(args[4]))
hurr_type <- tolower(as.character(args[5]))

# n.threads <- 1
# region <- "amz"
# area_type <- "itpa"
# ov_type <- "all"
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


paste0("Settings: ", paste(area_type, ov_type, hurr_type, sep = ", ")) |>
message()

path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/cf/", region, "/")
if(!dir.exists(path.cf))
  dir.create(path.cf, recursive = TRUE)

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")

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
if(ov_type == "no_ov") {
  data <- data[overlap == "none"]
}
if(ov_type == "ov") {
  if(area_type == "it") {
    data <- data[pa_type != "none"]
  }
  if(area_type == "pa") {
    data <- data[it_type != "none"]
  }
}

data.cf <-
  copy(data[, .(id, adm0,
                it_type, pa_type, 
                som_bmu, ed_east, ed_north)])
rm(data)
silence <- gc()

combs <- unique(data.cf[order(it_type, pa_type), .(it_type, pa_type)])
combs[, label := paste0(it_type, "_", pa_type)]


files.out <- paste0(path.cf, region, ".ten_pw_",
                    1:nrow(combs), ".",
                    area_type, ".",
                    ov_type, hurr_suf, ".rds")

group.by <- list(c("it_type", "pa_type"), c("it_type", "pa_type", "adm0"))

for(i in 1:nrow(combs)) {

  it.foc <- combs[i, it_type]
  pa.foc <- combs[i, pa_type]

  cf.idx <- data.cf[it_type == it.foc & pa_type == pa.foc, id]
  fac.idx <- data.cf[!(it_type == it.foc & pa_type == pa.foc), id]


  paste0("Defining counterfactual ", i, "/", nrow(combs), " …") |>
  message()
  paste0("No. of data: ", nrow(data.cf)) |>
  message()
  paste0("Reference: `it_type == ", it.foc, "`, `pa_type == ", pa.foc, "`") |>
  message()
  paste0("No. of factual observations: ", length(fac.idx)) |>
  message()
  paste0("No. of counterfactual observations: ", length(cf.idx)) |>
  message()
  message("Grouped by: ")
  print(group.by)
  paste0("Counterfactual will be saved as ", files.out[i]) |>
  message()

  a <- Sys.time()
  cf.def <-
    egp_define_counterfactual(data = data.cf,
                              som = som.fit,
                              cf.ids = cf.idx,
                              fac.ids = fac.idx,
                              compare.by = c("adm0"),
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
                              nb.strategy = "sequential",
                              deg.max = NULL,
                              agg.size = 1e6,
                              progress = TRUE)
  
  setnames(cf.def$groups,
           c("it_type", "pa_type", "adm0"), 
           c("it_type.fac", "pa_type.fac", "adm0.fac")) 
  cf.def$groups[, `:=`(it_type.cf = it.foc, pa_type.cf = pa.foc)]

  setcolorder(cf.def$groups, c("group.id", "it_type.cf", "pa_type.cf", 
                               "it_type.fac", "pa_type.fac", "adm0.fac"))

  b <- Sys.time()

  print(b-a)

  message("Saving output …")

  saveRDS(cf.def, files.out[i])

  rm(it.foc, pa.foc, fac.idx, cf.idx, cf.def)
  gc()

}


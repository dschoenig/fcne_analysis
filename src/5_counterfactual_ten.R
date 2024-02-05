args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(arrow)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
for_type <- tolower(as.character(args[3]))
area_type <- tolower(as.character(args[4]))
ov_type <- tolower(as.character(args[5]))
hurr_type <- tolower(as.character(args[6]))

# n.threads <- 1
# region <- "amz"
# for_type <- "pf"
# area_type <- "it"
# ov_type <- "ov"
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


paste0("Settings: ", paste(area_type, for_type, ov_type, hurr_type, sep = ", ")) |>
message()

path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/cf/", region, "/")
if(!dir.exists(path.cf))
  dir.create(path.cf, recursive = TRUE)

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")
file.out <- paste0(path.cf, region, ".ten.",
                   for_type, ".",
                   area_type, ".",
                   ov_type, hurr_suf, ".rds")

data <- readRDS(file.data)

som.fit <- readRDS(file.som)

if(for_type == "pf") {
  data <- data[primary_forest == TRUE]
}
if(region == "cam" & hurr_type == "no_otto") {
  data <- data[hurr_otto == FALSE]
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

if(area_type %in% c("it", "pa")) {
  if(area_type == "it") {
    cf.ids <- data[it_type == "none", id]  
    fac.ids <- data[it_type != "none", id]  
    comp.by <- c("for_type", "pa_type")
    group.var1 <- "it_type"
    group.var2 <- "pa_type"
  }
  if(area_type == "pa") {
    cf.ids <- data[pa_type == "none", id]  
    fac.ids <- data[pa_type != "none", id]  
    comp.by <- c("for_type", "it_type")
    group.var1 <- "pa_type"
    group.var2 <- "it_type"
  }
  if(ov_type != "ov") {
    group.by <- list(NULL, group.var1, "adm0", c("adm0", group.var1))
  }
  if(ov_type == "ov") {
    group.by <- list(NULL,
                     group.var1, group.var2,
                     c(group.var1, group.var2),
                     "adm0",
                     c("adm0", group.var1), c("adm0", group.var2),
                     c("adm0", group.var1, group.var2))
  }
}
if(area_type == "itpa") {
  cf.ids <- data[it_type == "none" & pa_type == "none", id]  
  fac.ids <- data[it_type != "none" | pa_type != "none", id]  
  comp.by <- "for_type"
  group.var1 <- "pa_type"
  group.var2 <- "it_type"
  group.by <- list(NULL,
                   group.var1, group.var2,
                   c(group.var1, group.var2),
                   "adm0",
                   c("adm0", group.var1), c("adm0", group.var2),
                   c("adm0", group.var1, group.var2))
}


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
                it_type, pa_type, for_type,
                som_bmu, ed_east, ed_north)])
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

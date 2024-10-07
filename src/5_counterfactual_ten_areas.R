args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
comp_sel <- tolower(as.character(args[3]))
ov_type <- tolower(as.character(args[4]))
hurr_type <- tolower(as.character(args[5]))

# n.threads <- 1
# region <- "cam"
# comp_sel <- "pa_mar"
# ov_type <- "ov"
# hurr_type <- "no_hurr"

setDTthreads(n.threads)

if(is.na(hurr_type)) {
  hurr_type <- "hurr"
}
if(hurr_type == "no_hurr" & region == "cam") {
  hurr_suf <- ".no_hurr"
} else {
  hurr_suf <- ""
}


paste0("Settings: ", paste(comp_sel, ov_type, hurr_type, sep = ", ")) |>
message()

path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/cf/", region, "/")
if(!dir.exists(path.cf))
  dir.create(path.cf, recursive = TRUE)

file.data <- paste0(path.data.proc, region, ".data.areas.rds")
file.areas <- paste0(path.data.proc, region, ".areas.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")
file.out <- paste0(path.cf, region, ".ten.areas.",
                   comp_sel, ".",
                   ov_type, hurr_suf, ".rds")

data <- readRDS(file.data)

som.fit <- readRDS(file.som)

data <- data[comp == comp_sel]

if(region == "cam" & hurr_type == "no_hurr") {
  data <- data[hurr_lf == FALSE]
}

if(ov_type == "no_ov") {
  data <- data[comp.type == "cf" | (comp.type == "fac" & overlap == "none")]
}
if(ov_type == "ov") {
  if(comp_sel == "full") {
    data <- data[comp.type == "cf" | (comp.type == "fac" & overlap != "none")]
  }
  if(comp_sel == "it_mar") {
    data <- data[(comp.type == "cf" & pa_type != "none") | (comp.type == "fac" & overlap != "none")]
  }
  if(comp_sel == "pa_mar") {
    data <- data[(comp.type == "cf" & it_type != "none") | (comp.type == "fac" & overlap != "none")]
  }
}

cf.ids <- data[comp.type == "cf", uid]
fac.ids <- data[comp.type == "fac", uid]

if(comp_sel == "full") {
    comp.by <- "NULL"
    group.by <- list("area.id")
}
if(comp_sel == "it_mar") {
    comp.by <- c("pa_type")
    group.by <- list("it.id")
}
if(comp_sel == "pa_mar") {
    comp.by <- c("it_type")
    group.by <- list("pa.id")
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


a <- Sys.time()
cf.def <-
  egp_define_counterfactual(data = data,
                            som = som.fit,
                            cf.ids = cf.ids,
                            fac.ids = fac.ids,
                            compare.by = comp.by,
                            group.by = group.by,
                            som.var = "som_bmu",
                            geo.vars = c("ed_east", "ed_north"),
                            geo.kernel = "matern32",
                            geo.range = NULL,
                            id.var = "uid",
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

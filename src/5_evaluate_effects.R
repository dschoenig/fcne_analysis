source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

# region <- "amz"
# n.threads <- 4

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.proc.rds")
file.effects <- paste0(path.effects, region, ".effects.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

marginals <- c("full","ten_loc")
draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

if(region == "amz") {
  adm.it_rec <- c("BOL", "BRA", "COL", "ECU", "GUF", "GUY", "PER", "VEN")
  adm.it_not_rec <- c("BOL", "ECU", "GUF", "GUY", "PER", "SUR", "VEN")
}
if(region == "cam") {
  adm.it_rec <- c("CRI", "MEX", "NIC", "PAN")
  adm.it_not_rec <- c("BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV")
}


data.proc <- readRDS(paste0(path.base, "data/processed/", region, ".data.proc.rds"))

groups.it_pa <-
  data.proc |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
  subset(n >= 100)
groups.adm_it_pa <-
  data.proc[(it_type == "none") |
            (it_type == "recognized" & adm0 %in% adm.it_rec) |
            (it_type == "not_recognized" & adm0 %in% adm.it_not_rec)] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type")) |>
  subset(n >= 50)
groups <- rbindlist(list(groups.it_pa, groups.adm_it_pa), fill = TRUE)
groups$group.id <- 1:nrow(groups)
setcolorder(groups, c("group.id", "group.label", "adm0", "it_type", "pa_type"))

id.list <- groups$ids
names(id.list) <- groups$group.label

rm(data.proc)

effects <- list()
for(i in seq_along(marginals)) {
  ds <- open_dataset(paste0(path.arrow, "marginal=", marginals[i]), format = "arrow")
  message(paste0("Evaluating effects for region `", region,
                   "` over marginal `", marginals[i], "` (",
                   length(draw.ids), " draws) …"))
  effects[[i]] <- summarize_predictions(ds,
                                   ids = id.list,
                                   draw.ids = draw.ids,
                                   draw.chunk = 100,
                                   clamp = link_cll(c(.Machine$double.eps, 1-.Machine$double.eps)),
                                   n.threads = n.threads
                                   )
}
names(effects) <- marginals

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects, file.effects)

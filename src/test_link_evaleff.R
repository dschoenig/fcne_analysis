source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

# region <- "amz"
# n.threads <- 4

# path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.lp <- paste0(path.base, "models/gam/test_link/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/test_link/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.proc.rds")
file.effects <- paste0(path.effects, region, ".eff.tenure.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

marginals <- c("full","ten_loc")
draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

if(region == "cam") {
  adm.it_rec <- c("CRI", "MEX", "NIC", "PAN")
  adm.it_not_rec <- c("BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV")
}


message("Aggregating observations …")
data.proc <- readRDS(paste0(path.data.proc, region, ".data.proc.rds"))

if(region == "cam") {
  it.adm.rec <- c("NIC", "PAN", "MEX", "CRI")
  it.adm.nor <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")

  pa.adm.ind <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")
  pa.adm.dir <- c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN", "SLV")

  itpa.adm.rec.ind <- c("NIC", "PAN", "MEX", "CRI")
  itpa.adm.rec.dir <- c("PAN", "MEX", "CRI")
  itpa.adm.nor.ind <- c("BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV")
  itpa.adm.nor.dir <- c("SLV", "BLZ", "PAN", "GTM", "HND", "CRI")

  data.eff <-
    data.proc[(it_type == "none" & pa_type == "none") |
              (it_type == "recognized" & pa_type == "none" & adm0 %in% it.adm.rec) |
              (it_type == "not_recognized" & pa_type == "none" & adm0 %in% it.adm.nor) |
              (pa_type == "indirect_use" & it_type == "none" & adm0 %in% pa.adm.ind) |
              (pa_type == "direct_use" & it_type == "none" & adm0 %in% pa.adm.dir) |
              (it_type == "recognized" & pa_type == "indirect_use" & adm0 %in% itpa.adm.rec.ind) |
              (it_type == "recognized" & pa_type == "direct_use" & adm0 %in% itpa.adm.rec.dir) |
              (it_type == "not_recognized" & pa_type == "indirect_use" & adm0 %in% itpa.adm.nor.ind) |
              (it_type == "not_recognized" & pa_type == "direct_use" & adm0 %in% itpa.adm.nor.dir)]
}

rm(data.proc)

n.min <- 1
groups.bl <-
  data.eff[it_type == "none" & pa_type == "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
  subset(n >= n.min)
groups.bl_adm <-
  data.eff[it_type == "none" & pa_type == "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type")) |>
  subset(n >= n.min)
groups.it <-
  data.eff[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type")) |>
  subset(n >= n.min)
groups.pa <-
  data.eff[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("pa_type")) |>
  subset(n >= n.min)
groups.it_pa <-
  data.eff[it_type != "none" & pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
  subset(n >= n.min)
groups.adm_it <-
  data.eff[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type")) |>
  subset(n >= n.min)
groups.adm_pa <-
  data.eff[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "pa_type")) |>
  subset(n >= n.min)
groups.adm_it_pa <-
  data.eff[it_type != "none" & pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type")) |>
  subset(n >= n.min)

groups <- rbindlist(list(groups.bl,
                         groups.bl_adm,
                         groups.it,
                         groups.pa,
                         groups.it_pa,
                         groups.adm_it,
                         groups.adm_pa,
                         groups.adm_it_pa), fill = TRUE)
groups$group.id <- 1:nrow(groups)
setcolorder(groups, c("group.id", "group.label", "adm0", "it_type", "pa_type"))

id.list <- groups$ids
names(id.list) <- groups$group.label

rm(data.eff)

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
effects$groups <- groups

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects, file.effects)

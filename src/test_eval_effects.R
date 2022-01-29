source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])
n.threads <- as.integer(args[2])

region <- "cam"
n.threads <- 1

path.base <- "/home/schoed/scratch/fcne_analysis/"
# path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

path.arrow <- paste0(path.lp, region, ".lp/")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.effects <- paste0(path.effects, region, ".eff.tenure.rds")

set_cpu_count(n.threads)
setDTthreads(n.threads)

## EVALUATE EFFECTS ############################################################

# partial <- c("full","b.it", "b.pa", "b.ov")
draw.ids <- as.character(1:1000)
# draw.ids <- as.character(1:100)

message("Aggregating observations …")
data.proc <- readRDS(file.data)

data.proc <- data.proc[1:5e4,]

n.min <- 1
groups.bl <-
  data.proc[it_type == "none" & pa_type == "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
  subset(n >= n.min)
groups.bl_adm <-
  data.proc[it_type == "none" & pa_type == "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type")) |>
  subset(n >= n.min)
groups.it <-
  data.proc[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type")) |>
  subset(n >= n.min)
groups.pa <-
  data.proc[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("pa_type")) |>
  subset(n >= n.min)
groups.it_pa <-
  data.proc[it_type != "none" | pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type")) |>
  subset(n >= n.min)
groups.adm_it <-
  data.proc[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type")) |>
  subset(n >= n.min)
groups.adm_pa <-
  data.proc[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "pa_type")) |>
  subset(n >= n.min)
groups.adm_it_pa <-
  data.proc[it_type != "none" | pa_type != "none"] |>
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

rm(data.proc)


partial.subset <- list(full = "TRUE",
                       cov0 = "TRUE",
                       it0 = 'it_type != "none"',
                       pa0 = 'pa_type != "none"',
                       ov0 = 'it_type != "none" & pa_type != "none"',
                       cov0_it0 = 'it_type != "none"',
                       cov0_pa0 = 'pa_type != "none"',
                       cov0_ov0 = 'it_type != "none" & pa_type != "none"')

partial.idx <- list()
for(i in seq_along(partial.subset)) {
  partial.idx[[i]] <-
    groups[eval(parse(text = partial.subset[[i]])), ids]
  names(partial.idx[[i]]) <-
    groups[eval(parse(text = partial.subset[[i]])), group.label]
}
names(partial.idx) <- names(partial.subset)


system.time({
effects.partial <- list()
for(i in seq_along(partial.idx)) {
  name.par <- names(partial.idx)[i]
  # ds <- open_dataset(paste0(path.arrow, "partial=", name.par), format = "arrow")
  ds <- open_dataset(paste0(path.arrow, "partial=", name.par,
                            "/cam.lp-001.arrow"), format = "arrow")
  message(paste0("Evaluating effects for region `", region,
                   "` for (partial) linear predictor `", name.par, "` (",
                   length(draw.ids), " draws) …"))
  effects.partial[[i]] <- aggregate_variables(ds,
                                          agg.fun = E,
                                          trans.fun = inv_cloglog
                                          ids = partial.idx[[i]],
                                          draw.ids = draw.ids,
                                          draw.chunk = 1000,
                                          # draw.chunk = 1000,
                                          agg.size = 1e4,
                                          n.threads = n.threads,
                                          gc = TRUE
                                          )
}
})

names(effects.partial) <- names(partial.idx)
effects.partial$groups <- groups

message(paste0("Saving outputs to `", file.effects, "` …"))
saveRDS(effects.partial, file.effects)


# selg <- groups[is.na(adm0), group.label]
# selg <- groups[adm0 == "SLV", group.label]

# selg <- groups[is.na(adm0) & it_type != "none", group.label]

# summary(effects.partial[[1]][,selg])
# summary(effects.partial[[2]][,selg])
# summary(effects.partial[[3]][,selg])
# summary(effects.partial[[6]][,selg])


# selg <- groups[is.na(adm0) & it_type != "none", group.label]
# summary((effects.partial[[1]][,selg] - effects.partial[[3]][,selg]) / effects.partial[[3]][,selg],
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))
# summary((effects.partial[[2]][,selg] - effects.partial[[6]][,selg]) / effects.partial[[6]][,selg],
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))


# selg <- groups[is.na(adm0) & it_type != "none", group.label]
# # selg <- groups[it_type == "not_recognized" & is.na(pa_type), group.label]
# summary((effects.partial[[1]][,selg] - effects.partial[[3]][,selg]),
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))
# summary((effects.partial[[2]][,selg] - effects.partial[[6]][,selg]),
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))

# selg <- groups[is.na(adm0) & pa_type != "none", group.label]
# summary((effects.partial[[1]][,selg] - effects.partial[[4]][,selg]) / effects.partial[[4]][,selg],
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))
# summary((effects.partial[[2]][,selg] - effects.partial[[7]][,selg]) / effects.partial[[7]][,selg],
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))


# selg <- groups[is.na(adm0) & pa_type != "none", group.label]
# summary((effects.partial[[1]][,selg] - effects.partial[[4]][,selg]),
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))
# summary((effects.partial[[2]][,selg] - effects.partial[[7]][,selg]),
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))

# selg <- groups[is.na(adm0) & pa_type != "none" & it_type != "none", group.label]
# summary((effects.partial[[1]][,selg] - effects.partial[[5]][,selg]) / effects.partial[[5]][,selg],
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))
# summary((effects.partial[[2]][,selg] - effects.partial[[8]][,selg]) / effects.partial[[8]][,selg],
#         mean, median, ggdist::Mode, quantile2, \(x) quantile(x, c(0.25, 0.75)))


# summary((effects.partial[[1]] - effects.partial[[2]]) / effects.partial[[2]])

# summary(effects.partial[[1]][,selg])
# summary(effects.partial[[2]][,selg])






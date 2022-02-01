source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)
region <- tolower(args[1])

# region <- "cam"

path.base <- "/home/schoed/scratch/fcne_analysis/"
path.base <- "../"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")
path.effects <- paste0(path.base, "models/gam/effects/")
if(!dir.exists(path.effects)) dir.create(path.effects)

file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")
prefix.file.effects <- paste0(region, ".eff.riskchange.")
file.effects.geo <- paste0(path.effects, prefix.file.effects, "geo.rds")
file.effects.tenure <- paste0(path.effects, prefix.file.effects, "tenure.rds")

## RISK CHANGE DUE TO TENURE AND COVARIATES IN GEOGRAPHICAL SPACE ##############

maps <- 
  list(
       name = c("cov", "it_c", "it", "pa_c", "pa", "ov"),
       region = c("all", "it_c", "it", "pa_c", "pa", "ov"),
       partial.factual = rep("full", 6),
       partial.counterfactual = c("cov0", "it0", "it0", "pa0", "pa0", "ov0")
       )

# Risk change Covariate effect

rc.geo <- list()
for(i in seq_along(maps$name)) {
  risk.geo <- readRDS(paste0(path.effects, region, ".eff.risk.geo.",
                             maps$region[i], ".rds"))
  rc.geo[[i]] <-
    list(arc = arc(risk.geo[[maps$partial.factual[i]]],
                   risk.geo[[maps$partial.counterfactual[i]]]),
         rrc = rrc(risk.geo[[maps$partial.factual[i]]],
                   risk.geo[[maps$partial.counterfactual[i]]]),
         map.units = risk.geo$map.units)
  rm(risk.geo)
}
names(rc.geo) <- maps$name


message(paste0("Saving outputs to `", file.effects.geo, "` …"))
saveRDS(rc.geo, file.effects.geo)

rm(rc.geo)
gc()

## OVERALL RISK CHANGE DUE TO TENURE (ENTIRE STUDY REGION) #####################

risk.ten <- readRDS(paste0(path.effects, region, ".eff.risk.tenure.overall.rds"))

tenure <- 
  list(
    subset = c('it_type != "none" & is.na(pa_type)',
               'pa_type != "none" & is.na(it_type)',
               'it_type != "none" & pa_type != "none"'),
    partial.factual = rep("full", 3),
    partial.counterfactual = c("it0", "pa0", "ov0")
    )

adm.lev <- levels(risk.ten$groups$adm0)

sel.it <- risk.ten$groups[is.na(adm0) &
                          it_type != "none" & is.na(pa_type), group.label]
sel.pa <- risk.ten$groups[is.na(adm0) &
                          pa_type != "none" & is.na(it_type), group.label]
sel.ov <- risk.ten$groups[is.na(adm0) &
                          it_type != "none" & pa_type != "none", group.label]
eff.arc <- cbind(arc(risk.ten$full, risk.ten$it0, sel.it),
                 arc(risk.ten$full, risk.ten$pa0, sel.pa),
                 arc(risk.ten$full, risk.ten$ov0, sel.ov)) |>
           as_draws_matrix()
eff.rrc <- cbind(rrc(risk.ten$full, risk.ten$it0, sel.it),
                 rrc(risk.ten$full, risk.ten$pa0, sel.pa),
                 rrc(risk.ten$full, risk.ten$ov0, sel.ov)) |>
           as_draws_matrix()
groups <- risk.ten$groups[group.label %in% c(sel.it, sel.pa, sel.ov)]

for(i in seq_along(adm.lev)) {
  sel.it <- risk.ten$groups[adm0 == adm.lev[i] &
                            it_type != "none" & is.na(pa_type), group.label]
  sel.pa <- risk.ten$groups[adm0 == adm.lev[i] &
                            pa_type != "none" & is.na(it_type), group.label]
  sel.ov <- risk.ten$groups[adm0 == adm.lev[i] &
                            it_type != "none" & pa_type != "none", group.label]
  eff.arc <- cbind(eff.arc,
                   arc(risk.ten$full, risk.ten$it0, sel.it),
                   arc(risk.ten$full, risk.ten$pa0, sel.pa),
                   arc(risk.ten$full, risk.ten$ov0, sel.ov)) |>
             as_draws_matrix()
  eff.rrc <- cbind(eff.rrc,
                   rrc(risk.ten$full, risk.ten$it0, sel.it),
                   rrc(risk.ten$full, risk.ten$pa0, sel.pa),
                   rrc(risk.ten$full, risk.ten$ov0, sel.ov)) |>
             as_draws_matrix()
  groups <- rbind(groups, risk.ten$groups[group.label %in% c(sel.it, sel.pa, sel.ov)])
  }

rc.tenure <- list(arc = eff.arc,
                  rrc = eff.rrc,
                  groups = groups)

message(paste0("Saving outputs to `", file.effects.tenure, "` …"))
saveRDS(rc.tenure, file.effects.tenure)


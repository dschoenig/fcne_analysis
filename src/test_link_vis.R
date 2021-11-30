source("utilities.R")

# args <- commandArgs(trailingOnly = TRUE)
# region <- tolower(args[1])
# n.threads <- as.integer(args[2])

region <- "cam"




path.base <- "../"
path.effects <- paste0(path.base, "models/gam/test_effects/effects/")
file.effects <- paste0(path.effects, region, ".eff.tenure.rds")

effects <- readRDS(file.effects)
eff <- effects$ten_loc
groups <- effects$groups

groups.itpa <- groups[is.na(adm0), group.label]
summary(rrc(eff[,which(colnames(eff) %in% groups.itpa)],
            bl = "it_type.none:pa_type.none", linkinv = linkinv_cauchit),
            E, sd, median, mad, mode = ggdist::Mode, quantile2)


path.base <- "/home/schoed/scratch/fcne_analysis/"
path.effects <- paste0(path.base, "models/gam/effects/")
file.effects <- paste0(path.effects, region, ".eff.tenure.rds")

effects <- readRDS(file.effects)
eff <- effects$ten_loc
groups <- effects$groups

groups.itpa <- groups[is.na(adm0), group.label]
summary(rrc(eff[,which(colnames(eff) %in% groups.itpa)],
            bl = "it_type.none:pa_type.none", linkinv = linkinv_cauchit),
            E, sd, median, mad, mode = ggdist::Mode, quantile2)

args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(mgcv)

source("utilities.R")

region <- tolower(as.character(args[1]))
model.resp <- tolower(as.character(args[2]))

# region <- "cam"
# model.resp <- "def"

path.gam <- "../models/gam/"
file.model <- paste0(path.gam, region, ".m1.", model.resp, ".rds")
file.post <- paste0(path.gam, region, ".m1.", model.resp, ".post.rds")

file.sum <- paste0(path.gam, region, ".m1.", model.resp, ".sum.rds")

model <- readRDS(file.model)
post <- readRDS(file.post)

mod.sum <- model_summary(model, post)

p.desc <-
  data.table(p.id = 1,
             p.label = "(Intercept)",
             p.desc = "Intercept",
             p.term = "\\alpha")
sm.desc <-
  data.table(sm.id = 1:14,
             sm.label =
               c("s(ed_east,ed_north)",
                 "s(ed_east,ed_north):it_typerecognized",
                 "s(ed_east,ed_north):it_typenot_recognized",
                 "s(ed_east,ed_north):pa_typeindirect_use",
                 "s(ed_east,ed_north):pa_typedirect_use",
                 "s(ed_east,ed_north):overlaprecognized:indirect_use",
                 "s(ed_east,ed_north):overlaprecognized:direct_use",
                 "s(ed_east,ed_north):overlapnot_recognized:indirect_use",
                 "s(ed_east,ed_north):overlapnot_recognized:direct_use",
                 "s(adm0)",
                 "s(it_type,adm0)",
                 "s(pa_type,adm0)",
                 "s(pa_type,it_type,adm0)",
                 "s(som_x,som_y)"),
             sm.desc =
               c("Geographic variation (overall)",
                 "Geographic variation (IL, recognized)",
                 "Geographic variation (IL, not recognized)",
                 "Geographic variation (PA, category I-IV)",
                 "Geographic variation (PA, category V-VI)",
                 "Geographic variation (IL, rec.; PA cat. I-IV)",
                 "Geographic variation (IL, rec.; PA cat. V-VI)",
                 "Geographic variation (IL, not rec.; PA cat. I-IV)",
                 "Geographic variation (IL, not rec.; PA cat. V-VI)",
                 "Between country variation (overall)",
                 "Between country variation (IL)",
                 "Between country variation (PA)",
                 "Between country variation (IL, PA)",
                 "Variation in covariate space (SOM)"),
             sm.term =
               c("f^{\\textrm{GEO}}_0(u_i)",
                 "f^{\\textrm{GEO}}_{\\textit{rec}}(u_i)",
                 "f^{\\textrm{GEO}}_{\\textit{notrec}}(u_i)",
                 "f^{\\textrm{GEO}}_{\\textit{strict}}(u_i)",
                 "f^{\\textrm{GEO}}_{\\textit{multi}}(u_i)",
                 "f^{\\textrm{GEO}}_{\\textit{rec,strict}}(u_i)",
                 "f^{\\textrm{GEO}}_{\\textit{rec,multi}}(u_i)",
                 "f^{\\textrm{GEO}}_{\\textit{notrec,strict}}(u_i)",
                 "f^{\\textrm{GEO}}_{\\textit{notrec,multi}}(u_i)",
                 "\\gamma_{c(i)}",
                 "\\gamma_{a(i), c(i)}",
                 "\\gamma_{b(i), c(i)}",
                 "\\gamma_{a(i), b(i), c(i)}",
                 "f^{\\textrm{SOM}}(s_i)"))


term.desc <- rbind(p.desc, sm.desc, fill = TRUE)
term.desc[, term.id := 1:.N]

terms <-
  rbind(mod.sum$p.terms, mod.sum$sm.terms, fill = TRUE) |>
  merge(term.desc, sort = FALSE)

col.format <-
  c("p.est", "p.ci.l", "p.ci.u",
    "sp.est.lambda0", "sp.var.lambda0",
    "sp.est.lambda1", "sp.var.lambda1")
col.na.rm <-
  c("p.ci", "sm.k", "sm.edf")

terms[, (col.format) := lapply(.SD, format_power, mag = 0, digits = 2), .SDcols = col.format]
terms[!is.na(p.id), p.ci := paste0("[", p.ci.l, ",\\ ", p.ci.u, "]")]
terms[, sm.edf := fifelse(is.na(sm.edf),
                          NA_character_,
                          format(round(sm.edf, digits = 2), nsmall = 2))]
terms[, sm.k := as.character(sm.k)]
terms[, (col.na.rm) := lapply(.SD, fill_na_empty), .SDcols = col.na.rm] 
terms[is.na(p.id), p.ci := ""]
terms[, `:=`(term.desc = fifelse(is.na(sm.id), p.desc, sm.desc),
             term = fifelse(is.na(sm.id), p.term, sm.term))]

terms <-
  terms[, .(term.id, term.desc, term,
            p.est, p.ci,
            sm.k, sm.edf,
            sp.est.lambda0, sp.var.lambda0,
            sp.est.lambda1, sp.var.lambda1)]


model.summary <- list(terms = terms,
                      n = mod.sum$n,
                      dev.expl = mod.sum$dev.expl,
                      aic = mod.sum$aic)

saveRDS(model.summary, file.sum)


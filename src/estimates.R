source("utilities.R")
library(data.table)


format_ci <- function(point,
                      lower,
                      upper,
                      postfix.p = NULL,
                      postfix.ci = NULL,
                      digits = -2,
                      invert = FALSE) {
  point <- as.numeric(point)
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  if(invert) {
    est.inv <- lapply(list(point, lower, upper), \(x) -1*x)
    point <- est.inv[[1]]
    lower <- est.inv[[3]]
    upper <- est.inv[[2]]
  }
  p <- format(round(point, digits = digits),
              big.mark = " ", trim = TRUE,
              nsmall = max(c(0, digits)))
  ci.l <- format(round(lower, digits = digits),
                 big.mark = " ", trim = TRUE,
                 nsmall = max(c(0, digits)))
  ci.u <- format(round(upper, digits = digits),
                 big.mark = " ", trim = TRUE,
                 nsmall = max(c(0, digits)))
  if(!is.null(postfix.p)) {
    p <- paste0(p, postfix.p)
  }
  if(!is.null(postfix.ci)) {
    ci.l <- paste0(ci.l, postfix.ci)
    ci.u <- paste0(ci.u, postfix.ci)
  }
  formatted <- paste0(p, " (", ci.l, ", ", ci.u, ")")
  formatted[is.na(point)] <- ""
  return(as.character(formatted))
}

extract_labeled <- function(x, labels) {
  y <- as.character(x)
  names(y) <- labels
  return(y)
}



## Single estimates for tenure

ten.adm <- readRDS("../data/visualization/tenure_adm.no_hurr.rds")$ten.sum

ten.amz <- ten.adm$amz$mar
ten.amz[,
        `:=`(
             area.f = format_ci(area.mar.median, area.mar.hdi90l, area.mar.hdi90u,
                                postfix.p = " km^2^"),
             area.fi = format_ci(area.mar.median, area.mar.hdi90l, area.mar.hdi90u,
                                 postfix.p = " km^2^", invert = TRUE),
             mar.fi = format_ci(mar.median*100, mar.hdi90l*100, mar.hdi90u*100,
                                digits = 2, postfix.p = " %", invert = TRUE),
             cf.fi = format_ci(cf.median*100, cf.hdi90l*100, cf.hdi90u*100,
                                digits = 2, postfix.p = " %", invert = FALSE)
          )]
setorder(ten.amz, cat.label)

ten.cam <- ten.adm$cam$mar
ten.cam[,
        `:=`(
             area.f = format_ci(area.mar.median, area.mar.hdi90l, area.mar.hdi90u,
                                postfix.p = " km^2^"),
             area.fi = format_ci(area.mar.median, area.mar.hdi90l, area.mar.hdi90u,
                                 postfix.p = " km^2^", invert = TRUE),
             mar.fi = format_ci(mar.median*100, mar.hdi90l*100, mar.hdi90u*100,
                                digits = 2, postfix.p = " %", invert = TRUE),
             cf.fi = format_ci(cf.median*100, cf.hdi90l*100, cf.hdi90u*100,
                                digits = 2, postfix.p = " %", invert = FALSE)
          )]
setorder(ten.cam, cat.label)


ten.amz[dist_type == "def" & is.na(adm0),
        .(cat.label,
          area.f,
          area.fi,
          fac = round(area.fac.median),
          cf = round(area.cf.median))]

area.amz.def <-
  ten.amz[dist_type == "def" & is.na(adm0),
          extract_labeled(area.f, cat.label)]

area.amz.def.inv <-
  ten.amz[dist_type == "def" & is.na(adm0),
          extract_labeled(area.fi, cat.label)]

area.amz.deg.inv <-
  ten.amz[dist_type == "deg" & is.na(adm0),
          extract_labeled(area.fi, cat.label)]



prop.amz.def.inv <-
  ten.amz[dist_type == "def" & is.na(adm0),
          extract_labeled(mar.fi, cat.label)]

prop.cf.amz.def.inv <-
  ten.amz[dist_type == "def" & is.na(adm0),
          extract_labeled(cf.fi, cat.label)]




ten.cam[dist_type == "def" & is.na(adm0),
        .(cat.label,
          area.f,
          area.fi,
          fac = round(area.fac.median),
          cf = round(area.cf.median))]

area.cam.def <-
  ten.cam[dist_type == "def" & is.na(adm0),
          extract_labeled(area.f, cat.label)]

area.cam.def.inv <-
  ten.cam[dist_type == "def" & is.na(adm0),
          extract_labeled(area.fi, cat.label)]

ten.cam[dist_type == "def" & is.na(adm0), .(cat.label, cf.median)]

prop.cam.def.inv <-
  ten.cam[dist_type == "def" & is.na(adm0),
          extract_labeled(mar.fi, cat.label)]


prop.cf.cam.def.inv <-
  ten.cam[dist_type == "def" & is.na(adm0),
          extract_labeled(cf.fi, cat.label)]


area.amz.def.inv["IL, recognized"]
area.cam.def.inv["IL, recognized"]



area.cam.def.inv[c("IL, not recognized", "PA, category I-IV")]


areas.amz <- readRDS("../data/processed/amz.sumstats.area.rds")
ten.itpa.def.amz <-
  readRDS("../models/marginal/amz/amz.def.ten.itpa.all.rds") |>
  merge(areas.amz$undist)

ten.itpa.def.amz[, area.mar := marginal * area]

ten.itpa.def.amz[it_type == "none" &
                 pa_type %in% c("indirect_use", "direct_use") &
                 is.na(adm0),
                 .(.draw, group.id, pa_type, area.mar)] |>
dcast(.draw ~ pa_type) |>
_[, .(.draw, area.mar = as.numeric(indirect_use + direct_use))] |>
_[ ,
  c(.(area.mar.median = median(area.mar)),
       hdci2(area.mar, var.name = "area.mar."))] |>
_[, .(area.f = format_ci(area.mar.median, area.mar.hdi90l, area.mar.hdi90u,
                         postfix.p = " km^2^", invert = TRUE))]





areas.cam <- readRDS("../data/processed/cam.sumstats.area.no_hurr.rds")
ten.itpa.def.cam <-
  readRDS("../models/marginal/cam/cam.def.ten.itpa.all.no_hurr.rds") |>
  merge(areas.cam$undist)

ten.itpa.def.cam[, area.mar := marginal * area]

ten.itpa.def.cam[it_type == "none" &
                 pa_type %in% c("indirect_use", "direct_use") &
                 is.na(adm0),
                 .(.draw, group.id, pa_type, area.mar)] |>
dcast(.draw ~ pa_type) |>
_[, .(.draw, area.mar = as.numeric(indirect_use + direct_use))] |>
_[ ,
  c(.(area.mar.median = median(area.mar)),
       hdci2(area.mar, var.name = "area.mar."))] |>
_[, .(area.f = format_ci(area.mar.median, area.mar.hdi90l, area.mar.hdi90u,
                         postfix.p = " km^2^", invert = TRUE))]




ten.itpa.def.cam[it_type == "none" &
                 pa_type %in% c("indirect_use", "direct_use") &
                 is.na(adm0),
                 .(.draw, group.id, pa_type, marginal)] |>
dcast(.draw ~ pa_type) |>
_[, .(.draw, marginal = as.numeric(indirect_use + direct_use)/2)] |>
_[ ,
  c(.(mar.median = median(marginal)),
       hdci2(marginal, var.name = "mar."))] |>
_[, .(mar.fi = format_ci(mar.median*100, mar.hdi90l*100, mar.hdi90u*100,
                         digits = 2, postfix.p = " %", invert = TRUE))]

areas.amz$undist[is.na(adm0)]



# TENURE COMPARISONS


ten.comp <- readRDS("../data/visualization/tenure_comp.no_hurr.rds")$ten.sum

ten.comp.amz <- ten.comp$amz$mar


ten.comp.amz[is.na(adm0) & mar_type == "ind", .(cat.label, mar.median)]
ten.comp.amz[is.na(adm0) & mar_type == "rec", .(cat.label, mar.median)]







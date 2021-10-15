library(data.table)
library(mgcv)
library(kohonen)
library(stringi)
source("utilities.R")
options("ffbatchsize" = 1, "ffbatchbytes" = 2 * 2^30)

## Paths
path.data <- "../processed/data/"
path.som <- "../processed/som/"
path.results <- "../results/models/"
dir.create(path.results, recursive = TRUE)

## Functions
find_para <- function(x, patterns, negate = FALSE, cfun = any) {
  if(is.matrix(x)) {
    x <- dimnames(x)[[2]]
  }
  # match.l <- lapply(patterns, \(l) grepl(paste(l, collapse="|"), x))
  match.l <- lapply(patterns, \(l) stri_detect_regex(x, paste(l, collapse="|"), negate = negate))
  match.m <- do.call(rbind, match.l)
  matches <- apply(match.m, 2, cfun)
  return(which(matches))
}
sub_para <- function(x, patterns, negate = FALSE, cfun = any) {
  ids <- find_para(x, patterns, negate, cfun)
  if(is.matrix(x)) {
    return(x[,ids])
  } else {
    return(x[ids])
  }
}
rename_para <- function(x, patterns, replacements) {
  if(is.matrix(x)) {
    para <- dimnames(x)[[2]]
  } else {
    para <- x
  }
  # match.l <- lapply(patterns, \(l) grepl(paste(l, collapse="|"), x))
  para_renamed <- stri_replace_all_regex(para, patterns, replacements, vectorise_all = FALSE)
  if(is.matrix(x)) {
    colnames(x) <- para_renamed
    return(x)
  } else {
    return(para_renamed)
  }
}

cam.data <- readRDS(paste0(path.data, "cam.data.rds"))
cam.som <- readRDS(paste0(path.som, "cam.som.1e6.rds"))
cam.som.mapped <- readRDS(paste0(path.som, "cam.som.mapped"))
cam.som.xy <- data.table(cam.som$grid$pts[cam.som.mapped$unit.classif,])
cam.data[, `:=`(som_x = cam.som.xy$x, som_y = cam.som.xy$y)]

cam.mod <- as.data.frame(cam.data[, 
                                  .(id, forestloss, it_type, pa_type,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
contrasts <- list(it_type = contr.treatment(levels(cam.data$it_type)),
                  pa_type = contr.treatment(levels(cam.data$pa_type)),
                  adm0 = diag(8))
dimnames(contrasts$adm0) <- list(levels(cam.data$adm0), levels(cam.data$adm0))

pe <- model.matrix(~ it_type + pa_type + it_type:pa_type + 
                   adm0 + it_type:adm0 + pa_type:adm0 + it_type:pa_type:adm0,
                   contrasts.arg = contrasts, data = cam.mod)
cam.mod$P <- 
  rename_para(cma.mod$P,
              patterns <- c("\\(Intercept\\)", "adm0",
                            "typenot", "typerec", "typeind", "typedir"),
              replacements <- c("intercept", "adm0-",
                                "type-not", "type-rec", "type-ind", "type-dir"))


rm(cam.data)
gc()

models <- list(id = 0:2,
               response = rep("forestloss", 3),
               predictor = c(
                             "P",
                             paste("P",
                                   "s(som_x, som_y, bs = 'gp', k = 100, xt = list(max.knots = 10000))",
                                   "s(som_x, som_y, bs = 'gp', by = adm0, k = 100, xt = list(max.knots = 10000))",
                                   "s(ed_east, ed_north, bs = 'gp', k = 200, xt = list(max.knots = 10000))",
                                   sep = " + "),
                             paste("P",
                                   "s(som_x, som_y, bs = 'gp', k = 100, xt = list(max.knots = 10000))",
                                   "s(som_x, som_y, bs = 'gp', by = adm0, k = 100, xt = list(max.knots = 10000))",
                                   "s(ed_east, ed_north, bs = 'gp', k = 200, xt = list(max.knots = 10000))",
                                   sep = " + ")
                             # paste("P",
                             #       "s(som_x, som_y, bs = 'gp', k = 1000, xt = list(max.knots = 10000))",
                             #       "s(som_x, som_y, bs = 'gp', by = adm0, k = 1000, xt = list(max.knots = 10000))",
                             #       "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                                   # sep = " + ")
                            ),
               paraPen = rep(list(P = list(diag(9)), R1 = list(diag(8)), R2 = list(diag(64))), 3),
               # paraPen = rep(list(P = list(diag(81))), 3),
               drop.intercept = rep(TRUE, 3),
               link = rep("cloglog", 3),
               select = rep(TRUE, 3)
               )

set.seed(1234)

cam.fitted <- fit_models(models, cam.mod[sam,], 5e3, subset = c(2), "../results/esa/", "cam.p81", gc.level =1)


##
load_models("cam.p81.t2", "../results/esa/", summary = FALSE, rename = "cam.t2")
linkfun <- cam.t2$fam$linkfun
linkinv <- cam.t2$fam$linkinv
cam.mu <- coefficients(cam.t2)
cam.V <- cam.t2$Vc
cam.p <- fitted(cam.t2)
unload_models("cam.t2")
gc()

ndraws <- 1000
cam.post <- mvnfast::rmvn(ndraws, cam.mu, cam.V, ncores = 8)


cam.mu[find_para(names(cam.mu),list("it_rec"))]

cam.mu[c(2,7,20)]

plot(density(exp(rowSums(cam.post[,c(2,7,20)]))))
plot(density(exp(rowSums(cam.post[,c(2,12,30)]))))

plot(density(cam.post[,1]))

cam.mu[c(2,7,19)]


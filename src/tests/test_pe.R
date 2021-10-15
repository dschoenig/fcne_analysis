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
cam.mod$P <- model.matrix(~ it_type + pa_type + 
                          it_type:pa_type + adm0 + it_type:adm0 + pa_type:adm0,
                          contrasts.arg = contrasts, data = cam.mod)
# cam.mod$P <- model.matrix(~ it_type + pa_type + 
#                    it_type:pa_type + adm0 + it_type:adm0 + pa_type:adm0 + it_type:pa_type:adm0,
#              contrasts.arg = contrasts, data = cam.mod)
# Fix names
cam.mod$P <- 
  rename_para(cam.mod$P,
            patterns <- c("\\(Intercept\\)", "adm0",
                          "typenot", "typerec", "typeind", "typedir"),
            replacements <- c("intercept", "adm0-",
                              "type-not", "type-rec", "type-ind", "type-dir"))
# Remove combinations that are empty by definition
# cam.mod$P <- 
#   sub_para(cam.mod$P,  
#            patterns = list(c("BLZ", "GTM", "HND", "SLV"),
#                            "it_type-recognized"),
#            negate = TRUE,
#            cfun = any)
# cam.mod$P <- 
#   sub_para(cam.mod$P,  
#            patterns = list("MEX",
#                            "it_type-not_recognized"),
#            negate = TRUE,
#            cfun = any)
rm(cam.data)
gc()


models <- list(id = 0:2,
               response = rep("forestloss", 3),
               predictor = c(
                             "P",
                             paste("P",
                                   "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                                   sep = " + "),
                             # paste("P",
                             #       "s(som_x, som_y, bs = 'gp', k = 10, xt = list(max.knots = 10000))",
                             #       "s(som_x, som_y, bs = 'gp', by = adm0, k = 10, xt = list(max.knots = 10000))",
                             #       "s(ed_east, ed_north, bs = 'gp', k = 20, xt = list(max.knots = 10000))",
                             #       sep = " + ")
                             paste("P",
                                   "s(som_x, som_y, bs = 'gp', k = 1000, xt = list(max.knots = 10000))",
                                   "s(som_x, som_y, bs = 'gp', by = adm0, k = 1000, xt = list(max.knots = 10000))",
                                   "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                                   sep = " + ")
                            ),
               paraPen = rep(list(P = list(diag(49))), 3),
               # paraPen = rep(list(P = list(diag(81))), 3),
               drop.intercept = rep(TRUE, 3),
               link = rep("cloglog", 3),
               select = rep(TRUE, 3)
               )


sam <- sample(1:nrow(cam.mod), 1e4)

cam.fitted <- fit_models(models, cam.mod, 2e3, subset = 2, "../results/esa/", "cam.t", gc.level =1)
gc()
load_models("cam.t2", "../results/esa/", summary = FALSE)
summary(cam.t2, re.test = FALSE)

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


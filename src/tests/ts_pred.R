library(data.table)
library(mgcv)
library(kohonen)
source("utilities.R")
options("ffbatchsize" = 1, "ffbatchbytes" = 2 * 2^30)

## Paths
path.data <- "../processed/data/"
path.som <- "../processed/som/"
path.results <- "../results/models/"
dir.create(path.results, recursive = TRUE)


## MODELS ######################################################################

models <- list(id = 0:2,
               response = rep("forestloss", 3),
               predictor = c(
                             "P",
                             paste("P",
                                   "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                                   sep = " + "),
                             paste("P",
                                   "s(som_x, som_y, bs = 'gp', k = 10, xt = list(max.knots = 10000))",
                                   "s(som_x, som_y, bs = 'gp', by = adm0, k = 10, xt = list(max.knots = 10000))",
                                   "s(adm0, bs = 're')",
                                   "s(ed_east, ed_north, bs = 'gp', k = 20, xt = list(max.knots = 10000))",
                                   sep = " + ")
                             # paste("P",
                             #       "s(som_x, som_y, bs = 'gp', k = 1000, xt = list(max.knots = 10000))",
                             #       "s(som_x, som_y, bs = 'gp', by = adm0, k = 1000, xt = list(max.knots = 10000))",
                             #       "s(adm0, bs = 're')",
                             #       "s(ed_east, ed_north, bs = 'gp', k = 2000, xt = list(max.knots = 10000))",
                             #       sep = " + ")
                            ),
               paraPen = rep(list(P = list(diag(9))), 3),
               drop.intercept = rep(TRUE, 3),
               link = rep("cloglog", 3),
               select = rep(TRUE, 3)
               )


cam.data <- readRDS(paste0(path.data, "cam.data.rds"))
cam.som <- readRDS(paste0(path.som, "cam.som.1e6.rds"))
cam.som.mapped <- readRDS(paste0(path.som, "cam.som.mapped"))

cam.som.xy <- data.table(cam.som$grid$pts[cam.som.mapped$unit.classif,])
cam.data[, `:=`(som_x = cam.som.xy$x, som_y = cam.som.xy$y)]
cam.mod <- as.data.frame(cam.data[, 
                                  .(id, forestloss, it_type, pa_type,
                                  som_x, som_y, ed_east, ed_north, adm0)
                                  ])
cam.mod$P <- model.matrix(~ it_type * pa_type, data = cam.mod)
# cam.mod$it_adm <- model.matrix(~ it_type:adm0 - 1, data = cam.mod)[, -seq(1, 24, by = 3)]
cam.mod$adm <- model.matrix(~ adm0 - 1, data = cam.mod)
rm(cam.data, cam.som, cam.som.mapped)

tm <- bam(forestloss ~  
          # it_type + pa_type + 
          P + 
          # it_adm +
          s(som_x, som_y, bs = "gp", k = 10, xt = list(max.knots = 10000)) +
          s(som_x, som_y, bs = "gp", by = adm0, k = 10, xt = list(max.knots = 10000)) +
          s(adm0, P, bs = "re") + 
          # adm +
          # s(it_type, adm0, bs = "re") +
          # s(pa_type, adm0, bs = "re") +
          s(ed_east, ed_north, bs = "gp", k = 20, xt = list(max.knots = 10000)),
          data = cam.mod[1:1e4,],
          family = binomial(link = "cloglog"),
          drop.intercept = TRUE,
          select = TRUE,
          discrete = TRUE,
          nthreads = c(2,1),
          paraPen = list(P = list(diag(9)))
          # paraPen = list(P = list(diag(9)),
          #                it_adm = list(diag(16)),
          #                adm = list(diag(8)))
          )

summary(tm, re.test = FALSE)

re <- tm$smooth[[10]]
re


tm2 <- bam(forestloss ~  it_type * pa_type + 
          s(som_x, som_y, bs = "gp", k = 10, xt = list(max.knots = 10000)) +
          s(som_x, som_y, bs = "gp", by = adm0, k = 10, xt = list(max.knots = 10000)) +
          s(adm0, bs = "re") + 
          s(ed_east, ed_north, bs = "gp", k = 20, xt = list(max.knots = 10000)),
          data = cam.mod[1:1e4,],
          family = binomial(link = "cloglog"),
          drop.intercept = TRUE,
          select = TRUE,
          discrete = TRUE,
          nthreads = c(2,1),
          paraPen = list(it_type = list(diag(3)), pa_type = list(diag(3)))
          )

predict(tm, cam.mod[1:10,], discrete = TRUE, type = "response")
fitted(tm)[1:10]


load_models("amz.m2", summary = FALSE)

p <- predict(amz.m2, amz.mod[1:2,], discrete = FALSE, type = "response")
lp <- predict(amz.m2, amz.mod[1:2,], discrete = FALSE, type = "lpmatrix")
f <- fitted(amz.m2)[1:2]
o <- as.numeric(amz.mod$forestloss[1:2])

p
f
o
linkinv <- amz.m2$fam$linkinv
amz.mod$it_type

post <- mvnfast::rmvn(1000, coef(amz.m2), amz.m2$Vc, ncores = 8)

pred <- lp %*% t(post)

plot(density(pred[1,]))

linkinv(max(density(pred[1,])$x))
linkinv(rowMeans(pred))

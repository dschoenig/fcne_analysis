library(data.table)
library(mgcv)
library(kohonen)
library(mvnfast)
source("utilities.R")

## Paths
path.data.int <- "../data/intermediate/"
path.data.proc <- "../data/processed/"
path.som <- "../models/som/"
path.gam <- "../models/gam/"
dir.create(path.gam, recursive = TRUE)


# ## AMAZON ######################################################################


# amz.data <- readRDS(paste0(path.data.int, "amz.data.rds"))
# amz.som <- readRDS(paste0(path.som, "amz.som.1e6.rds"))
# amz.som.mapped <- readRDS(paste0(path.data.int, "amz.som.mapped"))

# amz.som.xy <- data.table(amz.som$grid$pts[amz.som.mapped$unit.classif,])
# amz.data[, `:=`(som_x = amz.som.xy$x, som_y = amz.som.xy$y)]
# amz.data[pa_type != "none" & it_type != "none", overlap := paste(it_type, pa_type, sep = ":")]
# amz.data[is.na(overlap), overlap := "none"]
# amz.data[, overlap := factor(overlap,
#                              levels = c("none",
#                                         "recognized:indirect_use",
#                                         "recognized:direct_use",
#                                         "not_recognized:indirect_use",
#                                         "not_recognized:direct_use"),
#                              ordered = TRUE)]
# amz.data[,
#          `:=`(it_type = as.ordered(it_type),
#               pa_type = as.ordered(pa_type))]
# # saveRDS(amz.data, paste0(path.data.proc, "amz.data.proc.rds"))

# amz.mod <- as.data.frame(amz.data)
# amz.mod$b0 <- model.matrix(~ 1, amz.mod)


# k = 1000
# max.knots = 10000
# # Penalized intercept
# system.time({
# amz.m3 <-
#   bam(forestloss ~ -1 +
#       b0 +
#       s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
#       s(ed_east, ed_north, bs = 'gp', by = it_type, k = k, xt = list(max.knots = max.knots)) +
#       s(ed_east, ed_north, bs = 'gp', by = pa_type, k = k, xt = list(max.knots = max.knots)) +
#       s(ed_east, ed_north, bs = 'gp', by = overlap, k = k, xt = list(max.knots = max.knots)) +
#       s(som_x, som_y, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
#       s(som_x, som_y, bs = 'gp', by = adm0, k = k, xt = list(max.knots = max.knots)) +
#       s(it_type, pa_type, adm0, bs = "re"),
#       family = binomial(link = "cloglog"),
#       data = amz.mod,
#       drop.intercept = FALSE,
#       select = TRUE,
#       paraPen = list(b0 = list(diag(1))),
#       chunk.size = 5e3,
#       discrete = TRUE,
#       nthreads = c(2,1),
#       gc.level = 0
#       )
# })
# saveRDS(amz.m3, paste0(path.gam, "amz.m3.rds"))

# rm(amz.m3, amz.mod)
# gc()

## CENTRAL AMERICA #############################################################

cam.data <- readRDS(paste0(path.data.int, "cam.data.rds"))
cam.som <- readRDS(paste0(path.som, "cam.som.1e6.rds"))
cam.som.mapped <- readRDS(paste0(path.data.int, "cam.som.mapped"))

cam.som.xy <- data.table(cam.som$grid$pts[cam.som.mapped$unit.classif,])
cam.data[, `:=`(som_x = cam.som.xy$x, som_y = cam.som.xy$y)]
cam.data[pa_type != "none" & it_type != "none", overlap := paste(it_type, pa_type, sep = ":")]
cam.data[is.na(overlap), overlap := "none"]
cam.data[, overlap := factor(overlap,
                             levels = c("none",
                                        "recognized:indirect_use",
                                        "recognized:direct_use",
                                        "not_recognized:indirect_use",
                                        "not_recognized:direct_use"),
                             ordered = TRUE)]
cam.data[,
         `:=`(it_type = as.ordered(it_type),
              pa_type = as.ordered(pa_type))]
# saveRDS(cam.data, paste0(path.data.proc, "cam.data.proc.rds"))

cam.mod <- as.data.frame(cam.data)
cam.mod$b0 <- model.matrix(~ 1, cam.mod)

k = 1000
max.knots = 10000
# Penalized intercept
system.time({
cam.m3 <-
  bam(forestloss ~ -1 +
      b0 +
      s(ed_east, ed_north, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(ed_east, ed_north, bs = 'gp', by = it_type, k = k, xt = list(max.knots = max.knots)) +
      s(ed_east, ed_north, bs = 'gp', by = pa_type, k = k, xt = list(max.knots = max.knots)) +
      s(ed_east, ed_north, bs = 'gp', by = overlap, k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', k = k, xt = list(max.knots = max.knots)) +
      s(som_x, som_y, bs = 'gp', by = adm0, k = k, xt = list(max.knots = max.knots)) +
      s(it_type, pa_type, adm0, bs = "re"),
      family = binomial(link = "cloglog"),
      data = cam.mod,
      drop.intercept = FALSE,
      select = TRUE,
      paraPen = list(b0 = list(diag(1))),
      chunk.size = 5e3,
      discrete = TRUE,
      # nthreads = c(2,1),
      nthreads = 8,
      gc.level = 0
      )
})

saveRDS(cam.m3, paste0(path.gam, "cam.m3.rds"))
# rm(cam.m3, cam.mod)


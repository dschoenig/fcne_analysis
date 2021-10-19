library(RPostgres)
library(DBI)
library(data.table)

## Folders
path.raw <- "../data/raw/"
path.int <- "../data/intermediate/"

## AMAZON ######################################################################

amz.vars <- fread(paste0(path.raw, "amz.vars.csv"), 
                  na.strings = "",
                  key = "id")

amz.vars[, 
         `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
              it = ifelse(it == "t", TRUE, FALSE),
              it_type = factor(it_type, levels = c("none", "recognized", "not_recognized")),
              pa = ifelse(pa == "t", TRUE, FALSE),
              pa_type = factor(pa_type, levels = c("none", "indirect_use", "direct_use")),
              adm0 = factor(adm0)
              )
         ]
amz.vars[is.na(it_type), it_type := "none"]
amz.vars[is.na(pa_type), pa_type := "none"]

amz.data <- na.omit(amz.vars, cols = c("dist_set", "dist_roads", "dist_rivers", "slope", "adm0"))
saveRDS(amz.data, paste0(path.int, "amz.data.rds"))
rm(amz.data, amz.vars)

## CENTRAL AMERICA #############################################################

cam.vars <- fread(paste0(path.raw, "cam.vars.csv"), 
                  na.strings = "",
                  key = "id")

cam.vars[, 
         `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
              it = ifelse(it == "t", TRUE, FALSE),
              it_type = factor(it_type, levels = c("none", "recognized", "not_recognized")),
              pa = ifelse(pa == "t", TRUE, FALSE),
              pa_type = factor(pa_type, levels = c("none", "indirect_use", "direct_use")),
              adm0 = factor(adm0)
              )
         ]
cam.vars[is.na(it_type), it_type := "none"]
cam.vars[is.na(pa_type), pa_type := "none"]

cam.data <- na.omit(cam.vars, cols = c("dist_set", "dist_roads", "dist_rivers", "slope", "adm0"))
saveRDS(cam.data, paste0(path.int, "cam.data.rds"))
rm(cam.data, cam.vars)

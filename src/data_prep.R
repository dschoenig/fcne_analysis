library(RPostgres)
library(DBI)
library(data.table)

## Output folder
path.proc <- "../processed/data/"

## AMAZON ######################################################################

## Get data

amz.vars <- fread("../data/amz.vars.csv", 
                  na.strings = "",
                  key = "id")

amz.vars[, 
         `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
              it = ifelse(it == "t", TRUE, FALSE),
              it_type = factor(it_type, levels = c("none", "recognized", "not_recognized")),
              pa = ifelse(pa == "t", TRUE, FALSE),
              pa_type = factor(pa_type, levels = c("none", "indirect_use", "direct_use"))
              )
         ]
amz.vars[is.na(it_type), it_type := "none"]
amz.vars[is.na(pa_type), pa_type := "none"]

amz.data <- na.omit(amz.vars, cols = c("dist_set", "dist_roads", "dist_rivers", "slope"))
saveRDS(amz.data, paste0(path.proc, "amz.data.rds"))


## CENTRAL AMERICA #############################################################

## Get data

cam.vars <- fread("../data/cam.vars.csv", 
                  na.strings = "",
                  key = "id")

cam.vars[, 
         `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
              it = ifelse(it == "t", TRUE, FALSE),
              it_type = factor(it_type, levels = c("none", "recognized", "not_recognized")),
              pa = ifelse(pa == "t", TRUE, FALSE),
              pa_type = factor(pa_type, levels = c("none", "indirect_use", "direct_use"))
              )
         ]
cam.vars[is.na(it_type), it_type := "none"]
cam.vars[is.na(pa_type), pa_type := "none"]

cam.data <- na.omit(cam.vars, cols = c("dist_set", "dist_roads", "dist_rivers", "slope"))
saveRDS(cam.data, paste0(path.proc, "cam.data.rds"))

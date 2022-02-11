library(data.table)

## Folders
path.raw <- "../data/raw/"
path.int <- "../data/intermediate/"
path.proc <- "../data/processed/"

n.fit <- 1e7
n.val <- 2e6

regions <- c("amz", "cam")


for(i in 1:length(regions)) {

  file.data.raw <- paste0(path.raw, regions[i], ".vars.csv")
  file.areas.it <- paste0(path.raw, regions[i], ".indterr.csv")
  file.areas.it_pts <- paste0(path.raw, regions[i], ".indterr_pts.csv")
  file.areas.pa <- paste0(path.raw, regions[i], ".pareas.csv")
  file.areas.pa_pts <- paste0(path.raw, regions[i], ".pareas_pts.csv")
  file.areas.out <- paste0(path.proc, regions[i], ".areas.it_pa.rds")
  file.data.fit.int <- paste0(path.int, regions[i], ".data.fit.int.rds")
  file.data.val.int <- paste0(path.int, regions[i], ".data.val.int.rds")

  vars <- fread(file.data.raw, 
                    na.strings = "",
                    key = "id")

  vars[, 
           `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
                it = ifelse(it == "t", TRUE, FALSE),
                it_type = factor(it_type,
                                 levels = c("none", "recognized", "not_recognized"),
                                 ordered = TRUE),
                pa = ifelse(pa == "t", TRUE, FALSE),
                pa_type = factor(pa_type,
                                 levels = c("none", "indirect_use", "direct_use"),
                                 ordered = TRUE),
                adm0 = factor(adm0)
                )
           ]
  vars[is.na(it_type), it_type := "none"]
  vars[is.na(pa_type), pa_type := "none"]
  vars[pa_type != "none" & it_type != "none",
               overlap := paste(it_type, pa_type, sep = ":")]
  vars[is.na(overlap), overlap := "none"]
  vars[, overlap := factor(overlap,
                               levels = c("none",
                                          "recognized:indirect_use",
                                          "recognized:direct_use",
                                          "not_recognized:indirect_use",
                                          "not_recognized:direct_use"),
                               ordered = TRUE)]

  # Remove samples with incomplete covariate information and cut sample to
  # 12 million
  data.int <- na.omit(vars)[1:(n.fit + n.val),]

  # Randomly assign samples to fit and validation set
  set.seed(18470611)
  sam <- sample(1:nrow(data.int), nrow(data.int), replace = FALSE)
  data.fit.int <- data.int[sam[1:n.fit],]
  data.val.int <- data.int[sam[(n.fit+1):(n.fit + n.val)],]

  length(which(data.val.int$id %in% data.fit.int$id)) == 0

  saveRDS(data.fit.int, file.data.fit.int)
  saveRDS(data.val.int, file.data.val.int)

  
  areas.it_pts <- fread(file.areas.it_pts)
  areas.it <- fread(file.areas.it)
  
  areas.it_pts <- areas.it_pts[pt_id %in% data.fit.int$id,
                               .(ids = list(pt_id)),
                               it_id]

  areas.it <- areas.it[it_id %in% areas.it_pts$it_id]
  areas.it[landmark_cat %in% c("acknowledged, documented", "acknowledged, not documented"),
           it_type := "recognized"]
  areas.it[landmark_cat %in% c("not acknowledged, formal land claim submitted", 
                               "not acknowledged, customary tenure", "indicative areas"),
           it_type := "not_recognized"]
  
  areas.it <- merge(areas.it, areas.it_pts, by = "it_id")


  areas.pa_pts <- fread(file.areas.pa_pts)
  areas.pa <- fread(file.areas.pa)
  
  areas.pa_pts <- areas.pa_pts[pt_id %in% data.fit.int$id,
                               .(ids = list(pt_id)),
                               pa_id]

  areas.pa <- areas.pa[pa_id %in% areas.pa_pts$pa_id]
  areas.pa[iucn_cat %in% c("Ia", "Ib", "II", "III", "IV"),
           pa_type := "indirect_use"]
  areas.pa[iucn_cat %in% c("V", "VI"),
           pa_type := "direct_use"]
  
  areas.pa <- merge(areas.pa, areas.pa_pts, by = "pa_id")
 

  areas.it_pa <- rbind(areas.it[, !"landmark_cat"], areas.pa[, !"iucn_cat"],
                       fill = TRUE)

  areas.it_pa[,
              `:=`(it_type = factor(it_type,
                                    levels = c("none", "recognized", "not_recognized"),
                                    ordered = TRUE),
                   pa_type = factor(pa_type,
                                    levels = c("none", "indirect_use", "direct_use"),
                                    ordered = TRUE))]

  areas.it_pa$area.id <- seq.int(nrow(areas.it_pa))
  setnames(areas.it_pa, c("it_id", "pa_id"), c("it.id", "pa.id"))
  setcolorder(areas.it_pa, c("area.id", "it.id", "pa.id", "adm0", "it_type", "pa_type", "ids"))
  setkey(areas.it_pa, "area.id")

  saveRDS(areas.it_pa, file.areas.out)

  rm(data.int, data.fit.int, data.val.int, vars,
     areas.it, areas.it_pts, areas.pa, areas.pa_pts, areas.it_pa)

}

library(data.table)

## Folders
path.raw <- "../data/raw/"
path.int <- "../data/intermediate/"

regions <- c("amz", "cam")

for(i in 1:length(regions)) {
  file.data.raw <- paste0(path.raw, regions[i], ".vars.csv")
  file.data.int <- paste0(path.int, regions[i], ".data.int.rds")

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

  data.int <- na.omit(vars, cols = c("dist_set", "dist_roads", "dist_rivers", "slope", "adm0"))
  saveRDS(data.int, file.data.int)
  rm(data.int, vars)
}


library(data.table)

## Folders
path.raw <- "../data/raw/"
path.int <- "../data/intermediate/"

n.fit <- 1e7
n.val <- 2e6

regions <- c("amz", "cam")


for(i in 1:length(regions)) {
  file.data.raw <- paste0(path.raw, regions[i], ".vars.csv")
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
  rm(data.int, data.fit.int, data.fit.val, vars)
}

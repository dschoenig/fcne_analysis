
args <- commandArgs(trailingOnly = TRUE)
library(mgcv)
library(data.table)
library(ggplot2)

source("utilities.R")


region <- tolower(as.character(args[1]))
model_resp <- tolower(as.character(args[2]))

# region <- "cam"
# model_resp <- "def"


path.base <- "../"
path.som <- paste0(path.base, "models/som/")
path.gam <- paste0(path.base, "models/gam/")
path.agg <- paste0(path.base, "models/gam/")
path.data.proc <- paste0(path.base, "data/processed/")
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")


file.som <- paste0(path.som, region, ".som.1e6.rds")
file.model <- paste0(path.gam, region, ".m1.", model_resp, ".rds")
file.post <- paste0(path.gam, region, ".m1.", model_resp, ".post.rds")
file.data <- paste0(path.data.proc, region, ".data.fit.proc.rds")

file.agg <- paste0(path.agg, region, ".", model_resp, ".cov.rds")

## EVALUATE LINEAR PREDICTOR BASED ON DRAWS FROM MODEL POSTERIOR ###############

# Load model, posterior draws, and data
som <- readRDS(file.som)
gam <- readRDS(file.model)
post <- readRDS(file.post)
data <- readRDS(file.data)

som.sm.id <- which(sapply(gam$smooth, \(x) x$label) == "s(som_x,som_y)")
som.par <- with(gam$smooth[[som.sm.id]], first.para:last.para)

som.pred <- unique(data[, .(som_x, som_y, som_bmu)])[order(som_x, som_y)]

cov.names <- names(som$scale$mean)
cov.map <- get_mapping(som, prefix = "som_", bmu.name = "bmu")

mat.som <- PredictMat(gam$smooth[[som.sm.id]], cov.map)
post.som <- t(post[, som.par])

lp.som.mat <- mat.som %*% as.matrix(post.som)
rm(mat.som, post.som)
colnames(lp.som.mat) <- 1:ncol(lp.som.mat)
lp.som.dt <- as.data.table(lp.som.mat)
lp.som.dt[, som_bmu := cov.map$som_bmu]

lp.som <-
  melt(lp.som.dt,
       id.vars = "som_bmu",
       variable.name = ".draw", value.name = "lp")
rm(lp.som.dt)





n.quant <- c(20, 50, 100)

cov.sum.n.l <- list()

# for(q in seq_along(n.quant)) {
#   cov.sum.l <- list()
#   for(i in seq_along(cov.names)) {
#     cov.foc <- cov.names[i]
#     cov.foc.bin <- paste0(cov.foc, ".bin")
#     cov.env <- list(cov.col = cov.foc,
#                     cov.bin.col = cov.foc.bin)
#     cov.quant <- seq(1/n.quant[q], 1, length.out = n.quant[q])
#     cov.breaks <- c(min(cov.map[[cov.foc]], na.rm = TRUE),
#                     unique(unname(quantile(cov.map[[cov.foc]], probs = cov.quant))))
#     cov.map.foc <- cov.map[, .(som_bmu, cov.col), env = cov.env]
#     cov.map.foc[,
#                 cov.bin.col := cov.breaks[cut(cov.col, cov.breaks, labels = FALSE, include.lowest = TRUE)],
#                 env = cov.env]
#     cov.sum.l[[i]] <-
#       merge(lp.som, cov.map.foc) |>
#       _[, .(lp.bin = mean(lp)), by = .(.draw, cov.bin.col), env = cov.env] |>
#       _[,
#         c(.(cov = cov.foc,
#             lp.median = median(lp.bin)),
#           hdci2(lp.bin, var.name = "lp.")),
#         by = cov.foc.bin]
#     setnames(cov.sum.l[[i]], cov.foc.bin, "cov.val")
#   }
#   cov.sum.n.l[[q]] <- rbindlist(cov.sum.l)
#   cov.sum.n.l[[q]][, n.quant := n.quant[q]]
#   rm(cov.sum.l); gc()
# }
# cov.sum.n <- rbindlist(cov.sum.n.l)
# setcolorder(cov.sum.n, c("n.quant", "cov", "cov.val"))


n.quant <- c(20, 50, 100)
cov.sum.n.l <- list()

for(q in seq_along(n.quant)) {
  cov.sum.l <- list()
  for(i in seq_along(cov.names)) {
    cov.foc <- cov.names[i]
    cov.foc.bin <- paste0(cov.foc, ".bin")
    cov.env <- list(cov.col = cov.foc,
                    cov.bin.col = cov.foc.bin)
    cov.quant <- seq(1/n.quant[q], 1, length.out = n.quant[q])
    cov.breaks <- c(min(cov.map[[cov.foc]], na.rm = TRUE),
                    unique(unname(quantile(cov.map[[cov.foc]], probs = cov.quant))))
    cov.map.foc <- cov.map[, .(som_bmu, cov.col), env = cov.env]
    cov.map.foc[,
                cov.bin.col := cov.breaks[cut(cov.col, cov.breaks, labels = FALSE, include.lowest = TRUE)],
                env = cov.env]
    cov.sum.l[[i]] <-
      merge(lp.som, cov.map.foc) |>
      _[, .(lp.bin = mean(lp)), by = .(.draw, cov.bin.col), env = cov.env]
    cov.sum.l[[i]][, cov := cov.foc]
    setnames(cov.sum.l[[i]], cov.foc.bin, "cov.val")
  }
  cov.sum.n.l[[q]] <- rbindlist(cov.sum.l)
  cov.sum.n.l[[q]][, n.quant := n.quant[q]]
  rm(cov.sum.l); gc()
}
cov.sum.n <- rbindlist(cov.sum.n.l)
setcolorder(cov.sum.n, c("n.quant", "cov", "cov.val"))

saveRDS(cov.sum.n, file.agg)

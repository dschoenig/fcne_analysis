args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(data.table)
library(posterior)
library(bayesplot)
library(ggplot2)

source("utilities.R")

path.base <- "../"
path.gam <- paste0(path.base, "models/gam/")
path.data.proc <- paste0(path.base, "data/processed/")
path.pred <- paste0(path.base, "models/gam/ppc/cam1e5/")
path.pdf <- paste0(path.base, "models/gam/ppc/cam1e5/")

region <- tolower(as.character(args[1]))
region <- "cam"

file.gam <- paste0(path.gam, region, ".m3.rds")
file.post <- paste0(path.gam, region,  ".m3.post.rds")
file.data.fit <- paste0(path.data.proc, region, ".data.fit.proc.rds")
file.data.val <- paste0(path.data.proc, region, ".data.val.proc.rds")

if(!dir.exists(path.pdf))
  dir.create(path.pdf, recursive = TRUE)

n.postsam <- 1000
predict.chunk <- 500
post.chunk <- 200

# # Testing
# n.postsam <- 100
# predict.chunk <- 50000
# post.chunk <- 100

n.threads <- 1

## EVALUATE LINEAR PREDICTOR BASED ON DRAWS FROM MODEL POSTERIOR ###############

# Load model, posterior draws, and data
gam <- readRDS(file.gam)
post <- readRDS(file.post)
data.fit <- readRDS(file.data.fit)
data.val <- readRDS(file.data.val)


# Data for simulation

# Set 1: random points, from data used for fitting
s1.dat <- data.fit[1:1e5,]

# Set 2: random points, from data not used for fitting
s2.dat <- data.val[1:1e5,]

# set 3: random points, by 10km
res <- 1e4
corner <- c(min(data.fit$ed_east), min(data.fit$ed_north))
corner <- floor(corner / res) * res
bins.10km <- bin_cols(data.fit, c("ed_east", "ed_north"),
                      rep(res, 2), corner,
                      bin.names = c("ed_east.10km", "ed_north.10km"))

s3.dat <- cbind(data.fit, do.call(cbind, bins.10km))
s3.dat[, spid.10km := .GRP, by = c("ed_east.10km", "ed_north.10km")]

grp.10km <- s3.dat[, .(n = .N), spid.10km][n > 100, spid.10km]
s3.dat <- s3.dat[spid.10km %in% sample(grp.10km, 10)]

# # set 4: random points, by 25km
# res <- 2.5e4
# corner <- c(min(data.fit$ed_east), min(data.fit$ed_north))
# corner <- floor(corner / res) * res
# bins.25km <- bin_cols(data.fit, c("ed_east", "ed_north"),
#                       rep(res, 2), corner,
#                       bin.names = c("ed_east.25km", "ed_north.25km"))

# s4.dat <- cbind(data.fit, do.call(cbind, bins.25km))
# s4.dat[, spid.25km := .GRP, by = c("ed_east.25km", "ed_north.25km")]

# grp.25km <- s4.dat[, .(n = .N), spid.25km][n > 100, spid.25km]
# s4.dat <- s4.dat[spid.25km %in% sample(grp.25km, 10)]

eval.fit <- data.fit[id %in% unique(c(s1.dat$id, s3.dat$id))]
eval.val <- s2.dat

a <- Sys.time()
pred.fit <-
  evaluate_posterior(model = gam,
                     posterior = post[1:n.postsam,],
                     newdata = eval.fit,
                     id.col = "id",
                     predict.chunk = predict.chunk,
                     post.chunk = post.chunk,
                     type = "response",
                     progress = TRUE)
b <- Sys.time()
b-a

saveRDS(pred.fit, paste0(path.pred, "pred.fit.rds"))

a <- Sys.time()
pred.val <-
  evaluate_posterior(model = gam,
                     posterior = post[1:n.postsam,],
                     newdata = eval.val,
                     id.col = "id",
                     predict.chunk = predict.chunk,
                     post.chunk = post.chunk,
                     type = "response",
                     progress = TRUE)
b <- Sys.time()
b-a

saveRDS(pred.val, paste0(path.pred, "pred.val.rds"))

rm(gam)

# pred.fit <- readRDS(paste0(path.pred, "pred.fit.rds"))
# pred.val <- readRDS(paste0(path.pred, "pred.val.rds"))
# s2.dat <- data.val[id %in% colnames(pred.val)]

# path.pred <- paste0(path.base, "models/gam/ppc/cam1e5/previous/")
# pred.fit <- readRDS(paste0(path.pred, "pred.fit.rds"))
# pred.val <- readRDS(paste0(path.pred, "pred.val.rds"))

yrep.fit <- as_draws_matrix(apply(pred.fit, 2, \(x) rbinom(length(x), 1, x)))
yrep.val <- as_draws_matrix(apply(pred.val, 2, \(x) rbinom(length(x), 1, x)))

rm(pred.fit, pred.val)

# Test 1: Tenure categories and countries (fitting data) #######################

g1.fit.it_pa <-
  s1.dat |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type"))
g1.fit.it <-
  s1.dat[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type"))
g1.fit.pa <-
  s1.dat[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("pa_type"))
g1.fit.adm_it_pa <-
  s1.dat |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type"))
g1.fit.adm_it <-
  s1.dat[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type"))
g1.fit.adm_pa <-
  s1.dat[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "pa_type"))

g1.fit <- rbindlist(list(g1.fit.it_pa,
                         g1.fit.it,
                         g1.fit.pa,
                         g1.fit.adm_it_pa,
                         g1.fit.adm_it,
                         g1.fit.adm_pa), fill = TRUE)
g1.fit$group.id <- 1:nrow(g1.fit)
setorder(g1.fit, adm0, it_type, pa_type)
setcolorder(g1.fit, c("group.id", "group.label", "adm0", "it_type", "pa_type"))
g1.fit <- g1.fit[n>=10]


id.list <- g1.fit$ids
names(id.list) <- g1.fit$group.label

yrep.g1.prop <-
  aggregate_variables(
          predictions = yrep.fit,
           fun = mean,
           ids = id.list,
           agg.size = 1e5,
           n.threads = n.threads,
           progress = TRUE
  )


yobs.g1.prop <- NA
for(i in seq_along(id.list)) {
  yobs.g1.prop[i] <- data.fit[id %in% id.list[[i]], mean(forestloss)]
}
names(yobs.g1.prop) <- names(id.list)

n.bins <- 25

pdf(paste0(path.pdf, "ppc_s1.pdf"), 11, 8.5)
sel.g <- which(is.na(g1.fit$adm0))
pp <- ppc_stat_grouped(yobs.g1.prop[sel.g],
                 yrep.g1.prop[,sel.g],
                 as.factor(g1.fit$group.label[sel.g]),
                 stat = "identity",
                 binwidth = \(x) abs(max(x) - min(x)) / n.bins) +
  guides(fill = guide_legend(order = 1, title = bquote(italic(T) == "prop")))
print(pp)
lev.adm0 <- levels(g1.fit$adm0)
for(i in seq_along(lev.adm0)) {
  sel.g <- which(g1.fit$adm0 == lev.adm0[i])
  pp <- ppc_stat_grouped(yobs.g1.prop[sel.g],
                   yrep.g1.prop[,sel.g],
                   as.factor(g1.fit$group.label[sel.g]),
                   stat = "identity",
                   binwidth = \(x) abs(max(x) - min(x)) / n.bins) +
    guides(fill = guide_legend(order = 1, title = bquote(italic(T) == "prop")))
  print(pp)
}
dev.off()


# Test 2: Tenure categories and countries (validation data) ####################

g2.val.it_pa <-
  s2.dat |>
  ids_by_group(id.col = "id", group.vars = c("it_type", "pa_type"))
g2.val.it <-
  s2.dat[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("it_type"))
g2.val.pa <-
  s2.dat[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("pa_type"))
g2.val.adm_it_pa <-
  s2.dat |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type", "pa_type"))
g2.val.adm_it <-
  s2.dat[it_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "it_type"))
g2.val.adm_pa <-
  s2.dat[pa_type != "none"] |>
  ids_by_group(id.col = "id", group.vars = c("adm0", "pa_type"))

g2.val <- rbindlist(list(g2.val.it_pa,
                         g2.val.it,
                         g2.val.pa,
                         g2.val.adm_it_pa,
                         g2.val.adm_it,
                         g2.val.adm_pa), fill = TRUE)
g2.val$group.id <- 1:nrow(g2.val)
setorder(g2.val, adm0, it_type, pa_type)
setcolorder(g2.val, c("group.id", "group.label", "adm0", "it_type", "pa_type"))
g2.val <- g2.val[n>=10]


id.list <- g2.val$ids
names(id.list) <- g2.val$group.label

yrep.g2.prop <-
  aggregate_variables(
          predictions = yrep.val,
           fun = mean,
           ids = id.list,
           agg.size = 1e5,
           n.threads = n.threads,
           progress = TRUE
  )


yobs.g2.prop <- NA
for(i in seq_along(id.list)) {
  yobs.g2.prop[i] <- data.val[id %in% id.list[[i]], mean(forestloss)]
}
names(yobs.g2.prop) <- names(id.list)


n.bins <- 25

pdf(paste0(path.pdf, "ppc_s2.pdf"), 11, 8.5)
sel.g <- which(is.na(g2.val$adm0))
pp <- ppc_stat_grouped(yobs.g2.prop[sel.g],
                 yrep.g2.prop[,sel.g],
                 as.factor(g2.val$group.label[sel.g]),
                 stat = "identity",
                 binwidth = \(x) abs(max(x) - min(x)) / n.bins) +
  guides(fill = guide_legend(order = 1, title = bquote(italic(T) == "prop")))
print(pp)
lev.adm0 <- levels(g2.val$adm0)
for(i in seq_along(lev.adm0)) {
  sel.g <- which(g2.val$adm0 == lev.adm0[i])
  pp <- ppc_stat_grouped(yobs.g2.prop[sel.g],
                   yrep.g2.prop[,sel.g],
                   as.factor(g2.val$group.label[sel.g]),
                   stat = "identity",
                   binwidth = \(x) abs(max(x) - min(x)) / n.bins) +
    guides(fill = guide_legend(order = 1, title = bquote(italic(T) == "prop")))
  print(pp)
}
dev.off()

# Test 3: Map quadrants (10km2) ################################################



g3.fit <-
  ids_by_group(s3.dat, id.col = "id", group.vars = c("ed_east.10km", "ed_north.10km"))

g3.fit$group.id <- 1:nrow(g3.fit)
setorder(g3.fit, ed_east.10km, ed_north.10km)
setcolorder(g3.fit, c("group.id", "group.label", "ed_east.10km", "ed_north.10km"))
g3.fit <- g3.fit[n>=10]

id.list <- g3.fit$ids
names(id.list) <- g3.fit$group.label

yrep.g3.prop <-
  aggregate_variables(
          predictions = yrep.fit,
          fun = mean,
          ids = id.list,
          agg.size = 1e5,
          n.threads = n.threads,
          progress = TRUE
  )


yobs.g3.prop <- NA
for(i in seq_along(id.list)) {
  yobs.g3.prop[i] <- data.fit[id %in% id.list[[i]], mean(forestloss)]
}
names(yobs.g3.prop) <- names(id.list)

n.bins <- 25

pdf(paste0(path.pdf, "ppc_s3.pdf"), 11, 8.5)
pp <- ppc_stat_grouped(yobs.g3.prop,
                 yrep.g3.prop,
                 as.factor(g3.fit$group.label),
                 stat = "identity",
                 binwidth = \(x) abs(max(x) - min(x)) / n.bins) +
  guides(fill = guide_legend(order = 1, title = bquote(italic(T) == "prop")))
print(pp)
dev.off()



# Test 4: Difference between tenure categories #################################

# Study region
sel.g <- which(is.na(g2.val$adm0))
yrep.g4.rr <- rr(yrep.g2.prop[,sel.g], "it_type.none:pa_type.none") - 1

summary(yrep.g4.rr)

id.list <- g2.val[is.na(adm0), ids]
names(id.list) <- g2.val[is.na(adm0), group.label]

yobs.g4.rr <- NA
for(i in seq_along(id.list)) {
  yobs.g4.rr[i] <- data.val[id %in% id.list[[i]], mean(forestloss)]
}
names(yobs.g4.rr) <- names(id.list)
yobs.g4.rr <- (yobs.g4.rr / yobs.g4.rr["it_type.none:pa_type.none"]) - 1

n.bins <- 25

pdf(paste0(path.pdf, "ppc_s4.pdf"), 11, 8.5)
pp <- ppc_stat_grouped(yobs.g4.rr,
                 yrep.g4.rr,
                 as.factor(g2.val[is.na(adm0), group.label]),
                 stat = "identity") +
                 # binwidth = \(x) abs(max(x) - min(x)) / n.bins) +
  guides(fill = guide_legend(order = 1, title = bquote(italic(T) == "RR")))
print(pp)
dev.off()

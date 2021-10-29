library(mgcv)

source("utilities.R")


# Theoretical test

data <- gamSim(6, n = 10000)
data$id <- sample(1:nrow(data), nrow(data))
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) + s(fac, bs = "re"), data = data)
summary(mod)

model = mod
newdata = NULL
id.col = NULL
obs = NULL
predict.chunk = NULL
progress = TRUE

lpm <- lp_matrix(mod, newdata = data, id.col = "id", predict.chunk = 10, marginals = NULL)


post <- rmvn(n = 1000, mu = coef(mod), V = vcov(mod, unconditional = TRUE))

splits <- list(
               full = 1:41,
               x0 = grep("x0", names(coef(mod))),
               x1 = grep("x1", names(coef(mod)))
               )
system.time({
lp <- evaluate_posterior(mod, post, data, id.col = "id", predict.chunk = NULL, marginals = NULL,
                         post.chunk = 250)
})

lp[sample(1:1e7, 1e6)] <- -120
lp[sample(1:1e7, 1e6)] <- 120

sam.id <- sample(colnames(lp), 1000)

clamp <- lims

predictions <- lp

lims <- link_cll(c(.Machine$double.eps, 1-.Machine$double.eps))

summarize_predictions(lp, ids = sam.id, n.cores = 8, clamp = c(0,10))
summarize_predictions(lp, ids = sam.id, n.cores = 8)

x <- lp

lp.dt <- lapply(lp, \(x) {x.dt <- as.data.table(t(x))
                          colnames(x.dt) <- paste0("draw.", colnames(x.dt))
                          x.dt$id <- as.integer(colnames(x))
                          return(x.dt)})
lp.dt <- rbindlist(lp.dt, idcol = "marginal")
setcolorder(lp.dt, c("marginal", "id"))


lp.dt <- as.data.table(t(lp))
colnames(lp.dt) <- paste0("draw.", colnames(lp.dt))
lp.dt$id <- as.integer(colnames(lp))
setcolorder(lp.dt, "id")

lp.dt[,1:4]

library(arrow)

write_parquet(lp.dt, "test.parquet")

M <- matrix(rnorm(5e5), nrow=5e4, ncol=1e3)

object.size(M) / 2^30

lp.ds <- open_dataset("test.parquet")

predictions <- lp.ds
fun = mean
ids = NULL
id.col = NULL
draw.prefix = "draw"
draw.chunk = NULL
n.cores = 8

ds <- open_dataset("../tmp/lp/cam.lp/cam.lp-2.parquet")

(summarize_predictions(ds, clamp = c(-.5, 10)))

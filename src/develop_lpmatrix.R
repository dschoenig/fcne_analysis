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

lpm <- lp_matrix(mod, newdata = data, id.col = "id", predict.chunk = 10, marginals = splits)


post <- rmvn(n = 1000, mu = coef(mod), V = vcov(mod, unconditional = TRUE))

splits <- list(
               full = 1:41,
               x0 = grep("x0", names(coef(mod))),
               x1 = grep("x1", names(coef(mod)))
               )
system.time({
lp <- evaluate_posterior(mod, post, data, id.col = "id", predict.chunk = NULL, marginals = splits,
                         post.chunk = 250)
})


lp.dt <- lapply(lp, \(lp) {lp.dt <- as.data.table(t(lp))
                          colnames(lp.dt) <- paste0("draw.", colnames(lp.dt))
                          lp.dt$id <- as.integer(colnames(lp))
                          return(lp.dt)})
lp.dt <- rbindlist(lp.dt, idcol = "marginal")
setcolorder(lp.dt, c("marginal", "id"))

lp.dt[,1:4]

M <- matrix(rnorm(5e5), nrow=5e4, ncol=1e3)

object.size(M) / 2^30

read




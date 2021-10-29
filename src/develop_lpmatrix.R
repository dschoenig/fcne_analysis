library(mgcv)

source("utilities.R")


# Theoretical test

data <- gamSim(6, n = 1000)
data$id <- sample(1:nrow(data), nrow(data))
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) + s(fac, bs = "re"), data = data)
summary(mod)

model = mod
newdata = NULL
id.col = NULL
obs = NULL
predict.chunk = NULL
progress = TRUE

lpm <- lp_matrix(mod, newdata = data, id.col = "id", predict.chunk = 10)

head(lpm)

post <- rmvn(n = 1000, mu = coef(mod), V = vcov(mod, unconditional = TRUE))

pred <- evaluate_posterior(mod, post)
pred_sum <- summarize_predictions_by(pred, data, fun = mean, group.vars = "fac")

summary(pred_sum - mean(with(data, f0+f1+f2+f3)))
# Means should be around 3, 6, 9, 12

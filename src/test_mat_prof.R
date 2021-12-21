library(data.table)
library(mgcv)
source("utilities.R")

## Paths
path.data.proc <- "../data/processed/"

model.reg <- "cam"
file.data.proc <- paste0(path.data.proc, model.reg, ".data.fit.proc.rds")


data.proc <- readRDS(file.data.proc)


# Several runs

rho <- c(0.03, seq(1, 100, 1), seq(200, 1000, 100)) * 1e3
n.rep <- 10

prof.mat <- list()
for(i in 1:n.rep) {
  message(paste0("Replicate ", i, " / ", n.rep, "…"))
  sam <- sample(1:nrow(data.proc), 1e5)
  x <- as.matrix(data.proc[sam, .(ed_east, ed_north)])
  y <- data.proc[sam, forestloss]
  prof <- profile_matern(y, x,  m = 3, k = 200, rho = rho,
                         family = binomial(link = cauchit),
                         discrete = TRUE, nthreads = c(2,1))
  prof.mat[[i]] <- cbind(prof, sam = i)
}

prof.mat <- as.data.table(do.call(rbind, prof.mat))
prof.mat$sam <- as.factor(prof.mat$sam)

# saveRDS(prof.mat, "prof.mat1.rds")
prof.mat <- readRDS("prof.mat1.rds")


prof.mat[, .SD[which.min(REML_score)], sam]
prof.mat[, .SD[which.min(AIC)], sam]
prof.mat[, .SD[which.min(REML_score)], sam][,mean(rho)]
prof.mat[, .SD[which.min(AIC)], sam][,mean(rho)]


mod.s <- gam(REML_score ~ s(rho, k = 100, bs = "ts") + s(sam, bs = "re"),
             data = prof.mat, method = "REML")
REML.s <- data.frame(rho = seq(0, 1e6, 1000))
pred <- predict(mod.s, cbind(REML.s, sam = 1), exclude = "s(sam)",
                se.fit = TRUE, unconditional = TRUE)
REML.s$REML_score <- pred[[1]]
REML.s$REML_score.cl <- pred[[1]] - 1.96 * pred[[2]]
REML.s$REML_score.cu <- pred[[1]] + 1.96 * pred[[2]]

ggplot(prof.mat, aes(x = rho, y = REML_score)) +
  geom_line(aes(group = sam), colour = 4, alpha = 0.5)+
  geom_line(data = REML.s, size = 1) +
  geom_ribbon(data = REML.s, aes(ymin = REML_score.cl, ymax = REML_score.cu), alpha = 0.35) +
  xlim(1, 1e5)

REML.s[which.min(REML.s$REML_score),]

prof.mat[, `:=`(d.REML_score = (REML_score - shift(REML_score)) / (rho - shift(rho)),
                d.AIC = (AIC - shift(AIC)) / (rho - shift(rho))), sam]

mod.s <- gam(d.REML_score ~ s(rho, k = 100, bs = "ts") + s(sam, bs = "re"),
             data = prof.mat, method = "REML")
d.REML.s <- data.frame(rho = seq(0, 1e6, 1000))
pred <- predict(mod.s, cbind(d.REML.s, sam = 1), exclude = "s(sam)",
                se.fit = TRUE, unconditional = TRUE)
d.REML.s$d.REML_score <- pred[[1]]
d.REML.s$d.REML_score.cl <- pred[[1]] - 1.96 * pred[[2]]
d.REML.s$d.REML_score.cu <- pred[[1]] + 1.96 * pred[[2]]

d.REML.s[20:30,]

ggplot(prof.mat, aes(x = rho, y = d.REML_score)) +
  geom_line(aes(group = sam), colour = "blue", alpha = 0.2)+
  geom_line(data = d.REML.s) +
  geom_ribbon(data = d.REML.s, aes(ymin = d.REML_score.cl, ymax = d.REML_score.cu), alpha = 0.5) +
  xlim(1, 1e5)


source("utilities.R")
library(data.table)
library(mgcv)
options("ffbatchsize" = 1, "ffbatchbytes" = 2 * 2^30)

amz.m2.post <- readRDS("../results/models/amz.m2.post.rds")
sim_residuals("amz.m2", posterior = amz.m2.post, 
              n.threads = 8, on.disk = TRUE, storage.mode = "boolean") 
# sim_residuals("amz.m2", posterior = FALSE, 
#               n.threads = 8, on.disk = TRUE, storage.mode = "boolean") 
rm(amz.m2.post)

cam.m2.post <- readRDS("../results/models/cam.m2.post.rds")
sim_residuals("cam.m2", posterior = cam.m2.post,
              n.threads = 8, on.disk = TRUE, storage.mode = "boolean") 
# sim_residuals("cam.m2", posterior = FALSE,
#               n.threads = 8, on.disk = TRUE, storage.mode = "boolean") 
rm(cam.m2.post)


#---

amz <- readRDS("../data/amz.rds")
amz[pa == FALSE, `:=`(parea = "none")] 
amz[pa == TRUE & pa_use_ind, `:=`(parea = "indirect")] 
amz[pa == TRUE & pa_use_dir, `:=`(parea = "direct")] 
amz[it == FALSE, `:=`(indter = "none")] 
amz[it == TRUE & it_rec, `:=`(indter = "recognized")] 
amz[it == TRUE & it_notrec, `:=`(indter = "notrecognized")] 
amz$parea <- factor(amz$parea, levels = c("none", "indirect", "direct"))
amz$indter <- factor(amz$indter, levels = c("none", "recognized", "notrecognized"))
amz.mod <- as.data.frame(na.omit(amz[, 
                                     .(id, forestloss, parea, indter,
                                     som.x, som.y, ed_east, ed_north, adm0)
                                     ]))
amz.mod$P <- model.matrix(~ parea * indter, amz.mod)


load_models("amz.1e7.m2", "../results/tests/samsize/", summary = FALSE, rename = "amz.m2")
amz.m2.post <- mvnfast::rmvn(1000, mu = coefficients(amz.m2), sigma = amz.m2$Vc,
                             kpnames = TRUE, ncores = 8) 


system.time({
amz.m2.res <- quantile_residuals(amz.m2, obs = 1:5000, n.sim = 1000, posterior = amz.m2.post,
                                   row.chunk = 1e3, post.chunk = 200, on.disk = TRUE,
                                   n.threads = 8)
})

eval <- eval_post(amz.m2, amz.m2.post, obs = 1:100, on.disk = F, n.threads = 8)

fam<-fix.family.rd(amz.m2$family)

fam$rd(fam$linkinv(eval[[1]]), 1, 1)

eval2 <- eval_post(model = amz.m2, posterior = amz.m2.post, obs = 1:2000, coef = inc)

data <- model.frame(amz.m2)

indrec <- sample((which(data$adm0 == "ECU" & data$P[,4] == 1)), 5000)
ecu <- sample(which(data$adm0 == "ECU" & rowSums(data$P[,2:9]) == 0), 5000)

e_indrec <- eval_post(amz.m2, amz.m2.post, obs = indrec, post.chunk = 1000, n.threads = 8, on.disk = F)
e_ecu <- eval_post(amz.m2, amz.m2.post, obs = ecu, post.chunk = 1000, n.threads = 8, on.disk = F)

d <- density(e_ecu[[1]])
plot(d)


means <- colMeans(e_indrec[[1]]) - colMeans(e_ecu[[1]])

exp(quantile(means, c(0.025, 0.975)))

d <- density(e_indrec[[1]])
plot(d)


means1 <- means


mean(eval2[[1]])

which(colMeans(eval2[[1]]) == 1)

prot <- which(data$P[,2] == 1)

diff <- colMeans(eval2[[1]]) - colMeans(eval[[1]])

d <- density(eval2[[1]])
plot(d)

coef(amz.m2)[1]

apply(eval[[2]], 2, mean)


x <- ff(1:72, dim=c(8,9))
x
str(x)

update(x, ffap

inc <- which(!grepl("s(som.x,som.y)", names(coefficients(amz.m2)), fixed = T))
which(grepl("adm0ECU", names(coefficients(amz.m2)), fixed = T))

coefficients(amz.m2)[10000:10008]

data$adm0

length(coefficients(amz.m2))

?grep


save_residuals(amz.m2.res, "../results/samsize/amz.1e6.m2.res", rootpath = tempdir())

diag_residuals(amz.m2, amz.m2.res)

min(fitted(amz.m2))

library(pROC)

rr <- roc(amz.m2$model[,1], fitted(amz.m2))
ggroc(rr)


library(moranfast)

n <- 1e5
mor <- moranfast(amz.m2.res$quantile_residuals[1:n], amz.mod$ed_north[1:n], amz.mod$ed_east[1:n])
mor


amz.data <- as.data.table(model.frame(amz.m2))

map_res = 5000

amz.res <- st_transform(st_as_sf(amz.data[, .(ed_east, ed_north)],
                                  crs = "ESRI:102032", coords = c("ed_east", "ed_north")),
                         "ESRI:102033") 
amz.res[,c("east", "north")] <- as.data.frame(st_coordinates(amz.res$geometry))
amz.res <- as.data.table(amz.res)
# amz.res$fitted <- fitted(amz.m2)
amz.res$residuals <- amz.m2.res$quantile_residuals

amz.coord_bins <- bin_cols(amz.res, c("east", "north"), c(map_res, map_res), c(-2163e3, 1331e3))
amz.res[, `:=`(east.bin = amz.coord_bins$east.bin,
                  north.bin = amz.coord_bins$north.bin)]

amz.res.agg <- amz.res[,
                       .(residuals.m = mean(residuals),
                         # fitted.m = mean(fitted),
                         nobs = .N),
                       by = .(east.bin, north.bin)]

bg_map <- st_read("~/projects/data/source/gadm/3.6/world/gadm36_levels.gpkg",
                  layer = "level0",
                  query = "SELECT * FROM level0 
                           WHERE GID_0 IN ('ABW', 'AIA', 'ARG', 'ATG', 'BES', 
                                           'BHS', 'BLM', 'BLZ', 'BOL', 'BRA', 
                                           'BRB', 'CHL', 'COL', 'CRI', 'CUB', 
                                           'CUW', 'CYM', 'DMA', 'DOM', 'ECU', 
                                           'FLK', 'GLP', 'GRD', 'GTM', 'GUF', 
                                           'GUY', 'HND', 'HTI', 'JAM', 'KNA', 
                                           'LCA', 'MAF', 'MEX', 'MSR', 'MTQ', 
                                           'NIC', 'PAN', 'PER', 'PRI', 'PRY', 
                                           'SGS', 'SLV', 'SUR', 'SXM', 'TCA', 
                                           'TTO', 'UMI', 'URY', 'VCT', 
                                           'VEN', 'VGB', 'VIR', 'XCL')")

amz.bg_map <- st_transform(bg_map, "ESRI:102033")
# pan.bg_map <- st_transform(bg_map, "EPSG:32617")

amz.residuals.p <- ggplot(amz.res.agg) +
  geom_sf(data = amz.bg_map, fill = "grey75", colour = "grey65", size = 0.3) +
  # geom_sf(data = amz.bg_map[amz.bg_map$GID_0 == "PAN",],
  #         fill = "#F7F7F7", colour = "NA") +
  # geom_raster(mapping = aes(fill = nobs
                            # , alpha = nobs
  #                           , x = east.bin, y = north.bin)) +
  # geom_raster(mapping = aes(x = east.bin, y = north.bin), fill = "#F7F7F7F7") +
  geom_raster(mapping = aes(fill = residuals.m
                            # , alpha = nobs
                            , x = east.bin, y = north.bin)) +
  # geom_sf(data = amz.bg_map, fill = NA, colour = "grey65", size = 0.3) +
  # geom_sf(data = amz.lim_it, fill = NA, size = 0.6, colour = group_colours[1]) +
  # geom_sf(data = amz.lim_pa, fill = NA, size = 0.6, colour = group_colours[2]) +
  # geom_sf(data = amz.lim_it, fill = NA, size = 0.6) +
  # geom_sf(data = amz.lim_pa, fill = NA, size = 0.6, linetype = "dashed") +
  # scale_fill_distiller(type = "div", palette = "RdBu", direction = -1, limits = c(0,1)) +
  # scale_fill_gradientn(colours = divcolours) +
  scale_fill_viridis_c() +
  scale_alpha(
              range = c(0.5,1)
              , trans = "log"
              # , limits = c(1, 10)
              ) +
  coord_sf(expand = FALSE) +
  xlim(range(amz.res.agg$east.bin)[1] - map_res, range(amz.res.agg$east.bin)[2] + map_res) +
  ylim(range(amz.res.agg$north.bin)[1] - map_res, range(amz.res.agg$north.bin)[2] + map_res) +
  guides(alpha = FALSE) +
  labs(fill = "Quantile residuals", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey97", colour = NA),
        panel.grid = element_line(colour = "grey82"))

amz.residuals.p












str(amz.m2.res)


hist(qres_amz.m2$quantile_residuals)

fm <- bam(forestloss ~ P + 
          s(som.x, som.y, bs = 'gp', k = 100) +
          s(som.x, som.y, bs = 'gp', by = adm0, k = 100) +
          s(adm0, bs = 're') +
          s(ed_east, ed_north, bs = 'gp', k = 100),
          data = amz.mod[sample(1:nrow(amz.mod), 1e5),],
          family = binomial(link = "cloglog"),
          paraPen = list(P = list(diag(9))),
          select = TRUE,
          drop.intercept = TRUE,
          discrete = TRUE,
          nthreads = c(2,1),
          chunk.size = 1e5)

library(mvnfast)
posterior_fm <- rmvn(1000, mu = coefficients(fm), sigma = fm$Vc,
                        kpnames = TRUE, ncores = 8) 


load_models("amz.1e7.m2", "../results/tests/samsize/", summary = FALSE, rename = "model")
posterior_model <- rmvn(1000, mu = coefficients(model), sigma = model$Vc,
                        kpnames = TRUE, ncores = 8) 


system.time({
sim_fm <- simulate_post(fm, posterior_fm, obs = 1:2000, on.disk = TRUE, row.chunk = 1e3, post.chunk = 200, n.threads = 1)
})

system.time({
sim_model <- simulate_post(model, posterior_model, newdata = amz.mod[1:10000,], on.disk = TRUE, row.chunk = 1e3, post.chunk = 200, n.threads = 8)
})


source("utilities.R")

system.time({
  qres_fm <- quantile_residuals(fm, obs = 1:10, n.sim = 10, posterior = TRUE,
                                row.chunk = 1e4, post.chunk = 2, on.disk = TRUE,
                                n.threads = 8)
})

system.time({
  qres_model <- quantile_residuals(model, n.sim = 1000, obs = 1:2000, posterior = posterior_model,
                                   row.chunk = 1e3, post.chunk = 200, on.disk = TRUE,
                                   n.threads = 8)
})


system.time({
Xp <- predict(model, 
              amz.mod[1:1000,],
              type = "lpmatrix",
              newdata.guaranteed = TRUE,
              n.threads = 8,
              discrete = F)
})

system.time({
Xp %*% t(posterior_model)
})


system.time({
Xp %*% t(posterior_model[1:200,])
Xp %*% t(posterior_model[201:400,])
Xp %*% t(posterior_model[401:600,])
Xp %*% t(posterior_model[601:800,])
Xp %*% t(posterior_model[801:1000,])
})




mf <- model.frame(model)

require(mvnfast)
system.time({
posterior_model <- rmvn(1000, mu = coefficients(model), sigma = model$Vc, kpnames = TRUE, ncores = 8) 
})

mf$forestloss[1]

system.time({
  n <- 1000
  ps.res <- numeric(n)
  rs.res <- numeric(n)
  for(i in 1:n) {
    ps <- simulate_post(model = model, data = mf[1,],posterior = posterior, chunk.size = 1e3, progress = FALSE)
    ps.l <- sum(ps < 0)/1000
    ps.u <- sum(ps <= 0)/1000
    if (ps.l == ps.u) res <- ps.l else res <- runif(1, ps.l, ps.u)
    ps.res[i] <- res
    rs <- rbinom(1000, 1, fitted(model)[1])
    rs.l <- sum(rs < 0)/1000
    rs.u <- sum(rs <= 0)/1000
    if (rs.l == rs.u) res <- rs.l else res <- runif(1, rs.l, rs.u)
    rs.res[i] <- res
  }
})

hist(ps.res)
hist(rs.res)
hist(ps.res-rs.res)

hist(diff)

sum(ps)

sum(rs)

system.time({simulate(model,1)})

pred <- predict(model, amz.mod[1,], type = "lpmatrix", discrete = FALSE) 

model$terms






simulate_posterior
system.time({
  postSim(amz.mod, 1)
})

amz.mod$Vc

qres <- quantile_residuals(amz.mod, n.sim = 10, posterior = TRUE, post.chunk.size = 1e4, n.cores = 8, progress = TRUE)


A <- matrix(0.1, 1e6, 200)
object.size(A) / 1e6


library(mgcViz)

postSim


model <- fm

simulate_post <- function(model, posterior, chunk.size = 1e4, progress = TRUE, on.disk = FALSE) {
  if(is.null(dim(posterior))) {
    posterior <- matrix(posterior, ncol = length(posterior))
  } 
  m <- nrow(posterior)
  X <- model.frame(model)
  n <- nobs(model)
  fam <- fix.family.rd(model$family)
  weights <- model$prior.weights
  scale <- model$sig2
  from <- seq(1, n, chunk.size)
  if(length(from) > 1) {
    to <- c(from[2:length(from)]-1, n)
  } else {
    to <- chunk.size
  }
  sim <- matrix(nrow = n, ncol = m)
  if(progress) {
    prog <- txtProgressBar(min = 0, max = length(from), initial = 0,
                          char = "=", width = NA, title = "Progress", style = 3)
  }
  for(i in 1:length(from)) {
    Xp <- predict(model, 
                  X[from[i]:to[i],],
                  type = "lpmatrix",
                  discrete = T)
    lp <- Xp %*% t(posterior)
    str(Xp)     
    attr(Xp)
    coefficients(model) 
    str(X)
    sim[from[i]:to[i],] <- apply(fam$linkinv(lp), 2, fam$rd, 
                                 wt = weights[from[i]:to[i]],  scale = scale)
    if(progress) {
      setTxtProgressBar(prog, i)
    }
  }
  close(prog)
  return(sim)
}



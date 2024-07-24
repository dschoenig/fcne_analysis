source("utilities_egp.R")


## MAPPING HELPERS ##############################################################

crs.ea <- 
  list(cam = st_crs('PROJCS["Central_America_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900002"]]'),
       amz = st_crs('PROJCS["Amazon_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",-5.59],PARAMETER["longitude_of_center",-62.05],PARAMETER["standard_parallel_1",3.81],PARAMETER["standard_parallel_2",-15.62],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900004"]]'))


bin_cols <- function(data, columns, bin.res, bin.min = NULL, bin.round = NULL, bin.names = NULL, append = FALSE, right = TRUE) {
  bins.l <- list()
  for (i in 1:length(columns)) {
    if(is.null(bin.min)) {
      c.min <- min(data[[columns[i]]])
    } else {
      c.min <- bin.min[i]
    }
    c.max <- max(data[[columns[i]]])
    if(is.null(bin.round)) {
      b.lower <- c.min - bin.res[i]
      b.upper <- c.max + bin.res[i]
    } else {
      b.lower <- round(c.min, bin.round) - bin.res[i]
      b.upper <- round(c.max, bin.round) + bin.res[i]
    }
    b.breaks <- seq(from = b.lower, 
                    to =  b.upper, 
                    by = bin.res[i])
    b.center <- b.breaks[1:(length(b.breaks)-1)] + bin.res[i] / 2 
    cuts <- cut(data[[columns[i]]], breaks = b.breaks, labels = FALSE, right = right)
    bins.l[[i]] <- b.center[cuts]
  }
  if(is.null(bin.names)) {
    names(bins.l) <- paste0(columns, ".bin")
  } else {
    names(bins.l) <- bin.names
  }
  if(append) {
    return(cbind(data, do.call(cbind, bins.l)))
  } else {
    return(bins.l)
  }
}



## PLOTTING HELPERS #############################################################

label_arc <- function(x, ndec = 0, psign = TRUE) {
  per <- format(round(100 * x, ndec), nsmall = ndec, trim = TRUE)
  per[which(x > 0)] <- paste0("+", per[which(x > 0)])
  if(psign) {
    per <- paste0(per, "%")
  }
  per[is.na(x)] <- NA
  return(per)
}

label_per <- function(x, ndec = 0, psign = TRUE) {
  per <- format(round(100 * x, ndec), nsmall = ndec, trim = TRUE)
  if(psign) {
    per <- paste0(per, "%")
  }
  per[is.na(x)] <- NA
  return(per)
}





# require(mgcv, quietly = TRUE)
# require(data.table, quietly = TRUE)
# require(posterior, quietly = TRUE)
# require(doParallel, quietly = TRUE)
# require(arrow, quietly = TRUE)
# require(dplyr, quietly = TRUE)

# require(igraph) # needed only for Kaski-Lagus error (SOM)
# require(ggplot2)
# require(patchwork)


#cloglog <- function(mu) {
#  log(-log(1 - mu))
#}

#inv_cloglog <- function(eta) {
#  pmax(pmin(-expm1(-exp(eta)), 1 - .Machine$double.eps), .Machine$double.eps)
#}

## link_cauchit <- function(mu) {
##   qcauchy(mu)
## }

## linkinv_cauchit <- function (eta) {
##     thresh <- -qcauchy(.Machine$double.eps)
##     eta <- pmin(pmax(eta, -thresh), thresh)
##     pcauchy(eta)
## }

#logit <- function(mu) {
#  -log(1 / mu - 1)
#}

#inv_logit <- function(eta) { 
#  1 / (1 + exp(-eta))
#}

#arc <- function(x, ...) {
#  UseMethod("arc", x)
#}

#arc.draws_matrix <- function(x,
#                             ref,
#                             select = NULL,
#                             trans = identity) {
#  if(is.null(select)){
#    select <- intersect(colnames(x), colnames(ref))
#  }
#  arc <- trans(x[, select]) - trans(ref[, select])
#  return(arc)
#}


#rrc <- function(x, ...) {
#  UseMethod("rrc", x)
#}

#rrc.draws_matrix <- function(x,
#                             ref,
#                             select = NULL,
#                             trans = identity) {
#  if(is.null(select)){
#    select <- intersect(colnames(x), colnames(ref))
#  }
#  rrc <- (trans(x[, select]) - trans(ref[, select])) / trans(ref[, select])
#  return(rrc)
#}

#Mode <- function(x, na.rm = FALSE) {
#  # Based on ggdist::Mode
#  if (na.rm) {
#    x = x[!is.na(x)]
#  }
#  else if (anyNA(x)) {
#    return(NA_real_)
#  }
#  if (is.integer(x)) {
#    ux = unique(x)
#    mod <- ux[which.max(tabulate(match(x, ux)))]
#  } else {
#    d = density(x, cut = 0)
#    mod <- d$x[which.max(d$y)]
#  }
#  return(mod)
#}

#hdi <- function(x, mass = 0.95) {
#  hdi.l <- lapply(mass, \(mass) HDInterval::hdi(x, mass))
#  hdi <- do.call(c, hdi.l)
#  names(hdi) <- paste(rep(paste0("hdi", mass*100), each = 2),
#                 names(hdi),
#                 sep = ".")
#  return(hdi)
#}

## model_overview <- function(models, path = "../results/models/", prefix = "") {
##   # models: Character vector containing the names of the models to be loaded.
##   n <- length(models)
##   modres <- vector(mode = "list", length = n)
##   for(i in 1:n) {
##     fname <- paste0(path, prefix, models[i], ".rds")
##     mod <- readRDS(fname)
##     modres[[i]]$id <- models[i]
##     modres[[i]]$df <- nobs(mod) - df.residual(mod)
##     modres[[i]]$aic <- AIC(mod)
##     modres[[i]]$file <- fname
##     rm(mod)
##   }
##   return(rbindlist(modres))
## }


## sim_residuals <- function(models, path = "../results/models/", prefix = "", ...){
##   # models: Character vector containing the names of the models for which
##   #   residuals are to be simulated.
##   simulated <- vector(mode = "list", length = length(models))
##   for(i in 1:length(models)){
##     simulated[[i]]$id <- models[i]
##     simulated[[i]]$residuals <- FALSE
##     print(paste0("Simulating residuals for model ", models[i],
##                  " (", i, " of ", length(models), ")"))
##     modfit <- readRDS(paste0(path, prefix, models[i], ".rds"))
##     modres <- quantile_residuals(modfit, ...)
##     simulated[[i]]$residuals <- TRUE
##     save_residuals(residuals = modres, file = paste0(path, prefix, models[i], ".res"),
##                    rootpath = tempdir())
##     for (i in 1:length(modres$simulations)) {
##       delete(modres$simulations[[i]])
##     }
##     rm(modfit, modres)
##     gc()
##   }
##   return(simulated)
## }


## quantile_residuals <- function(model,
##                                n.sim = 1000, 
##                                sim.chunk = 200,
##                                obs = "all",
##                                posterior = TRUE, 
##                                row.chunk = 1e3, 
##                                on.disk = TRUE,
##                                storage.mode = "double",
##                                n.threads = 1,
##                                seed = NULL,
##                                progress = TRUE
##                                ) {

##   # Set RNG
##   if (!is.null(seed)) {
##     if (exists(".Random.seed")) {
##       prev.random.state <- .Random.seed
##     } else {
##       prev.random.state <- NULL
##     }
##     set.seed(seed)
##   }

##   if(is.numeric(obs)) {
##     n <- length(obs)
##     observed.response <- model.frame(model)[obs,1] 
##   } else {
##     n <- nobs(model)
##     observed.response <- model.frame(model)[,1] 
##   }
  
##   if(n.sim < sim.chunk) sim.chunk <- n.sim
 
##   sim.from <- seq(1, n.sim, sim.chunk)
##   if(length(sim.from) > 1) {
##     sim.to <- c(sim.from[2:length(sim.from)]-1, n.sim)
##   } else {
##     sim.to <- n.sim
##   }
##   sim.chunks <- sim.to - c(0, sim.to[-length(sim.to)])

##   # Simulate response and fill matrices
##   if(all(posterior == TRUE) | is.matrix(posterior)) {
##     if(is.matrix(posterior)) {
##       post <- posterior[1:n.sim,]
##     } else {
##       post <- mvnfast::rmvn(n = n.sim, mu = coefficients(model), 
##                             sigma = vcov(model, unconditional = TRUE),
##                             ncores = n.threads)
##       if(progress) print(paste0("Generated ", n.sim, " random draws from model posterior."))
##     }
##     if(progress) print("Simulating response ...")
##     simulations <- sim_post(model = model,
##                                  posterior = post,
##                                  obs = obs,
##                                  row.chunk = row.chunk,
##                                  post.chunk = sim.chunk, 
##                                  progress = progress, 
##                                  on.disk = on.disk,
##                                  storage.mode = storage.mode,
##                                  n.threads = n.threads)
##   } else {
##     simulations <- list()
##     if(progress) {
##       print("Simulating response ...")
##       prog <- txtProgressBar(min = 0, max = n.sim, initial = 0,
##                             char = "=", width = NA, title = "Progress", style = 3)
##     }
##     fam <- fix.family.rd(model$family)
##     wt <- model$prior.weights
##     scale <- model$sig2
##     fv <- fitted(model)
##     if(is.numeric(obs)) {
##       fv <- fv[obs]
##       wt <- wt[obs]
##     }
##     for (s in 1:length(sim.from)) {
##       if(on.disk) {
##         if(is.null(storage.mode)) storage.mode <- vmode(fitted(model))
##         simulations[[s]] <- ff(dim = c(n, sim.chunks[s]), vmode = storage.mode, 
##                                    pattern = paste0(tempdir(), "/qres"), finalizer = "delete")
##         for (i in 1:sim.chunks[s]) {
##           simulations[[s]][,i] <- fam$rd(fv, wt, scale)
##           if(progress) {
##             setTxtProgressBar(prog, sim.from[s] -1 + i)
##           }
##         } # end loop over columns
##       } else {
##         if(is.null(storage.mode)) storage.mode <- class(fitted(model))[1]
##         simulations[[s]] <- matrix(nrow = n, ncol = sim.chunks[s])
##         for (i in 1:sim.chunks[s]) {
##           simulations[[s]][,i] <- as(fam$rd(fv, wt, scale), storage.mode)
##           if(progress) {
##             setTxtProgressBar(prog, sim.from[s] -1 + i)
##           }
##         } # end loop over columns
##       } # end on.disk == FALSE
##     } # end loop over nodes
##     if(progress) close(prog)
##   }

##   if(progress) print("Calculating residuals ...")
    
##   sim.lower <- sim.upper <- quantile.residuals <- rep(0, n)
  
##   # Compute quantile residuals based on probability integral transform
##   # Same method (but different implementations) as in the DHARMa package
##   if(on.disk) {
##     for (m in 1:length(simulations)) {
##       sims <- simulations[[m]]
##       sim.lower <- sim.lower +
##         ffcolapply(rowSums(sims[,i1:i2] < observed.response),
##                               X=sims, RETURN = TRUE, CFUN = "csum") / n.sim
##       sim.upper <- sim.upper + 
##         ffcolapply(rowSums(sims[,i1:i2] <= observed.response),
##                                X=sims, RETURN = TRUE, CFUN = "csum") / n.sim
##     }
##   } else {
##     for (m in 1:length(simulations)) {
##      sims <- simulations[[m]]
##      sim.lower <- sim.lower + rowSums(apply(sims, MARGIN = 2,
##                                             function(x) x < observed.response)) / n.sim
##      sim.upper <- sim.upper + rowSums(apply(sims, MARGIN = 2,
##                                             function(x) x <= observed.response)) / n.sim
##     }
##   }

##   quantile_residuals <- mapply(function(lower, upper) { 
##                                if (lower == upper) lower else runif(1, lower, upper)},
##                                sim.lower, sim.upper, SIMPLIFY = TRUE, USE.NAMES = FALSE)

##   # Restore RNG
##   if (!is.null(seed)) {
##     .Random.seed <- prev.random.state
##   }

##   res <- list(n = n, seed = seed, simulations = simulations, quantile_residuals = quantile_residuals)
##   return(res)
## }

## save_residuals <- function(residuals, file, rootpath = getOption("fftempdir")) {
##   if(is.ff(residuals$simulations[[1]])){
##     ffsave_list(residuals$simulations, file = paste0(file, ".sim"), rootpath = rootpath)
##   }
##   saveRDS(residuals, file = paste0(file, ".rds"))
## }

## load_residuals <- function(models, path = "../results/models/", prefix = "", simulations = TRUE, overwrite = FALSE, env = .GlobalEnv){
##   # models: Character vector containing the names of the models for which
##   #   residuals are to be loaded.
##   for(i in 1:length(models)) {
##     rname <- paste0(prefix, models[i], ".res")
##     res <- readRDS(paste0(path, rname, ".rds"))
##     if (simulations) {
##       res$simulations <- ffload_list(file = paste0(path, rname, ".sim"), 
##                                      overwrite = overwrite, rootpath = tempdir())
##     } else {
##       res$simulations <- NA
##     }
##     assign(rname,
##            res,
##            pos = env)
##     rm(res)
##   }
## }


## unload_residuals <- function(models, prefix = "", env = .GlobalEnv){
##   obj <- ls(name = env)
##   mnames <- paste0(prefix, models)
##   res_rm <- obj[obj %in% paste0(mnames, ".res")]
##   # Properly delete ff objects
##   for (i in 1:length(res_rm)) {
##     res <- get(res_rm[i], pos = env)
##     for (s in 1:length(res$simulations)) {
##       delete(res$simulations[[s]])
##     }
##   }
##   # Remove R objects
##   rm(list = res_rm, pos = env)
## }


## sim_post <- function(model, 
##                      posterior, 
##                      newdata = NULL,
##                      obs = "all",
##                      coef = "all",
##                      row.chunk = 1e3,
##                      post.chunk = 200,
##                      on.disk = FALSE,
##                      storage.mode = NULL,
##                      n.threads = 1,
##                      discrete = FALSE,
##                      progress = TRUE
##                      ) {
##   # Same as eval_post, but simulation is done *before* ff matrix is
##   # filled to reduce disk writes.
##   if(is.null(dim(posterior))) {
##     posterior <- matrix(posterior, ncol = length(posterior))
##   } 
##   if(is.null(newdata)) {
##     data <- model.frame(model)
##   } else {
##     data <- newdata
##   }
##   if(is.numeric(obs)) {
##     data <- data[obs,]
##   }
##   n <- nrow(data)
##   m <- nrow(posterior)
##   fam <- fix.family.rd(model$family)
##   weights <- model$prior.weights
##   scale <- model$sig2
##   row.from <- seq(1, n, row.chunk)
##   if(length(row.from) > 1) {
##     row.to <- c(row.from[2:length(row.from)]-1, n)
##   } else {
##     row.to <- n
##   }
##   post.from <- seq(1, m, post.chunk)
##   if(length(post.from) > 1) {
##     post.to <- c(post.from[2:length(post.from)]-1, m)
##   } else {
##     post.to <- m
##   }
##   post.chunks <- post.to - c(0, post.to[-length(post.to)])
##   # Set excluded coefficients to 0
##     if(is.numeric(coef)) {
##       posterior[,-coef] <- 0
##     }
##   simulations <- list()
##   for(s in 1:length(post.from)) {
##     if(on.disk) {
##       if(is.null(storage.mode)) storage.mode <- vmode(fitted(model))
##       simulations[[s]] <- ff(dim = c(n, post.chunks[s]), vmode = storage.mode, 
##                                  pattern = paste0(tempdir(), "/qres"), finalizer = "delete")
##     } else {
##       if(is.null(storage.mode)) storage.mode <- class(fitted(model))[1]
##       simulations[[s]] <- matrix(as(0, storage.mode), nrow = n, ncol = post.chunks[s])
##     }
##   }
##   if(progress) {
##     prog <- txtProgressBar(min = 0, max = length(row.from), initial = 0,
##                           char = "=", width = NA, title = "Progress", style = 3)
##   }
##   for(i in 1:length(row.from)) {
##     Xp <- predict(model, 
##                   data[row.from[i]:row.to[i],],
##                   type = "lpmatrix",
##                   block.size = row.chunk,
##                   newdata.guaranteed = TRUE,
##                   n.threads = n.threads,
##                   discrete = discrete)
##     for(j in 1:length(post.from)) {
##       lp <- Xp %*% t(posterior[post.from[j]:post.to[j],])
##       if(on.disk) {
##         simulations[[j]][row.from[i]:row.to[i],] <- 
##           apply(fam$linkinv(lp), 2, fam$rd, 
##                 wt = weights[row.from[i]:row.to[i]],  
##                 scale = scale)
##       } else {
##         simulations[[j]][row.from[i]:row.to[i],] <- 
##           as(apply(fam$linkinv(lp), 2, fam$rd, 
##                    wt = weights[row.from[i]:row.to[i]],  
##                    scale = scale),
##              storage.mode)
##       }
##       rm(lp)
##     }
##     rm(Xp)
##     gc()
##     if(progress) {
##       setTxtProgressBar(prog, i)
##     }
##   }
##   if(progress) close(prog)
##   return(simulations)
## }

#profile_matern <- function(y, x, rho,
#                           m = NULL, k = NULL, family = gaussian,
#                           discrete = FALSE, nthreads = NULL) {
#  if(is.null(m)) m <- 3
#  if(is.null(k)) k <- 100
#  m[2] <- NA
#  max.knots <- max(2000, k * 10)
#  REML <- numeric()
#  AIC <- numeric()
#  for(i in seq_along(rho)) {
#    message(paste0("Profiling `rho = ", rho[i], "` (", i, " / ", length(rho), ")"))
#    m[2] <- rho[i]
#    if(discrete) {
#      if(is.null(nthreads)) nthreads <- 1
#      mod <- bam(y ~ s(x[,1], x[,2], bs = "gp",
#                       m = m, k = k, xt = list(max.knots = max.knots)),
#                 family = family,
#                 discrete = TRUE,
#                 nthreads = nthreads)
#    } else {
#      mod <- gam(y ~ s(x[,1], x[,2], bs = "gp",
#                       m = m, k = k, xt = list(max.knots = max.knots)),
#                 family = family,
#                 method = "REML")
#    }
#    REML[i] <- mod$gcv.ubre
#    AIC[i] <- mod$aic
#  }
#  mat.prof <- data.frame(rho = rho, REML_score = REML, AIC = AIC)
#  return(mat.prof)
#}


#chunk_seq <- function(from, to, size = to) {
#  chunk.from <- seq(from, to, size)
#  if(length(chunk.from) > 1) {
#    chunk.to <- c(chunk.from[2:length(chunk.from)]-1, to)
#  } else {
#    chunk.to <- to
#  }
#  chunk.size <- chunk.to - c(0, chunk.to[-length(chunk.to)])
#  return(list(from = chunk.from,
#              to = chunk.to,
#              size = chunk.size))
#}

#lp_matrix <- 
#  function(
#           model, 
#           newdata = NULL,
#           id.col = NULL,
#           obs = NULL,
#           predict.chunk = NULL,
#           progress = TRUE,
#           ...
#           ) {
#  if(is.null(newdata)) {
#    data <- model.frame(model)
#  } else {
#    data <- newdata
#  }
#  if(is.numeric(obs)) {
#    data <- data[obs,]
#  }
#  n <- nrow(data)
#  m <- length(coef(model))
#  if(is.null(predict.chunk)) predict.chunk <- n
#  predict.chunks <- chunk_seq(1, n, predict.chunk)
#  # Set excluded coefficients to 0
#  lpmatrix <- matrix(nrow = n, ncol = m)
#  if(!is.null(id.col)) {
#    rownames(lpmatrix) <- data[[id.col]]
#  } else {
#    rownames(lpmatrix) <- 1:nrow(data)
#  }
#  colnames(lpmatrix) <- names(coef(model))
#  if(progress) {
#    prog <- txtProgressBar(min = 0, max = length(predict.chunks$from), initial = 0,
#                           char = "=", width = NA, title = "Progress", style = 3)
#  }
#  for(i in 1:length(predict.chunks$from)) {
#    Xp <- predict(model, 
#                  data[predict.chunks$from[i]:predict.chunks$to[i],],
#                  type = "lpmatrix",
#                  block.size = predict.chunks$size[i],
#                  newdata.guaranteed = TRUE,
#                  cluster = NULL,
#                  ...)
#    lpmatrix[(predict.chunks$from[i]:predict.chunks$to[i]), ] <- Xp
#    rm(Xp)
#    gc()
#    if(progress) {
#      setTxtProgressBar(prog, i)
#    }
#  }
#  if(progress) close(prog)
#  return(lpmatrix)
#}


#evaluate_posterior.old <- 
#  function(
#           model, 
#           posterior,
#           newdata = NULL,
#           id.col = NULL,
#           type = "link",
#           obs = NULL,
#           coef = NULL,
#           marginals = NULL,
#           predict.chunk = NULL,
#           post.chunk = NULL,
#           progress = TRUE
#           ) {
#  if(is.null(dim(posterior))) {
#    posterior <- matrix(posterior, ncol = length(posterior))
#  }
#  if(is.null(newdata)) {
#    data <- model.frame(model)
#  } else {
#    data <- newdata
#  }
#  if(is.numeric(obs)) {
#    data <- data[obs,]
#  }
#  n <- nrow(data)
#  m <- nrow(posterior)
#  if(is.null(predict.chunk)) predict.chunk <- n
#  if(is.null(post.chunk)) post.chunk <- m
#  predict.chunks <- chunk_seq(1, n, predict.chunk)
#  post.chunks <- chunk_seq(1, m, post.chunk)
#  # Set excluded coefficients to 0
#  if(is.numeric(coef)) {
#    posterior[,-coef] <- 0
#  }
#  if(is.null(marginals)) {
#    marginals <- list(1:ncol(posterior))
#  }
#  evaluated <- array(dim = c(n, m, length(marginals)))
#  if(!is.null(id.col)) {
#    dimnames(evaluated)[1] <- list(data[[id.col]])
#  } else {
#    dimnames(evaluated)[1] <- list(1:nrow(data))
#  }
#  if(progress) {
#    prog <- txtProgressBar(min = 0, max = length(predict.chunks$from), initial = 0,
#                           char = "=", width = NA, title = "Progress", style = 3)
#  }
#  for(i in 1:length(predict.chunks$from)) {
#    Xp <- predict(model, 
#                  data[predict.chunks$from[i]:predict.chunks$to[i],],
#                  type = "lpmatrix",
#                  block.size = predict.chunks$size[i],
#                  newdata.guaranteed = TRUE,
#                  cluster = NULL)
#    for(j in 1:length(marginals)) {
#    m.predict.chunk <- matrix(nrow = predict.chunks$size[i],
#                              ncol = m)
#      for(k in 1:length(post.chunks$from)) {
#        lp <- Xp[, marginals[[j]]] %*% t(posterior[post.chunks$from[k]:post.chunks$to[k],
#                                               marginals[[j]]])
#        if(type == "response") {
#          fam <- fix.family.rd(model$family)
#          m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- fam$linkinv(lp)
#        } else {
#          m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- lp
#        }
#        rm(lp)
#      }
#    evaluated[(predict.chunks$from[i]:predict.chunks$to[i]), , j] <- m.predict.chunk
#    rm(m.predict.chunk)
#    }
#    rm(Xp)
#    gc()
#    if(progress) {
#      setTxtProgressBar(prog, i)
#    }
#  }
#  if(progress) close(prog)
#  evaluated <- lapply(seq(dim(evaluated)[3]),
#                        \(x) {y <- evaluated[ , , x]
#                              dimnames(y)[1:2] <- dimnames(evaluated)[1:2]
#                              return(as_draws_matrix(t(y)))})
#  if(length(marginals) == 1) {
#    return(evaluated[[1]])
#  } else {
#    names(evaluated) <- names(marginals)
#    return(evaluated)
#  }
#}


#evaluate_posterior <- 
#  function(
#           model, 
#           posterior,
#           newdata = NULL,
#           id.col = NULL,
#           type = "link",
#           obs = NULL,
#           coef = NULL,
#           marginals = NULL,
#           marginal.ids = NULL,
#           predict.chunk = NULL,
#           post.chunk = NULL,
#           progress = TRUE
#           ) {
#  if(is.null(dim(posterior))) {
#    posterior <- matrix(posterior, ncol = length(posterior))
#  }
#  if(is.null(newdata)) {
#    data <- model.frame(model)
#  } else {
#    data <- newdata
#  }
#  if(is.null(id.col)) {
#    data$id <- 1:nrow(data)
#    id.col <- "id"
#  }
#  if(is.numeric(obs)) {
#    data <- data[obs,]
#  }
#  n <- nrow(data)
#  m <- nrow(posterior)
#  if(is.null(predict.chunk)) predict.chunk <- n
#  if(is.null(post.chunk)) post.chunk <- m
#  predict.chunks <- chunk_seq(1, n, predict.chunk)
#  post.chunks <- chunk_seq(1, m, post.chunk)
#  # Set excluded coefficients to 0
#  if(is.numeric(coef)) {
#    posterior[,-coef] <- 0
#  }
#  if(is.null(marginals)) {
#    marginals <- list(marginal = 1:ncol(posterior))
#  }
#  if(is.null(marginal.ids)) {
#    marginal.ids <- list(marginal = data[[id.col]])
#  }
#  if(length(marginals) != length(marginal.ids))
#    stop("Number of elements in `marginals` and `marginal.ids` must match.")
#  id.lu <- cbind(data[[id.col]], 1:nrow(data))
#  colnames(id.lu) <- c(id.col, "row.id")
#  id.lu <- as.data.table(id.lu)
#  setorder(id.lu, row.id)
#  mar.lu <- list()
#  for(i in seq_along(marginal.ids)) {
#    mar.lu[[i]] <- id.lu[id %in% marginal.ids[[i]]]
#    mar.lu[[i]][, marginal := names(marginals)[i]]
#  }
#  mar.lu <- rbindlist(mar.lu)
#  mar.lu$chunk <-
#    cut(mar.lu$row.id,
#        with(predict.chunks, c(to[1] - size[1], to)),
#        labels = FALSE)
#  mar.lu <-
#    mar.lu[,
#           .(n = .N,
#             row.ids = list(mar.lu$row.id[.I]),
#             ids = list(mar.lu[[id.col]][.I])),
#           by = c("marginal", "chunk")]
#  mar.lu[, `:=`(eval.id.from = 1 + cumsum(n) - n, eval.id.to = cumsum(n)), marginal]
#  evaluated <- list()
#  for(i in seq_along(marginals)) {
#      idx <- id.lu[eval(parse(text = paste(id.col, "%in% marginal.ids[[i]]")))]
#      evaluated[[i]] <- array(dim = c(nrow(idx), m))
#      dimnames(evaluated[[i]])[1] <- list(idx[[id.col]])
#  }
#  if(progress) {
#    prog <- txtProgressBar(min = 0, max = length(predict.chunks$from), initial = 0,
#                           char = "=", width = NA, title = "Progress", style = 3)
#  }
#  for(i in 1:length(predict.chunks$from)) {
#    Xp <- predict(model, 
#                  data[predict.chunks$from[i]:predict.chunks$to[i],],
#                  type = "lpmatrix",
#                  block.size = predict.chunks$size[i],
#                  newdata.guaranteed = TRUE,
#                  cluster = NULL)
#    for(j in 1:length(marginals)) {
#      chunk.lu <- mar.lu[chunk == i & marginal == names(marginals)[j]]
#      if(nrow(chunk.lu) < 1) next
#      pc.rows <- unlist(chunk.lu$row.ids) - with(predict.chunks, to[i] - size[i])
#      eval.rows <- with(chunk.lu, eval.id.from:eval.id.to)
#      m.predict.chunk <- matrix(nrow = chunk.lu$n, ncol = m)
#      for(k in 1:length(post.chunks$from)) {
#        lp <-
#          Xp[pc.rows, marginals[[j]]] %*%
#          t(posterior[post.chunks$from[k]:post.chunks$to[k], marginals[[j]]])
#        if(type == "response") {
#          fam <- model$family
#          m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- fam$linkinv(lp)
#        } else {
#          m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- lp
#        }
#        rm(lp)
#      }
#      evaluated[[j]][eval.rows,] <- m.predict.chunk
#      rm(m.predict.chunk)
#    }
#    rm(Xp)
#    gc()
#    if(progress) {
#      setTxtProgressBar(prog, i)
#    }
#  }
#  if(progress) close(prog)
#  evaluated <- lapply(evaluated, \(x) as_draws_matrix(t(x)))
#  if(length(marginals) == 1) {
#    return(evaluated[[1]])
#  } else {
#    names(evaluated) <- names(marginals)
#    return(evaluated)
#  }
#}

#aggregate_variables <- function(predictions, ...) {
#  UseMethod("aggregate_variables", predictions)
#}


#aggregate_variables.draws_matrix <- 
#  function(
#           predictions,
#           trans.fun = NULL,
#           agg.fun = E,
#           ids = NULL,
#           draw.ids = NULL,
#           draw.chunk = NULL,
#           agg.size = NULL,
#           clamp = NULL,
#           n.threads = NULL,
#           progress = TRUE,
#           ...) {
#  if(is.numeric(n.threads)) {
#    dt.threads.old <- getDTthreads()
#    arrow.threads.old <- cpu_count()
#    setDTthreads(n.threads)
#    set_cpu_count(n.threads)
#  }
#  if(is.null(draw.ids)) {
#    draw.ids <- rownames(predictions)
#  }
#  if(is.null(draw.chunk)) {
#    draw.chunk <- length(draw.ids)
#  }
#  if(is.null(ids)) {
#    ids <- list(1:ncol(predictions))
#  }
#  draw.chunks <- chunk_seq(1, length(draw.ids), draw.chunk)
#  if(is.null(agg.size))
#    agg.size <- min(unlist(lapply(ids, length)))
#  # n.obs <- sum(unlist(lapply(ids, length)))
#  # if(agg.size > n.obs)
#  #   agg.size <- n.obs
#  id.col <- "id"
#  draw.column <- "draw"
#  value.column <- "eta"
#  # id.cond <- parse(text = paste(id.col, "%in% ids.sum"))
#  # draw.cond <- parse(text = paste0(draw.column,
#  #                                  "%in% draw.ids[draw.chunks$from[i]:draw.chunks$to[i]]"))
#  agg.expr <- parse(text = paste0("agg.fun(", value.column,")"))
#  # order.cond <- parse(text = paste0("order(", draw.column, ", group.id)"))
#  cast.form <- as.formula(paste(draw.column, "~ group.id"))
#  ids.dt <- data.table(group.label = names(ids),
#                       group.id = 1:length(ids),
#                       ids = ids)[order(group.id)]
#  ids.dt[,N := as.numeric(unname(unlist(lapply(ids, length))))]
#  ids.dt[order(-N), Nc := cumsum(N)]
#  ids.dt[,
#         `:=`(aggregate = ifelse(N < agg.size, TRUE, FALSE))
#         ][aggregate == TRUE, 
#           agg.id := ceiling(Nc / agg.size)]
#  setkey(ids.dt, group.id)
#  # setindex(ids.dt, agg.id)
#  agg.ids <- na.omit(unique(ids.dt[order(agg.id), agg.id]))
#  single.ids <- ids.dt[aggregate == FALSE, group.id]
#  ids.dt.l <-
#    ids.dt[,
#           lapply(.SD, unlist), .SDcols = c("ids", "agg.id"),
#           by = "group.id"
#           ]
#  setnames(ids.dt.l, "ids", id.col)
#  predictions.summarized <- matrix(NA, nrow = length(draw.ids), ncol = length(ids))
#  dimnames(predictions.summarized)[1:2] <- list(draw.ids, ids.dt$group.label)
#  predictions <- as.data.table(as_draws_df(predictions))
#  exclude.vars <- c(".iteration", ".chain")
#  predictions <-
#    predictions[,!..exclude.vars] |>
#    melt(id.vars = ".draw",
#         variable.name = id.col, value.name = value.column,
#         variable.factor = FALSE)
#  predictions <- predictions[, (draw.column) := as.factor(.draw)][,-".draw"]
#  setkeyv(predictions, draw.column)
#  if(is.factor(predictions[[id.col]]) &
#     !is.factor(ids.dt.l[[id.col]])) 
#    ids.dt.l[[id.col]] <- factor(as.character(ids.dt.l[[id.col]]))
#  if(is.character(predictions[[id.col]]) &
#     !is.character(ids.dt.l[[id.col]]))
#    ids.dt.l[[id.col]] <- as.character(ids.dt.l[[id.col]])
#  setkeyv(ids.dt.l, id.col)
#  setindexv(ids.dt.l, list("group.id", "agg.id"))
#  if(!is.null(clamp)) {
#    clamp.cond.l <- parse(text = paste(value.column, "< clamp[1]"))
#    clamp.cond.u <- parse(text = paste(value.column, "> clamp[2]"))
#    predictions[eval(clamp.cond.l) | eval(clamp.cond.u),
#                 (value.column) := fcase(eval(clamp.cond.l), clamp[1],
#                                         eval(clamp.cond.u), clamp[2])]
#  }
#  if(!is.null(trans.fun)) {
#    trans.expr <- parse(text = paste0(value.column, " := trans.fun(", value.column,")"))
#    predictions[, eval(trans.expr)]
#  }
#  if(progress) {
#    prog <- txtProgressBar(min = 0, max = max(ids.dt$Nc) * length(draw.chunks$from), initial = 0,
#                           char = "=", width = NA, title = "Progress", style = 3)
#    prog.counter <- 0
#  }
#  for(i in seq_along(draw.chunks$from)) {
#    draws.sum <- draw.ids[draw.chunks$from[i]:draw.chunks$to[i]]
#    predictions.draws <- predictions[.(draws.sum),
#                                     on = draw.column,
#                                     nomatch = NULL]
#    setkeyv(predictions, id.col)
#   for(j in seq_along(single.ids)) {
#      order.cond <- parse(text = paste0("order(", draw.column, ")"))
#      ids.sum <- ids.dt.l[.(single.ids[j]), on = "group.id"][[id.col]]
#      chunk.summarized <-
#        predictions.draws[.(ids.sum),
#                          nomatch = NULL,
#                          on = id.col
#                          ][,
#                            .(val = eval(agg.expr)),
#                            by = draw.column,
#                            ][eval(order.cond),
#                             val]
#      if(!is.null(chunk.summarized) & length(chunk.summarized) == draw.chunks$size[i]) {
#        predictions.summarized[draw.chunks$from[i]:draw.chunks$to[i],
#                               single.ids[j]] <- chunk.summarized
#      } else {
#        predictions.summarized[draw.chunks$from[i]:draw.chunks$to[i],
#                               single.ids[j]] <- NA
#      }
#      if(progress) {
#        prog.counter <- prog.counter + ids.dt[group.id == single.ids[j], N]
#        setTxtProgressBar(prog, prog.counter)
#      }
#    }
#    for(k in seq_along(agg.ids)) {
#      match.ids.groups <- ids.dt.l[.(agg.ids[k]), on = "agg.id"]
#      chunk.summarized <-
#        predictions.draws[match.ids.groups,
#                          nomatch = NULL,
#                          on = id.col,
#                          allow.cartesian = TRUE
#                          ][,
#                            .(val = eval(agg.expr)),
#                            by = c(draw.column, "group.id")
#                            ]
#      setorderv(chunk.summarized, c(draw.column, "group.id"))
#      if(nrow(chunk.summarized) > 0) {
#        chunk.summarized <- dcast(chunk.summarized, cast.form, value.var = "val")
#        group.cols <- as.integer(colnames(chunk.summarized[,-..draw.column]))
#        draw.cols <- draw.chunks$from[i]:draw.chunks$to[i]
#        predictions.summarized[draw.cols, group.cols] <- 
#          as.matrix(chunk.summarized[, -..draw.column])
#      } else {
#        draw.cols <- draw.chunks$from[i]:draw.chunks$to[i]
#        group.cols <- match.ids.groups[, unique(group.id)]
#        predictions.summarized[draw.cols, group.cols] <- NA
#      }
#      if(progress) {
#        prog.counter <- prog.counter + sum(ids.dt[agg.id == agg.ids[k], N])
#        setTxtProgressBar(prog, prog.counter)
#      }
#    }
#  }
#  if(progress) close(prog)
#  if(is.numeric(n.threads)) {
#    setDTthreads(dt.threads.old)
#  }
#  return(as_draws_matrix(predictions.summarized))
#}

#aggregate_variables.FileSystemDataset <- 
#  function(
#           predictions,
#           trans.fun = NULL,
#           agg.fun = mean,
#           ids = NULL,
#           id.col = "id",
#           draw.column = "draw",
#           draw.ids = NULL,
#           draw.chunk = NULL,
#           value.column = "eta",
#           agg.size = NULL,
#           clamp = NULL,
#           pull.all = TRUE,
#           progress = TRUE,
#           gc = FALSE,
#           ...) {
#  if(is.null(draw.ids)) {
#    # Hack to get draw.ids if not specified
#    draw.ids <- levels(as.data.frame(ds[1,draw.column])[[1]])
#  }
#  if(is.null(draw.chunk)) {
#    draw.chunk <- length(draw.ids)
#  }
#  if(is.null(ids)) {
#    n.r <-  
#      filter(predictions, .data[[draw.column]] == draw.ids[1]) |>
#      nrow()
#    ids <- list(1:n.r)
#  }
#  if(!pull.all) {
#    pull.ids <- unique(do.call(c, ids))
#  }
#  draw.chunks <- chunk_seq(1, length(draw.ids), draw.chunk)
#  if(is.null(agg.size)) {
#    agg.size <- min(unlist(lapply(ids, length)))
#  }
#  ids.dt <- data.table(group.label = names(ids),
#                       group.id = 1:length(ids),
#                       ids = ids)[order(group.id)]
#  ids.dt[,N := as.numeric(unname(unlist(lapply(ids, length))))]
#  ids.dt[order(-N), Nc := cumsum(N)]
#  ids.dt[,
#      `:=`(aggregate = ifelse(N < agg.size, TRUE, FALSE))
#      ][aggregate == TRUE, 
#        agg.id := ceiling(Nc / agg.size)]
#  setkey(ids.dt, group.id)
#  # setindex(ids.dt, agg.id)
#  agg.ids <- na.omit(unique(ids.dt[order(agg.id), agg.id]))
#  single.ids <- ids.dt[aggregate == FALSE, group.id]
#  ids.dt.l <-
#    ids.dt[,
#           lapply(.SD, unlist), .SDcols = c("ids", "agg.id"),
#           by = "group.id"
#           ]
#  setnames(ids.dt.l, "ids", id.col)
#  id.sample <- collect(ds[1,id.col])[[id.col]]
#  if(is.factor(id.sample) &
#     !is.factor(ids.dt.l[[id.col]])) 
#    ids.dt.l[[id.col]] <- factor(as.character(ids.dt.l[[id.col]]))
#  if(is.character(id.sample) &
#     !is.character(ids.dt.l[[id.col]]))
#    ids.dt.l[[id.col]] <- as.character(ids.dt.l[[id.col]])
#  setkeyv(ids.dt.l, id.col)
#  setindexv(ids.dt.l, list("group.id", "agg.id"))
#  predictions.summarized <- matrix(NA, nrow = length(draw.ids), ncol = length(ids))
#  dimnames(predictions.summarized)[1:2] <- list(draw.ids, ids.dt$group.label)
#  if(progress) {
#    prog <- txtProgressBar(min = 0, max = max(ids.dt$Nc) * as.numeric(length(draw.chunks$from)),
#                           initial = 0,
#                           char = "=", width = NA, title = "Progress", style = 3)
#    prog.counter <- 0
#  }
#  for(i in seq_along(draw.chunks$from)) {
#    if(pull.all) {
#      predictions.pulled <-
#        filter(predictions,
#               .data[[draw.column]] %in% draw.ids[draw.chunks$from[i]:draw.chunks$to[i]]) |>
#        collect()
#    } else {
#      predictions.pulled <-
#        filter(predictions,
#               id %in% pull.ids,
#               .data[[draw.column]] %in% draw.ids[draw.chunks$from[i]:draw.chunks$to[i]]) |>
#        collect()
#    }
#    setDT(predictions.pulled, key = id.col)
#    if(!is.null(clamp)) {
#      clamp.cond.l <- parse(text = paste(value.column, "< clamp[1]"))
#      clamp.cond.u <- parse(text = paste(value.column, "> clamp[2]"))
#      predictions.pulled[eval(clamp.cond.l) | eval(clamp.cond.u),
#                         (value.column) := fcase(eval(clamp.cond.l), clamp[1],
#                                                 eval(clamp.cond.u), clamp[2])]
#    }
#    if(!is.null(trans.fun)) {
#      trans.expr <- parse(text = paste0(value.column, " := trans.fun(", value.column,")"))
#      predictions.pulled[, eval(trans.expr)]
#    }
#    agg.expr <- parse(text = paste0("agg.fun(", value.column,")"))
#    id.cond <- parse(text = paste(id.col, "%in% ids.sum"))
#    for(j in seq_along(single.ids)) {
#      order.cond <- parse(text = paste0("order(", draw.column, ")"))
#      ids.sum <- ids.dt.l[.(single.ids[j]), on = "group.id"][[id.col]]
#      chunk.summarized <-
#        predictions.pulled[.(ids.sum),
#                           .(val = eval(agg.expr)),
#                           by = draw.column,
#                           nomatch = NULL
#                           ][eval(order.cond),
#                             val]
#      if(!is.null(chunk.summarized) & length(chunk.summarized) == draw.chunks$size[i]) {
#        predictions.summarized[draw.chunks$from[i]:draw.chunks$to[i],
#                               single.ids[j]] <- chunk.summarized
#      } else {
#        predictions.summarized[draw.chunks$from[i]:draw.chunks$to[i], single.ids[j]] <- NA
#      }
#      if(progress) {
#        prog.counter <- prog.counter + ids.dt[group.id == single.ids[j], N]
#        setTxtProgressBar(prog, prog.counter)
#      }
#    }
#    order.cond <- parse(text = paste0("order(", draw.column, ", group.id)"))
#    cast.form <- as.formula(paste(draw.column, "~ group.id"))
#    for(k in seq_along(agg.ids)) {
#      match.ids.groups <- ids.dt.l[.(agg.ids[k]), on = "agg.id"]
#      chunk.summarized <-
#        predictions.pulled[match.ids.groups,
#                           nomatch = NULL,
#                           allow.cartesian = TRUE
#                           ][ ,
#                             .(val = eval(agg.expr)),
#                             by = c(draw.column, "group.id")
#                             ]
#      setorderv(chunk.summarized, c(draw.column, "group.id"))
#      if(nrow(chunk.summarized) > 0) {
#        chunk.summarized <- dcast(chunk.summarized, cast.form, value.var = "val")
#        draw.cols <- draw.chunks$from[i]:draw.chunks$to[i]
#        group.cols <- as.integer(colnames(chunk.summarized[,-..draw.column]))
#        predictions.summarized[draw.cols, group.cols] <- 
#          as.matrix(chunk.summarized[, -..draw.column])
#      } else {
#        draw.cols <- draw.chunks$from[i]:draw.chunks$to[i]
#        group.cols <- match.ids.groups[, unique(group.id)]
#        predictions.summarized[draw.cols, group.cols] <- NA
#      }
#      if(progress) {
#        prog.counter <- prog.counter + sum(ids.dt[agg.id == agg.ids[k], N])
#        setTxtProgressBar(prog, prog.counter)
#      }
#    }
#    rm(predictions.pulled)
#    if(gc) gc()
#  }
#  if(progress) close(prog)
#  return(as_draws_matrix(predictions.summarized))
#}

#ids_by_group <- function(data,
#                         id.col = NULL,
#                         group.vars = NULL,
#                         group.labels = NULL,
#                         add.label = TRUE,
#                         expand.label = TRUE,
#                         label.prefix = NULL,
#                         ...){
#  setDT(data)
#  if(is.null(group.vars)) {
#    if(is.null(id.col)) {
#      groups <- data[, .(n = .N, ids = list(.I))]
#    } else {
#      groups <- data[, .(n = .N, ids = list(data[[id.col]][.I]))]
#    }
#  } else {
#    if(is.null(id.col)) {
#      groups <- data[, .(n = .N, ids = list(.I)), by = group.vars]
#    } else {
#      groups <- data[, .(n = .N, ids = list(data[[id.col]][.I])), by = group.vars]
#    }
#    setorderv(groups, group.vars)
#    labels <- groups[, ..group.vars]
#  }
#  if(!is.null(group.labels)) {
#    for(i in 1:length(group.vars)) {
#      old.labels <- as.character(groups[[group.vars[i]]])
#      new.labels <- factor(group.labels[[i]][old.labels], 
#                           levels = group.labels[[i]])
#      groups[, (group.vars[i]) := new.labels]
#    }
#    setorderv(groups, group.vars)
#  }
#  if(add.label == TRUE & !is.null(group.vars)) {
#    groups <- add_group_label(data = groups, cols = group.vars,
#                              expand.label = expand.label)
#  }
#  if(is.character(add.label) & !is.null(group.vars)) {
#    groups <- add_group_label(data = groups, cols = group.vars,
#                              label.name = add.label,
#                              expand.label = expand.label)
#  }
#  if(!is.null(label.prefix)) {
#    groups[[add.label]] <- paste0(label.prefix, groups[[add.label]])
#  }
#  return(groups)
#}

#add_group_label <- function(data,
#                      cols,
#                      label.name = "group.label",
#                      expand.label = TRUE) {
#    data <- copy(data)
#    labels <- data[, ..cols]
#    if(expand.label) {
#      for(i in 1:length(cols)) {
#        labels[[cols[i]]] <- paste(cols[i], labels[[cols[i]]], sep = ".")
#      }
#    }
#    labels.c <- do.call(paste, c(labels, sep = ":"))
#    data[, (label.name) := labels.c]
#    setorderv(data, c(cols, label.name))
#    setcolorder(data, c(label.name, cols))
#    return(data)
#}

#summarize_predictions_by <- 
#  function(predictions,
#           data,
#           group.vars,
#           group.labels = NULL,
#           ...) {
#  groups <- ids_by_group(data = data,
#                         group.vars = group.vars,
#                         group.labels = group.labels,
#                         add.label = TRUE)
#  id.list <- groups$ids
#  names(id.list) <- groups$group.label
#  groups.summarized <-
#    summarize_predictions(predictions = predictions,
#                         ids = id.list,
#                         ...)
#  return(groups.summarized)
#}

#extract_weights <- function(x, ...) {
#  UseMethod("extract_weights", x)
#}


#extract_weights.data.table <- function(x, w.col = NULL, by.col = NULL, standardize = TRUE) {
#  if(is.null(w.col)) w.col <- names(x)[2]
#  if(is.null(by.col)) by.col <- names(x)[1]
#  w.expr <- parse(text = paste0("sum(", w.col, ")"))
#  w.dt <- x[, .(w = eval(w.expr)), by.col]
#  w <- w.dt$w
#  names(w) <- w.dt[[by.col]]
#  if(standardize) w <- w / sum(w)
#  return(w)
#}


#weighted_sum <- function(x, ...) {
#  UseMethod("weighted_sum", x)
#}


#weighted_sum.draws_matrix <- function(x, w) {
#  y <-
#    x[,names(w)] %*% diag(w) |>
#    rowSums() |>
#    rvar()
#  return(y)
#}


#reweigh_posterior <- function(posterior, ...) {
#  UseMethod("reweigh_posterior", posterior)
#}


#reweigh_posterior.draws_matrix <- function(posterior, w, w.init = NULL) {
#  if(is.null(w.init)) {
#    w.re <- w
#  } else {
#    w.re <- lapply(w.mod, \(x) w <- w.init[names(x)] * x)
#  }
#  var.w <-
#    lapply(w.re, \(w) weighted_sum(posterior, w)) |>
#    as_draws_matrix()
#  return(var.w)
#}



## SOM UTILITIES ################################################################

#init_som <- function(data, xdim, ydim) {
#  # Calculate principal components
#  init_pca <- prcomp(x = data, center = FALSE, scale = FALSE)
#  init_max <- apply(init_pca$x[, 1:2], 2, max)
#  init_min <- apply(init_pca$x[, 1:2], 2, min)
#  # Distribute nodes along first two PC
#  init_coord_pc <- matrix(NA, nrow = xdim * ydim, ncol = 2)
#  init_coord_pc[, 1] <-  rep(seq(init_min[1], init_max[1], 
#                                  length.out = xdim),
#                              times = ydim)
#  init_coord_pc[, 2] <-  rep(seq(init_min[2], init_max[2], 
#                                  length.out = ydim),
#                              each = ydim)
#  # Map to covariate space
#  init_coord_cov <- init_coord_pc %*% t(init_pca$rotation[,1:2])
#  return(init_coord_cov)
#}

#scale_data_som <- function(data, som) {
#  scaled <- t((t(data) - som$scale$mean) / som$scale$sd)
#  return(scaled)
#}

#bmu <- function(x, codes, bmu.rank = 1, dist = FALSE) {
#  dist_bmu <- colMeans((t(codes) - x)^2, na.rm = TRUE)
#  bmus <- order(dist_bmu)[bmu.rank]
#  names(bmus) <- paste0("bmu.", bmu.rank)
#  if(dist) {
#    distances <- dist_bmu[bmus]
#    names(distances) <- paste0("dist.", bmu.rank)
#    return(list(bmus, dist_bmu[bmus]))
#  } else {
#    return(bmus)
#  }
#}

#units_nb <- function(x, y, grid) {
#  # Only for rectangular, square grids
#  stopifnot({
#              grid$topo == "rectangular"
#              grid$toroidal == FALSE
#            })
#  u1 <- grid$pts[x,]
#  u2 <- grid$pts[y,]
#  du.x <- abs(u1[,1] - u2[,1])
#  du.y <- abs(u1[,2] - u2[,2])
#  nb <- (du.x == 1 | du.y == 1)
#  return(nb)
#}

#dist_to_unit <- function(x, unit, codes, type = "sumofsquares") {
#  stopifnot({
#              type %in% c("euclidean", "sumofsquares")
#            })
#  dist.squared <- rowSums((x - codes[unit, ])^2, na.rm = TRUE)
#  if(type == "euclidean") {
#    return(sqrt(dist.squared))
#  }
#  if(type == "sumofsquares") {
#    return(dist.squared)
#  }
#}

#embed_som <- function(som,
#                      data = NULL,
#                      bmu.rank = 1,
#                      dist = FALSE,
#                      grid.coord = FALSE,
#                      n.cores = 1) {
#  if(is.null(data)) {
#    data <- som$data[[1]]
#  }
#  n.bmu <- length(bmu.rank)
#  bmus <- matrix(NA, nrow = nrow(data), ncol = length(bmu.rank))
#  dimnames(bmus) <- list(rownames(data), paste0("bmu.", bmu.rank))
#  if(dist) {
#    bmu.dist <- bmus
#    dimnames(bmu.dist) <- list(rownames(data), paste0("dist.", bmu.rank))
#  }
#  if(n.bmu == 1 & bmu.rank[1] == 1) {
#    # Use fast implementation from `kohonen`
#    mapped <- map(x = som, newdata = data)
#    bmus[,1:n.bmu] <- mapped$unit.classif
#    if(dist) {
#      bmu.dist[,1:n.bmu] <- mapped$distances
#    }
#  } else {
#    codes <- som$codes[[1]]
#    registerDoParallel(n.cores)
#    mapped <-
#      foreach(i = 1:nrow(data), .combine = rbind) %dopar% {
#        bmu(data[i,], codes, bmu.rank = bmu.rank, dist = dist)
#      }
#    bmus[,1:n.bmu] <- do.call(rbind, as.list(mapped[,1:n.bmu]))
#    if(dist) {
#      bmu.dist[,1:n.bmu] <- do.call(rbind, as.list(mapped[,1:n.bmu]))
#    }
#  }
#  results <- list()
#  results$bmu <- bmus
#  if(dist) {
#    results$distances <- bmu.dist
#  }
#  if(grid.coord) {
#    coord <- list()
#    for(i in 1:length(bmu.rank)) {
#      coord[[i]] <- som$grid$pts[bmus[,i],]
#    }
#    names(coord) <- colnames(bmus)
#    results$grid.coordinates <- coord
#  }
#  return(results)
#}


#quantization_error <- function(som, data = NULL, distances = NULL) {
#  if(is.null(data)) {
#    distances <- som$distances
#  }
#  if(is.null(distances)) {
#    embedded <- embed_som(som = som, data = data, bmu.rank = 1, n.cores = n.cores, dist = TRUE)
#    distances <- embedded$distances
#  }
#  if(som$dist.fcts == "sumofsquares") {
#    dist.sq <- distances
#  } else {
#    dist.sq <- distances^2
#  }
#  quant.e <- mean(dist.sq)
#  return(quant.e)
#}

#topological_error <- function(som, data = NULL, n.cores = 1, bmus = NULL) {
#  if(is.null(data)) {
#    data <- som$data[[1]]
#  }
#  if(is.null(bmus)){
#    bmus <- embed_som(som = som, data = data, bmu.rank = c(1,2), n.cores = n.cores)$bmu
#  }
#  stopifnot({
#             nrow(bmus) == nrow(data)
#             ncol(bmus) == 2
#            })
#  nb <- units_nb(bmus[,1], bmus[,2], som$grid)
#  topo.e <- 1 - (sum(nb) / length(nb))
#  return(topo.e)
#}

#kaski_lagus_error <- function(som, data = NULL, n.cores = 1, bmus = NULL) {
#  if(is.null(data)) {
#    data <- som$data[[1]]
#  }
#  if(is.null(bmus)){
#    bmus <- embed_som(som = som, data = data, bmu.rank = c(1,2), n.cores = n.cores)
#  }
#  stopifnot({
#             nrow(bmus) == nrow(data)
#             ncol(bmus) == 2
#            })
#  dist.grid <- unit.distances(som$grid)
#  dist.fs <- as.matrix(dist(som$codes[[1]], method = "euclidean"))
#  Aw <- dist.fs * (dist.grid == 1)
#  graph <- igraph::graph_from_adjacency_matrix(Aw, weighted = TRUE)
#  rm(Aw, dist.fs, dist.grid)
#  registerDoParallel(n.cores)
#  # Carefult with number of cores here, distance calculation is memory
#  # intensive
#  dist.shortest <-
#    foreach(i = 1:nrow(bmus), .combine = c) %dopar% {
#      d <- igraph::distances(graph, bmus[i,1], bmus[i,2])
#      return(d[1,1])
#    }
#  dist.eucl <- dist_to_unit(data, bmus[,1], som$codes[[1]],
#                            type = "euclidean")
#  kl.e <- mean(dist.eucl + dist.shortest)
#  return(kl.e)
#}

#variance_explained <- function(som, data = NULL, n.cores = 1, qe = NULL) {
#  if(is.null(data)) {
#    data <- som$data[[1]]
#  }
#  if(is.null(qe)) {
#    qe <- quantization_error(som, data)
#  }
#  var.data <- mean(colSums((t(data) - colMeans(data, na.rm = TRUE))^2, na.rm = TRUE))
#  var.ex <- 1 - (qe/var.data)
#  return(var.ex)
#}

#                               som = NULL
#                               mapped = NULL
#                               family = gaussian
#                               combined = FALSE
#                               bs = "tp"
#                               k.max = NULL
#                               max.knots = NULL
#                               n.cores = 1
#                               approximate = TRUE
#                               ret.models = FALSE


#evaluate_embedding <- function(data,
#                               som = NULL,
#                               mapped = NULL,
#                               family = gaussian,
#                               combined = FALSE,
#                               bs = "tp",
#                               k.max = NULL,
#                               max.knots = NULL,
#                               n.cores = 1,
#                               approximate = TRUE,
#                               ret.models = FALSE) {
#  if(is.null(som) & is.null(mapped))
#    stop("Must provide either `som` or `mapped`.")
#  if(is.null(mapped)) {
#    embedding <- embed_som(som, data, n.cores = n.cores, bmu.rank = 1, grid.coord = TRUE)
#    mapped <- embedding$grid.coordinates[[1]]
#  }
#  data <- as.data.frame(data)
#  k.mod <- min(nrow(unique(mapped)), k.max)
#  if(is.null(max.knots)) {
#    if(!is.null(som)) {
#      max.knots.mod <- nrow(som$grid$pts)
#    } else {
#      max.knots.mod <- max(2000, min(k.mod * 10, nrow(data)))
#    }
#  } else {
#    max.knots.mod <- max.knots
#  }
#  models <- list()
#  k <- edf <- r.sq <- dev.expl <- list()
#  vars <- colnames(data)
#  for(i in 1:ncol(data)) {
#    message(paste0("Evaluate embedding for variable ", i, " / ", ncol(data), " …"))
#    if(length(family) == 1) {
#      fam <- family
#    } else {
#      fam <- family[[i]]
#    }
#    if(!approximate) {
#      models[[i]] <-
#        gam(data[,i] ~ s(mapped[,1], mapped[,2], bs = bs,
#                         k = k.mod, xt = list(max.knots = max.knots.mod)),
#            family = fam,
#            optimizer = "efs",
#            method = "REML")
#    } else {
#      models[[i]] <-
#        bam(data[,i] ~ s(mapped[,1], mapped[,2], bs = bs,
#                         k = k.mod, xt = list(max.knots = max.knots.mod)),
#            family = fam,
#            discrete = TRUE,
#            nthreads = n.cores)
#    }
#  }
#  if(combined) {
#    message(paste0("Evaluate combined embedding for all variables …"))
#    vars <- c(vars, "combined")
#    formulas <- list()
#    for(i in 1:ncol(data)) {
#      formulas[[i]] <- as.formula(paste0("data[,", i, "] ~ 1"))
#    }
#    formulas[[ncol(data)+1]] <-
#      as.formula(paste0(paste(1:ncol(data), collapse = " + "),
#                        " ~ s(mapped[,1], mapped[,2], bs = '", bs ,"', ",
#                        "k = k.mod - ncol(data)) - 1"))
#    models[[length(models) + 1]] <-
#      gam(formulas,
#          family = mvn(d = ncol(data)),
#          method = "REML",
#          optimizer = "efs")
#  }
#  for(i in 1:length(models)) {
#    k[[i]] <- k.mod
#    edf[[i]] <- sum(models[[i]]$edf)
#    dev.expl[[i]] <-
#      (models[[i]]$null.deviance - models[[i]]$deviance) / 
#      models[[i]]$null.deviance
#    w <- as.numeric(models[[i]]$prior.weights)
#    mean.y <- sum(w * models[[i]]$y)/sum(w)
#    w <- sqrt(w)
#    residual.df <- length(models[[i]]$y) - sum(models[[i]]$edf)
#    r.sq[[i]] <-
#      1 - var(w * (as.numeric(models[[i]]$y) - models[[i]]$fitted.values)) *
#      (nobs(models[[i]]) - 1)/(var(w * (as.numeric(models[[i]]$y) - mean.y)) *
#      (length(models[[i]]$y) - sum(models[[i]]$edf)))
#  }
#  evaluation <- list(variable = vars, k = k, edf = edf,
#                      dev.expl = dev.expl, r.sq = r.sq) |>
#                lapply(unlist) |>
#                as.data.frame()
#  if(ret.models) {
#    results <- list(evaluation = evaluation, models = models)
#  } else {
#    results <- evaluation
#  }
#  return(results)
#}


#bmu_min_obs <- function(x, codes, obs.bmu, obs.min = 1) {
#  obs.bmu <- data.table(bmu = obs.bmu[[1]],
#                        n = obs.bmu[[2]])
#  dist.bmu <- colMeans((t(codes) - x)^2, na.rm = TRUE)
#  dist.bmu <- data.table(bmu = 1:length(dist.bmu),
#                     dist = dist.bmu)
#  bmus <- merge(dist.bmu, obs.bmu, "bmu")[order(dist)]
#  bmu.ids <- c(1, bmus[, n.c := cumsum(n)][n.c < obs.min, .I] + 1)
#  bmu.matched <- bmus$bmu[bmu.ids]
#  return(bmu.matched)
#}

#bmu_match_reference <- function(data, som, obs.bmu, obs.min, n.cores = 1) {
#  codes <- som$codes[[1]]
#  registerDoParallel(n.cores)
#  bmus.matched <-
#    foreach(i = 1:nrow(data)) %dopar% {
#      bmu_min_obs(x = data[i,], codes = codes,
#                  obs.bmu = obs.bmu, obs.min = obs.min)
#    }
#  return(bmus.matched)
#}

## GAM UTILITIES ################################################################

#lookup_smooths <- function(gam) {
#  smooth.lu <- list()
#    for(i in seq_along(gam$smooth)) {
#      smooth.lu[[i]] <-
#        with(gam$smooth[[i]],
#             data.frame(smooth.id = i, label, first.para, last.para))
#    }
#  smooth.lu <- rbindlist(smooth.lu)
#  smooth.lu[, `:=`(para = list(seq(first.para, last.para))), "smooth.id"]
#  return(smooth.lu)
#}


##diag_residuals <- function(model, 
##                           residuals,
##                           sample = NULL,
##                           hist.bins = 100,
##                           trend.res = 1000,
##                           trend.k = 10,
##                           col.empirical = palette.colors(palette = "Set 1")[1],
##                           col.theoretical = palette.colors(palette = "Set 1")[2],
##                           qq.point.size = 1.5,
##                           line.size = 0.8,
##                           plot.theme = theme_classic()
##                           ) 
##  {
##  # Extract residuals and fitted values
##  if(is.null(sample)){
##    qres <- data.table(residuals = residuals$quantile_residuals,
##                       linpred = predict(model, type = "link",
##                                         newdata.guaranteed = TRUE),
##                       response =  model$model[,1])
##  } else {
##    qres <- data.table(residuals = residuals$quantile_residuals[sample],
##                       linpred = predict(model, newdata = model$model[sample,],
##                                         type = "link",
##                                         newdata.guaranteed = TRUE),
##                       response =  model$model[sample, 1],
##    fitted = fitted(model)[sample])
##  }

##  # Preparatory calculations
##  # Histogram
##  qres[, bin := cut(residuals, seq(0, 1, 1/hist.bins), labels = FALSE)]

##  # Residuals vs. linear predictor
##  qres[, linpred_r := frank(linpred)]

##  # Model for smooth trend
##  # m_res_linpred <- gam(residuals ~ s(predicted), data = qres, select = TRUE)
##  m_res_linpred <- bam(residuals ~ s(linpred, k = trend.k), 
##                    data = qres, 
##                    select = TRUE, 
##                    discrete = TRUE)

##  # Trend for untransformed predictor
##  t_res_linpred <- data.table(linpred = seq(from = min(qres$linpred), 
##                                         to = max(qres$linpred), 
##                                         length.out = trend.res))
##  t_res_linpred[,trend := predict(m_res_linpred, t_res_linpred)]

##  # Trend for rank-transformed predictor
##  quantiles <- (1:(trend.res -1)) / (trend.res - 1)
##  t_res_linpred_r <- data.table(linpred = c(min(qres$linpred), 
##                                       quantile(qres$linpred, quantiles)),
##                           linpred_r = (0:(trend.res - 1) / (trend.res - 1) * 
##                                    (length(qres$linpred)  - 1)) + 1
##                           )
##  t_res_linpred_r[,trend := predict(m_res_linpred, t_res_linpred_r)]

##  # Plots
##  p_qq <- ggplot(qres, aes(sample = residuals)) +
##          geom_qq(distribution = qunif, pch = 16, size = qq.point.size) +
##          geom_abline(slope = 1, intercept = 0, 
##                      size = line.size,
##                      col = col.theoretical) +
##          labs(title = "Uniform Q-Q",
##               x = "Theoretical quantiles",
##               y = "Quantile residuals") +
##          plot.theme
##  p_hist <- ggplot(qres, aes(x = bin/hist.bins - 1/(2*hist.bins), y = ..count..)) +
##            geom_hline(yintercept = nrow(qres) / hist.bins, 
##                       size = line.size,
##                       col = col.theoretical) +
##            geom_bar(width = 1/hist.bins) +
##            labs(title = paste0("Histogram of residuals (",
##                                hist.bins, " bins)"),
##                 x = "Quantile residuals",
##                 y = "Frequency") +
##            plot.theme
##  p_res_linpred <- ggplot(qres, aes(x = linpred, y = residuals)) +
##              geom_bin2d(bins = sqrt(1e5), show.legend = FALSE) +
##              #geom_hex(bins = sqrt(1e5), show.legend = FALSE) +
##              geom_hline(yintercept = 0.5, 
##                         size = line.size,
##                         col = col.theoretical) +
##              geom_line(data = t_res_linpred, 
##                        mapping = aes(y = trend), 
##                        linetype = "dashed",
##                        size = line.size,
##                        col = col.empirical) +
##              scale_fill_gradient(low = "grey90", high = "grey10", na.value = "grey90") +
##              labs(title = "Residuals vs. linear predictor",
##                   x = "Linear predictor",
##                   y = "Quantile residuals") +
##              plot.theme 
##  p_res_linpred_r <- ggplot(qres, aes(x = linpred_r, y = residuals)) +
##                     #geom_hex(bins = sqrt(1e5), show.legend = FALSE) +
##                     geom_bin2d(bins = sqrt(1e5), show.legend = FALSE) +
##                     geom_hline(yintercept = 0.5, 
##                                size = line.size,
##                                col = col.theoretical) +
##                     geom_line(data = t_res_linpred_r, 
##                               mapping = aes(y = trend),
##                               linetype = "dashed",
##                               size = line.size,
##                               col = col.empirical) +
##                     scale_fill_gradient2(low = "grey90", high = "grey10", na.value = "grey90") +
##                     #scale_fill_viridis_c()+
##                     labs(title = "Residuals vs. linear predictor (rank)",
##                          x = "Linear predictor (rank transformed)",
##                          y = "Quantile residuals") +
##                     plot.theme

##  # Arrange using `patchwork` package
##  p_arranged <- p_qq + p_hist + p_res_linpred  + p_res_linpred_r
##  return(p_arranged)
##  }

# -----------------------------------------------------------------------------


library(mgcv)
library(data.table)
library(posterior)
library(doParallel)
# library(ggplot2)
# library(patchwork)


link_cll <- function(mu) {
  log(-log(1 - mu))
}

invlink_cll <- function(eta) {
  pmax(pmin(-expm1(-exp(eta)), 1 - .Machine$double.eps), .Machine$double.eps)
}


# model_overview <- function(models, path = "../results/models/", prefix = "") {
#   # models: Character vector containing the names of the models to be loaded.
#   n <- length(models)
#   modres <- vector(mode = "list", length = n)
#   for(i in 1:n) {
#     fname <- paste0(path, prefix, models[i], ".rds")
#     mod <- readRDS(fname)
#     modres[[i]]$id <- models[i]
#     modres[[i]]$df <- nobs(mod) - df.residual(mod)
#     modres[[i]]$aic <- AIC(mod)
#     modres[[i]]$file <- fname
#     rm(mod)
#   }
#   return(rbindlist(modres))
# }


# sim_residuals <- function(models, path = "../results/models/", prefix = "", ...){
#   # models: Character vector containing the names of the models for which
#   #   residuals are to be simulated.
#   simulated <- vector(mode = "list", length = length(models))
#   for(i in 1:length(models)){
#     simulated[[i]]$id <- models[i]
#     simulated[[i]]$residuals <- FALSE
#     print(paste0("Simulating residuals for model ", models[i],
#                  " (", i, " of ", length(models), ")"))
#     modfit <- readRDS(paste0(path, prefix, models[i], ".rds"))
#     modres <- quantile_residuals(modfit, ...)
#     simulated[[i]]$residuals <- TRUE
#     save_residuals(residuals = modres, file = paste0(path, prefix, models[i], ".res"),
#                    rootpath = tempdir())
#     for (i in 1:length(modres$simulations)) {
#       delete(modres$simulations[[i]])
#     }
#     rm(modfit, modres)
#     gc()
#   }
#   return(simulated)
# }


# quantile_residuals <- function(model,
#                                n.sim = 1000, 
#                                sim.chunk = 200,
#                                obs = "all",
#                                posterior = TRUE, 
#                                row.chunk = 1e3, 
#                                on.disk = TRUE,
#                                storage.mode = "double",
#                                n.threads = 1,
#                                seed = NULL,
#                                progress = TRUE
#                                ) {

#   # Set RNG
#   if (!is.null(seed)) {
#     if (exists(".Random.seed")) {
#       prev.random.state <- .Random.seed
#     } else {
#       prev.random.state <- NULL
#     }
#     set.seed(seed)
#   }

#   if(is.numeric(obs)) {
#     n <- length(obs)
#     observed.response <- model.frame(model)[obs,1] 
#   } else {
#     n <- nobs(model)
#     observed.response <- model.frame(model)[,1] 
#   }
  
#   if(n.sim < sim.chunk) sim.chunk <- n.sim
 
#   sim.from <- seq(1, n.sim, sim.chunk)
#   if(length(sim.from) > 1) {
#     sim.to <- c(sim.from[2:length(sim.from)]-1, n.sim)
#   } else {
#     sim.to <- n.sim
#   }
#   sim.chunks <- sim.to - c(0, sim.to[-length(sim.to)])

#   # Simulate response and fill matrices
#   if(all(posterior == TRUE) | is.matrix(posterior)) {
#     if(is.matrix(posterior)) {
#       post <- posterior[1:n.sim,]
#     } else {
#       post <- mvnfast::rmvn(n = n.sim, mu = coefficients(model), 
#                             sigma = vcov(model, unconditional = TRUE),
#                             ncores = n.threads)
#       if(progress) print(paste0("Generated ", n.sim, " random draws from model posterior."))
#     }
#     if(progress) print("Simulating response ...")
#     simulations <- sim_post(model = model,
#                                  posterior = post,
#                                  obs = obs,
#                                  row.chunk = row.chunk,
#                                  post.chunk = sim.chunk, 
#                                  progress = progress, 
#                                  on.disk = on.disk,
#                                  storage.mode = storage.mode,
#                                  n.threads = n.threads)
#   } else {
#     simulations <- list()
#     if(progress) {
#       print("Simulating response ...")
#       prog <- txtProgressBar(min = 0, max = n.sim, initial = 0,
#                             char = "=", width = NA, title = "Progress", style = 3)
#     }
#     fam <- fix.family.rd(model$family)
#     wt <- model$prior.weights
#     scale <- model$sig2
#     fv <- fitted(model)
#     if(is.numeric(obs)) {
#       fv <- fv[obs]
#       wt <- wt[obs]
#     }
#     for (s in 1:length(sim.from)) {
#       if(on.disk) {
#         if(is.null(storage.mode)) storage.mode <- vmode(fitted(model))
#         simulations[[s]] <- ff(dim = c(n, sim.chunks[s]), vmode = storage.mode, 
#                                    pattern = paste0(tempdir(), "/qres"), finalizer = "delete")
#         for (i in 1:sim.chunks[s]) {
#           simulations[[s]][,i] <- fam$rd(fv, wt, scale)
#           if(progress) {
#             setTxtProgressBar(prog, sim.from[s] -1 + i)
#           }
#         } # end loop over columns
#       } else {
#         if(is.null(storage.mode)) storage.mode <- class(fitted(model))[1]
#         simulations[[s]] <- matrix(nrow = n, ncol = sim.chunks[s])
#         for (i in 1:sim.chunks[s]) {
#           simulations[[s]][,i] <- as(fam$rd(fv, wt, scale), storage.mode)
#           if(progress) {
#             setTxtProgressBar(prog, sim.from[s] -1 + i)
#           }
#         } # end loop over columns
#       } # end on.disk == FALSE
#     } # end loop over nodes
#     if(progress) close(prog)
#   }

#   if(progress) print("Calculating residuals ...")
    
#   sim.lower <- sim.upper <- quantile.residuals <- rep(0, n)
  
#   # Compute quantile residuals based on probability integral transform
#   # Same method (but different implementations) as in the DHARMa package
#   if(on.disk) {
#     for (m in 1:length(simulations)) {
#       sims <- simulations[[m]]
#       sim.lower <- sim.lower +
#         ffcolapply(rowSums(sims[,i1:i2] < observed.response),
#                               X=sims, RETURN = TRUE, CFUN = "csum") / n.sim
#       sim.upper <- sim.upper + 
#         ffcolapply(rowSums(sims[,i1:i2] <= observed.response),
#                                X=sims, RETURN = TRUE, CFUN = "csum") / n.sim
#     }
#   } else {
#     for (m in 1:length(simulations)) {
#      sims <- simulations[[m]]
#      sim.lower <- sim.lower + rowSums(apply(sims, MARGIN = 2,
#                                             function(x) x < observed.response)) / n.sim
#      sim.upper <- sim.upper + rowSums(apply(sims, MARGIN = 2,
#                                             function(x) x <= observed.response)) / n.sim
#     }
#   }

#   quantile_residuals <- mapply(function(lower, upper) { 
#                                if (lower == upper) lower else runif(1, lower, upper)},
#                                sim.lower, sim.upper, SIMPLIFY = TRUE, USE.NAMES = FALSE)

#   # Restore RNG
#   if (!is.null(seed)) {
#     .Random.seed <- prev.random.state
#   }

#   res <- list(n = n, seed = seed, simulations = simulations, quantile_residuals = quantile_residuals)
#   return(res)
# }

# save_residuals <- function(residuals, file, rootpath = getOption("fftempdir")) {
#   if(is.ff(residuals$simulations[[1]])){
#     ffsave_list(residuals$simulations, file = paste0(file, ".sim"), rootpath = rootpath)
#   }
#   saveRDS(residuals, file = paste0(file, ".rds"))
# }

# load_residuals <- function(models, path = "../results/models/", prefix = "", simulations = TRUE, overwrite = FALSE, env = .GlobalEnv){
#   # models: Character vector containing the names of the models for which
#   #   residuals are to be loaded.
#   for(i in 1:length(models)) {
#     rname <- paste0(prefix, models[i], ".res")
#     res <- readRDS(paste0(path, rname, ".rds"))
#     if (simulations) {
#       res$simulations <- ffload_list(file = paste0(path, rname, ".sim"), 
#                                      overwrite = overwrite, rootpath = tempdir())
#     } else {
#       res$simulations <- NA
#     }
#     assign(rname,
#            res,
#            pos = env)
#     rm(res)
#   }
# }


# unload_residuals <- function(models, prefix = "", env = .GlobalEnv){
#   obj <- ls(name = env)
#   mnames <- paste0(prefix, models)
#   res_rm <- obj[obj %in% paste0(mnames, ".res")]
#   # Properly delete ff objects
#   for (i in 1:length(res_rm)) {
#     res <- get(res_rm[i], pos = env)
#     for (s in 1:length(res$simulations)) {
#       delete(res$simulations[[s]])
#     }
#   }
#   # Remove R objects
#   rm(list = res_rm, pos = env)
# }


# sim_post <- function(model, 
#                      posterior, 
#                      newdata = NULL,
#                      obs = "all",
#                      coef = "all",
#                      row.chunk = 1e3,
#                      post.chunk = 200,
#                      on.disk = FALSE,
#                      storage.mode = NULL,
#                      n.threads = 1,
#                      discrete = FALSE,
#                      progress = TRUE
#                      ) {
#   # Same as eval_post, but simulation is done *before* ff matrix is
#   # filled to reduce disk writes.
#   if(is.null(dim(posterior))) {
#     posterior <- matrix(posterior, ncol = length(posterior))
#   } 
#   if(is.null(newdata)) {
#     data <- model.frame(model)
#   } else {
#     data <- newdata
#   }
#   if(is.numeric(obs)) {
#     data <- data[obs,]
#   }
#   n <- nrow(data)
#   m <- nrow(posterior)
#   fam <- fix.family.rd(model$family)
#   weights <- model$prior.weights
#   scale <- model$sig2
#   row.from <- seq(1, n, row.chunk)
#   if(length(row.from) > 1) {
#     row.to <- c(row.from[2:length(row.from)]-1, n)
#   } else {
#     row.to <- n
#   }
#   post.from <- seq(1, m, post.chunk)
#   if(length(post.from) > 1) {
#     post.to <- c(post.from[2:length(post.from)]-1, m)
#   } else {
#     post.to <- m
#   }
#   post.chunks <- post.to - c(0, post.to[-length(post.to)])
#   # Set excluded coefficients to 0
#     if(is.numeric(coef)) {
#       posterior[,-coef] <- 0
#     }
#   simulations <- list()
#   for(s in 1:length(post.from)) {
#     if(on.disk) {
#       if(is.null(storage.mode)) storage.mode <- vmode(fitted(model))
#       simulations[[s]] <- ff(dim = c(n, post.chunks[s]), vmode = storage.mode, 
#                                  pattern = paste0(tempdir(), "/qres"), finalizer = "delete")
#     } else {
#       if(is.null(storage.mode)) storage.mode <- class(fitted(model))[1]
#       simulations[[s]] <- matrix(as(0, storage.mode), nrow = n, ncol = post.chunks[s])
#     }
#   }
#   if(progress) {
#     prog <- txtProgressBar(min = 0, max = length(row.from), initial = 0,
#                           char = "=", width = NA, title = "Progress", style = 3)
#   }
#   for(i in 1:length(row.from)) {
#     Xp <- predict(model, 
#                   data[row.from[i]:row.to[i],],
#                   type = "lpmatrix",
#                   block.size = row.chunk,
#                   newdata.guaranteed = TRUE,
#                   n.threads = n.threads,
#                   discrete = discrete)
#     for(j in 1:length(post.from)) {
#       lp <- Xp %*% t(posterior[post.from[j]:post.to[j],])
#       if(on.disk) {
#         simulations[[j]][row.from[i]:row.to[i],] <- 
#           apply(fam$linkinv(lp), 2, fam$rd, 
#                 wt = weights[row.from[i]:row.to[i]],  
#                 scale = scale)
#       } else {
#         simulations[[j]][row.from[i]:row.to[i],] <- 
#           as(apply(fam$linkinv(lp), 2, fam$rd, 
#                    wt = weights[row.from[i]:row.to[i]],  
#                    scale = scale),
#              storage.mode)
#       }
#       rm(lp)
#     }
#     rm(Xp)
#     gc()
#     if(progress) {
#       setTxtProgressBar(prog, i)
#     }
#   }
#   if(progress) close(prog)
#   return(simulations)
# }

chunk_seq <- function(from, to, size = to) {
  chunk.from <- seq(from, to, size)
  if(length(chunk.from) > 1) {
    chunk.to <- c(chunk.from[2:length(chunk.from)]-1, to)
  } else {
    chunk.to <- to
  }
  chunk.size <- chunk.to - c(0, chunk.to[-length(chunk.to)])
  return(list(from = chunk.from,
              to = chunk.to,
              size = chunk.size))
}

lp_matrix <- 
  function(
           model, 
           newdata = NULL,
           id.col = NULL,
           obs = NULL,
           predict.chunk = NULL,
           progress = TRUE,
           ...
           ) {
  if(is.null(newdata)) {
    data <- model.frame(model)
  } else {
    data <- newdata
  }
  if(is.numeric(obs)) {
    data <- data[obs,]
  }
  n <- nrow(data)
  m <- length(coef(model))
  if(is.null(predict.chunk)) predict.chunk <- n
  predict.chunks <- chunk_seq(1, n, predict.chunk)
  # Set excluded coefficients to 0
  lpmatrix <- matrix(nrow = n, ncol = m)
  if(!is.null(id.col)) {
    rownames(lpmatrix) <- data[[id.col]]
  } else {
    rownames(lpmatrix) <- 1:nrow(data)
  }
  colnames(lpmatrix) <- names(coef(model))
  if(progress) {
    prog <- txtProgressBar(min = 0, max = length(predict.chunks$from), initial = 0,
                           char = "=", width = NA, title = "Progress", style = 3)
  }
  for(i in 1:length(predict.chunks$from)) {
    Xp <- predict(model, 
                  data[predict.chunks$from[i]:predict.chunks$to[i],],
                  type = "lpmatrix",
                  block.size = predict.chunks$size[i],
                  newdata.guaranteed = TRUE,
                  cluster = NULL,
                  ...)
    lpmatrix[(predict.chunks$from[i]:predict.chunks$to[i]), ] <- Xp
    rm(Xp)
    gc()
    if(progress) {
      setTxtProgressBar(prog, i)
    }
  }
  if(progress) close(prog)
  return(lpmatrix)
}

evaluate_posterior <- 
  function(
           model, 
           posterior,
           newdata = NULL,
           id.col = NULL,
           type = "link",
           obs = NULL,
           coef = NULL,
           marginals = NULL,
           predict.chunk = NULL,
           post.chunk = NULL,
           progress = TRUE
           ) {
  if(is.null(dim(posterior))) {
    posterior <- matrix(posterior, ncol = length(posterior))
  }
  if(is.null(newdata)) {
    data <- model.frame(model)
  } else {
    data <- newdata
  }
  if(is.numeric(obs)) {
    data <- data[obs,]
  }
  n <- nrow(data)
  m <- nrow(posterior)
  if(is.null(predict.chunk)) predict.chunk <- n
  if(is.null(post.chunk)) post.chunk <- m
  predict.chunks <- chunk_seq(1, n, predict.chunk)
  post.chunks <- chunk_seq(1, m, post.chunk)
  # Set excluded coefficients to 0
  if(is.numeric(coef)) {
    posterior[,-coef] <- 0
  }
  if(is.null(marginals)) {
    marginals <- list(1:ncol(posterior))
  }
  evaluated <- array(dim = c(n, m, length(marginals)))
  if(!is.null(id.col)) {
    dimnames(evaluated)[1] <- list(data[[id.col]])
  } else {
    dimnames(evaluated)[1] <- list(1:nrow(data))
  }
  if(progress) {
    prog <- txtProgressBar(min = 0, max = length(predict.chunks$from), initial = 0,
                           char = "=", width = NA, title = "Progress", style = 3)
  }
  for(i in 1:length(predict.chunks$from)) {
    Xp <- predict(model, 
                  data[predict.chunks$from[i]:predict.chunks$to[i],],
                  type = "lpmatrix",
                  block.size = predict.chunks$size[i],
                  newdata.guaranteed = TRUE,
                  cluster = NULL)
    for(j in 1:length(marginals)) {
    m.predict.chunk <- matrix(nrow = predict.chunks$size[i],
                              ncol = m)
      for(k in 1:length(post.chunks$from)) {
        lp <- Xp[, marginals[[j]]] %*% t(posterior[post.chunks$from[k]:post.chunks$to[k],
                                               marginals[[j]]])
        if(type == "response") {
          fam <- fix.family.rd(model$family)
          m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- fam$linkinv(lp)
        } else {
          m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- lp
        }
        rm(lp)
      }
    evaluated[(predict.chunks$from[i]:predict.chunks$to[i]), , j] <- m.predict.chunk
    rm(m.predict.chunk)
    }
    rm(Xp)
    gc()
    if(progress) {
      setTxtProgressBar(prog, i)
    }
  }
  if(progress) close(prog)
  evaluated <- lapply(seq(dim(evaluated)[3]),
                        \(x) {y <- evaluated[ , , x]
                              dimnames(y)[1:2] <- dimnames(evaluated)[1:2]
                              return(as_draws_matrix(t(y)))})
  if(length(marginals) == 1) {
    return(evaluated[[1]])
  } else {
    names(evaluated) <- names(marginals)
    return(evaluated)
  }
}

summarize_predictions <- function(x, ...) {
  UseMethod("summarize_predictions", x)
}

summarize_predictions.draws_matrix <- 
  function(predictions,
           fun = mean,
           ids = NULL,
           draw.chunk = NULL,
           n.cores = 1,
           ...) {
  if(is.null(ids)) {
    ids <- 1:ncol(predictions)
  }
  idx <- which(colnames(predictions) %in% ids)
  if(is.null(draw.chunk)) {
    draw.chunk <- nrow(predictions)
  }
  draw.chunks <- chunk_seq(1, nrow(predictions), draw.chunk)
  registerDoParallel(cores = n.cores)
  pred_summarized <-
    foreach(i = 1:length(draw.chunks$from),
            .combine = c) %dopar% {
      pred_summarized_chunk <- 
        apply(predictions[draw.chunks$from[i]:draw.chunks$to[i],idx], 1, fun)
    }
  stopImplicitCluster()
  return(rvar(pred_summarized))
}

summarize_predictions.FileSystemDataset <- 
  function(predictions,
           fun = mean,
           ids = NULL,
           id.col = NULL,
           draw.prefix = "draw",
           draw.chunk = NULL,
           n.cores = 1,
           ...) {
  if(is.null(ids)) {
    ids <- 1:nrow(predictions)
  }
  if(is.null(id.col)) {
    source.ids <- 1:nrow(predictions)
  } else {
    source.ids <- as.data.frame(predictions[, id.col])[[id.col]]
  }
  idx <- which(source.ids %in% ids)
  if(is.null(draw.prefix)) {
    draw.cols <- 1:ncol(predictions)
  } else {
    draw.cols <- grep(draw.prefix, names(predictions))
  }
  if(is.null(draw.chunk)) {
    draw.chunk <- length(draw.cols)
  }
  draw.chunks <- chunk_seq(1, length(draw.cols), draw.chunk)
  registerDoParallel(cores = n.cores)
  predictions.summarized <-
    foreach(i = 1:length(draw.chunks$from),
            .combine = c) %dopar% {
      predictions.pulled <- 
        as.data.frame(predictions[idx,
                                  draw.cols[draw.chunks$from[i]:draw.chunks$to[i]]])
      predictions.summarized.chunk <- 
        apply(t(as.matrix(predictions.pulled)), 1, fun)
      return(predictions.summarized.chunk)
    }
  stopImplicitCluster()
  return(rvar(predictions.summarized))
}


summarize_predictions_by <- 
  function(predictions,
           data = NULL,
           fun = mean,
           id.col = NULL,
           group.vars = NULL,
           group.labels = NULL,
           draw.prefix = "draw",
           draw.chunk = NULL,
           n.cores = 1,
           ...) {
  setDT(data)
  if(is.null(id.col)) {
    groups <- data[, .(n = .N, ids = list(.I)), group.vars]
  } else {
    groups <- data[, .(n = .N, ids = list(data$id[.I])), group.vars]
  }
  if(!is.null(group.labels)) {
    for(i in 1:length(group.vars)) {
      old.labels <- as.character(groups[[group.vars[i]]])
      new.labels <- factor(group.labels[[i]][old.labels], 
                           levels = group.labels[[i]])
      groups[[group.vars[i]]] <- new.labels
    }
  }
  setorderv(groups, group.vars)
  summary.labels <- do.call(paste, c(groups[, 1:length(group.vars)] , sep = "."))
  groups.summarized <- list()
  for(i in 1:nrow(groups)) {
    group.summary <- 
      summarize_predictions(predictions,
                            fun = fun,
                            ids = unlist(groups[i, ids]),
                            id.col = id.col,
                            draw.prefix = draw.prefix,
                            draw.chunk = draw.chunk,
                            n.cores = n.cores
                            # , ...
                            )
    groups.summarized[[summary.labels[i]]] <- group.summary
  }
  return(as_draws_matrix(groups.summarized))
}


bin_cols <- function(data, columns, bin.res, bin.min = NULL, round = NULL, append = FALSE) {
  bins.l <- vector(mode = "list", length = length(columns))
  for (i in 1:length(columns)) {
    if(is.null(bin.min)) {
      c.min <- min(data[[columns[i]]])
    } else {
      c.min <- bin.min[i]
    }
    c.max <- max(data[[columns[i]]])
    if (is.null(round)) {
      b.lower <- c.min - bin.res[i]
      b.upper <- c.max + bin.res[i]
    } else {
      b.lower <- round(c.min, round) - bin.res[i]
      b.upper <- round(c.max, round) + bin.res[i]
    }
    b.breaks <- seq(from = b.lower, 
                    to =  b.upper, 
                    by = bin.res[i])
    b.center <- b.breaks[1:(length(b.breaks)-1)] + bin.res[i] / 2 
    cuts <- cut(data[[columns[i]]], breaks = b.breaks, labels = FALSE)
    binned <- b.center[cuts]
    b.col <- paste0(columns[i], ".bin")
    bins.l[[b.col]] <- binned
  }
  return(bins.l)
}

#diag_residuals <- function(model, 
#                           residuals,
#                           sample = NULL,
#                           hist.bins = 100,
#                           trend.res = 1000,
#                           trend.k = 10,
#                           col.empirical = palette.colors(palette = "Set 1")[1],
#                           col.theoretical = palette.colors(palette = "Set 1")[2],
#                           qq.point.size = 1.5,
#                           line.size = 0.8,
#                           plot.theme = theme_classic()
#                           ) 
#  {
#  # Extract residuals and fitted values
#  if(is.null(sample)){
#    qres <- data.table(residuals = residuals$quantile_residuals,
#                       linpred = predict(model, type = "link",
#                                         newdata.guaranteed = TRUE),
#                       response =  model$model[,1])
#  } else {
#    qres <- data.table(residuals = residuals$quantile_residuals[sample],
#                       linpred = predict(model, newdata = model$model[sample,],
#                                         type = "link",
#                                         newdata.guaranteed = TRUE),
#                       response =  model$model[sample, 1],
#    fitted = fitted(model)[sample])
#  }

#  # Preparatory calculations
#  # Histogram
#  qres[, bin := cut(residuals, seq(0, 1, 1/hist.bins), labels = FALSE)]

#  # Residuals vs. linear predictor
#  qres[, linpred_r := frank(linpred)]

#  # Model for smooth trend
#  # m_res_linpred <- gam(residuals ~ s(predicted), data = qres, select = TRUE)
#  m_res_linpred <- bam(residuals ~ s(linpred, k = trend.k), 
#                    data = qres, 
#                    select = TRUE, 
#                    discrete = TRUE)

#  # Trend for untransformed predictor
#  t_res_linpred <- data.table(linpred = seq(from = min(qres$linpred), 
#                                         to = max(qres$linpred), 
#                                         length.out = trend.res))
#  t_res_linpred[,trend := predict(m_res_linpred, t_res_linpred)]

#  # Trend for rank-transformed predictor
#  quantiles <- (1:(trend.res -1)) / (trend.res - 1)
#  t_res_linpred_r <- data.table(linpred = c(min(qres$linpred), 
#                                       quantile(qres$linpred, quantiles)),
#                           linpred_r = (0:(trend.res - 1) / (trend.res - 1) * 
#                                    (length(qres$linpred)  - 1)) + 1
#                           )
#  t_res_linpred_r[,trend := predict(m_res_linpred, t_res_linpred_r)]

#  # Plots
#  p_qq <- ggplot(qres, aes(sample = residuals)) +
#          geom_qq(distribution = qunif, pch = 16, size = qq.point.size) +
#          geom_abline(slope = 1, intercept = 0, 
#                      size = line.size,
#                      col = col.theoretical) +
#          labs(title = "Uniform Q-Q",
#               x = "Theoretical quantiles",
#               y = "Quantile residuals") +
#          plot.theme
#  p_hist <- ggplot(qres, aes(x = bin/hist.bins - 1/(2*hist.bins), y = ..count..)) +
#            geom_hline(yintercept = nrow(qres) / hist.bins, 
#                       size = line.size,
#                       col = col.theoretical) +
#            geom_bar(width = 1/hist.bins) +
#            labs(title = paste0("Histogram of residuals (",
#                                hist.bins, " bins)"),
#                 x = "Quantile residuals",
#                 y = "Frequency") +
#            plot.theme
#  p_res_linpred <- ggplot(qres, aes(x = linpred, y = residuals)) +
#              geom_bin2d(bins = sqrt(1e5), show.legend = FALSE) +
#              #geom_hex(bins = sqrt(1e5), show.legend = FALSE) +
#              geom_hline(yintercept = 0.5, 
#                         size = line.size,
#                         col = col.theoretical) +
#              geom_line(data = t_res_linpred, 
#                        mapping = aes(y = trend), 
#                        linetype = "dashed",
#                        size = line.size,
#                        col = col.empirical) +
#              scale_fill_gradient(low = "grey90", high = "grey10", na.value = "grey90") +
#              labs(title = "Residuals vs. linear predictor",
#                   x = "Linear predictor",
#                   y = "Quantile residuals") +
#              plot.theme 
#  p_res_linpred_r <- ggplot(qres, aes(x = linpred_r, y = residuals)) +
#                     #geom_hex(bins = sqrt(1e5), show.legend = FALSE) +
#                     geom_bin2d(bins = sqrt(1e5), show.legend = FALSE) +
#                     geom_hline(yintercept = 0.5, 
#                                size = line.size,
#                                col = col.theoretical) +
#                     geom_line(data = t_res_linpred_r, 
#                               mapping = aes(y = trend),
#                               linetype = "dashed",
#                               size = line.size,
#                               col = col.empirical) +
#                     scale_fill_gradient2(low = "grey90", high = "grey10", na.value = "grey90") +
#                     #scale_fill_viridis_c()+
#                     labs(title = "Residuals vs. linear predictor (rank)",
#                          x = "Linear predictor (rank transformed)",
#                          y = "Quantile residuals") +
#                     plot.theme

#  # Arrange using `patchwork` package
#  p_arranged <- p_qq + p_hist + p_res_linpred  + p_res_linpred_r
#  return(p_arranged)
#  }

init_som <- function(data, xdim, ydim) {
  # Calculate principal components
  init_pca <- prcomp(x = data, center = FALSE, scale = FALSE)
  init_max <- apply(init_pca$x[, 1:2], 2, max)
  init_min <- apply(init_pca$x[, 1:2], 2, min)
  # Distribute nodes along first two PC
  init_coord_pc <- matrix(NA, nrow = xdim * ydim, ncol = 2)
  init_coord_pc[, 1] <-  rep(seq(init_min[1], init_max[1], 
                                  length.out = xdim),
                              times = ydim)
  init_coord_pc[, 2] <-  rep(seq(init_min[2], init_max[2], 
                                  length.out = ydim),
                              each = ydim)
  # Map to covariate space
  init_coord_cov <- init_coord_pc %*% t(init_pca$rotation[,1:2])
  return(init_coord_cov)
}

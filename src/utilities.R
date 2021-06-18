library(mgcv)
library(ff)
library(data.table)
library(ggplot2)
library(patchwork)

fit_models <- function(models, data, chunk.size = 1e6, path = "results/models/", prefix = "", subset = NULL, summary = FALSE, gc.level = 0){
  # data: A `data.frame` containing the response and predictor variables
  #   as columns. 
  # models: A `list` with the following elements: `id` (character), `response`
  #   (character) for the response variable, `predictor` (character) for the
  #   predictor side of the R model formula, `link` (character) for the link
  #   function to be used. `select` (boolean), `drop.intercept` (boolean),
  #   `paraPen` (list) correspond to the respective arguments to `bam()`.
  n <- length(models[[1]])
  m <- ifelse(is.null(subset), n, length(subset))
  cm <- 1
  # List to hold basic information to be returned by function call
  modres <- vector(mode = "list", length = n)
  withCallingHandlers({
      for (i in 1:n) {
        if(!models$id[i] %in% subset & !is.null(subset)) next
        print(paste0("[", Sys.time(), "] ",
                     "Fitting model ", models$id[i], " (", cm, " of ", m, ")"))
        cm <- cm + 1
        fit_warnings <- character(0)
        modres[[i]]$id <- models$id[i]
        modres[[i]]$fitted <- FALSE
        modres[[i]]$df <- 0
        modres[[i]]$aic <- 0
        modres[[i]]$warnings <- 0
        modres[[i]]$time <- 0
        modres[[i]]$file <- paste0(path, prefix, models$id[i], ".rds")
        t.s <- as.numeric(Sys.time())
        modfit <- bam(as.formula(paste0(models$response[i], " ~ ", models$predictor[i])),
                      family = binomial(link = models$link[i]),
                      data = data,
                      drop.intercept = models$drop.intercept[i],
                      # gamma = models$gamma[i],
                      select = models$select[i],
                      paraPen = models$paraPen[i],
                      chunk.size = chunk.size,
                      discrete = TRUE,
                      nthreads = c(2,1),
                      gc.level = gc.level
        )
        t.e <- as.numeric(Sys.time())
        modfit$elapsed <- t.e - t.s
        modfit$warnings <- fit_warnings
        saveRDS(modfit, modres[[i]]$file)
        modres[[i]]$fitted <- TRUE
        modres[[i]]$df <- nobs(modfit) - df.residual(modfit)
        modres[[i]]$aic <- AIC(modfit)
        modres[[i]]$warnings <- length(fit_warnings)
        modres[[i]]$time <- modfit$elapsed 
        if(summary) {
          print(paste0("[", Sys.time(), "] ","Calculating summary …"))
          modsum <- summary(modfit)
          saveRDS(modsum, paste0(path, prefix, models$id[i], ".sum.rds"))
        }
        rm(modfit)
        gc()
      }
    }, 
    warning = function(x) {
      fit_warnings <<- append(fit_warnings, x$message)
      invokeRestart("muffleWarning")
    })
  return(modres)
}

load_models <- function(models, 
                        path = "results/models/",
                        prefix = "",
                        rename = NULL,
                        summary = TRUE,
                        env = .GlobalEnv)
  {
  # models: Character vector containing the names of the models to be loaded.
  mname <- paste0(prefix, models)
  if(is.null(rename)) {
    mname_assign <- mname
  } else {
    mname_assign <- rename
  }
  for(i in 1:length(models)) {
    assign(mname_assign[i],
           readRDS(paste0(path, mname[i], ".rds")),
           pos = env)
    if(summary) {
      if(file.exists(paste0(path, mname[i], ".sum.rds"))) {
        assign(paste0(mname_assign[i], ".sum"),
               readRDS(paste0(path, mname[i], ".sum.rds")),
               pos = env)
      } else {
        assign(paste0(mname_assign[i], ".sum"),
               summary(get(mname[i], envir = env)),
               pos = env)
        saveRDS(get(paste0(mname[i], ".sum")), paste0(path, mname[i], ".sum.rds"))
      }
    } # end summary
  } # end loop
}

load_summaries <- function(models, path = "results/models/", prefix = "", env = .GlobalEnv){
  # models: Character vector containing the names of the models to be loaded.
  for(i in 1:length(models)) {
    mname <- paste0(prefix, models[i])
    if(file.exists(paste0(path, mname, ".sum.rds"))) {
      assign(paste0(mname, ".sum"),
             readRDS(paste0(path, mname, ".sum.rds")),
             pos = env)
    } else {
      mod <- readRDS(paste0(path, mname, ".rds"))
      assign(paste0(mname, ".sum"),
             summary(mod),
             pos = env)
      saveRDS(get(paste0(mname, ".sum")), paste0(path, mname, ".sum.rds"))
      rm(mod)
    }
  } # end loop
}

model_overview <- function(models, path = "results/models/", prefix = "") {
  # models: Character vector containing the names of the models to be loaded.
  n <- length(models)
  modres <- vector(mode = "list", length = n)
  for(i in 1:n) {
    fname <- paste0(path, prefix, models[i], ".rds")
    mod <- readRDS(fname)
    modres[[i]]$id <- models[i]
    modres[[i]]$df <- nobs(mod) - df.residual(mod)
    modres[[i]]$aic <- AIC(mod)
    modres[[i]]$warnings <- length(mod$fit_warnings)
    modres[[i]]$time <- mod$elapsed 
    modres[[i]]$file <- fname
    rm(mod)
  }
  return(rbindlist(modres))
}


unload_models <- function(models, prefix = "", env = .GlobalEnv){
  obj <- ls(name = env)
  mnames <- paste0(prefix, models)
  mod_rm <- obj[obj %in% c(mnames,
                    paste0(mnames, ".res"),
                    paste0(mnames, ".sum"))]
  rm(list = mod_rm, pos = env)
}


sim_residuals <- function(models, path = "results/models/", prefix = "", ...){
  # models: Character vector containing the names of the models for which
  #   residuals are to be simulated.
  simulated <- vector(mode = "list", length = length(models))
  for(i in 1:length(models)){
    simulated[[i]]$id <- models[i]
    simulated[[i]]$residuals <- FALSE
    print(paste0("Simulating residuals for model ", models[i],
                 " (", i, " of ", length(models), ")"))
    modfit <- readRDS(paste0(path, prefix, models[i], ".rds"))
    modres <- quantile_residuals(modfit, ...)
    simulated[[i]]$residuals <- TRUE
    save_residuals(residuals = modres, file = paste0(path, prefix, models[i], ".res"),
                   rootpath = tempdir())
    for (i in 1:length(modres$simulations)) {
      delete(modres$simulations[[i]])
    }
    rm(modfit, modres)
  }
  return(simulated)
}


quantile_residuals <- function(model,
                               n.sim = 1000, 
                               posterior = FALSE, 
                               obs = "all",
                               row.chunk = 1e3, 
                               post.chunk = 200,
                               on.disk = TRUE,
                               n.threads = 1,
                               seed = NULL,
                               progress = TRUE
                               ) {

  # Set RNG
  if (!is.null(seed)) {
    if (exists(".Random.seed")) {
      prev.random.state <- .Random.seed
    } else {
      prev.random.state <- NULL
    }
    set.seed(seed)
  }

  if(is.numeric(obs)) {
    n <- length(obs)
    observed.response <- model.frame(model)[obs,1] 
  } else {
    n <- nobs(model)
    observed.response <- model.frame(model)[,1] 
  }
  
  if(n.sim < post.chunk) post.chunk <- n.sim
 
  post.from <- seq(1, n.sim, post.chunk)
  if(length(post.from) > 1) {
    post.to <- c(post.from[2:length(post.from)]-1, n.sim)
  } else {
    post.to <- n.sim
  }
  post.chunks <- post.to - c(0, post.to[-length(post.to)])

  # Simulate response and fill matrices
  if(all(posterior == TRUE) | is.matrix(posterior)) {
    if(is.matrix(posterior)) {
      post <- posterior
    } else {
      post <- mvnfast::rmvn(n = n.sim, mu = coefficients(model), 
                            sigma = vcov(model, unconditional = TRUE),
                            ncores = n.threads)
      if(progress) print(paste0("Generated ", n.sim, " random draws from model posterior."))
    }
    if(progress) print("Simulating response ...")
    simulations <- sim_post(model = model,
                                 posterior = post,
                                 obs = obs,
                                 row.chunk = row.chunk,
                                 post.chunk = post.chunk, 
                                 progress = progress, 
                                 on.disk = on.disk, 
                                 n.threads = n.threads)
  } else {
    simulations <- list()
    if(progress) {
      print("Simulating response ...")
      prog <- txtProgressBar(min = 0, max = n.sim, initial = 0,
                            char = "=", width = NA, title = "Progress", style = 3)
    }
    for (s in 1:length(post.from)) {
      if(on.disk) {
        simulations[[s]] <- ff(dim = c(n, post.chunks[s]), vmode = vmode(fitted(model)), 
                                   pattern = paste0(tempdir(), "/qres"), finalizer = "delete")
      } else {
        simulations[[s]] <- matrix(nrow = n, ncol = post.chunks[s])
      }
      for (i in 1:post.chunks[s]) {
        if(is.numeric(obs)) {
          simulations[[s]][,i] <- as.matrix(simulate(model, 1)[obs,])
        } else {
          simulations[[s]][,i] <- as.matrix(simulate(model, 1))
        }
        if(progress) {
          setTxtProgressBar(prog, post.from[s] -1 + i)
        }
      }
    }
    if(progress) close(prog)
  }

  if(progress) print("Calculating residuals ...")
    
  sim.lower <- sim.upper <- quantile.residuals <- rep(0, n)
  
  # Compute quantile residuals based on probability integral transform
  # Same method (but different implementations) as in the DHARMa package
  if(on.disk) {
    for (m in 1:length(simulations)) {
      sims <- simulations[[m]]
      sim.lower <- sim.lower +
        ffcolapply(rowSums(sims[,i1:i2] < observed.response),
                              X=sims, RETURN = TRUE, CFUN = "csum") / n.sim
      sim.upper <- sim.upper + 
        ffcolapply(rowSums(sims[,i1:i2] <= observed.response),
                               X=sims, RETURN = TRUE, CFUN = "csum") / n.sim
    }
  } else {
    for (m in 1:length(simulations)) {
     sims <- simulations[[m]]
     sim.lower <- sim.lower + rowSums(apply(sims, MARGIN = 2,
                                            function(x) x < observed.response)) / n.sim
     sim.upper <- sim.upper + rowSums(apply(sims, MARGIN = 2,
                                            function(x) x <= observed.response)) / n.sim
    }
  }

  quantile_residuals <- mapply(function(lower, upper) { 
                               if (lower == upper) lower else runif(1, lower, upper)},
                               sim.lower, sim.upper, SIMPLIFY = TRUE, USE.NAMES = FALSE)

  # Restore RNG
  if (!is.null(seed)) {
    .Random.seed <- prev.random.state
  }

  res <- list(n = n, seed = seed, simulations = simulations, quantile_residuals = quantile_residuals)
  return(res)
}

save_residuals <- function(residuals, file, rootpath = getOption("fftempdir")) {
  ffsave_list(residuals$simulations, file = paste0(file, ".sim"), rootpath = rootpath)
  saveRDS(residuals, file = paste0(file, ".rds"))
}

load_residuals <- function(models, path = "results/models/", prefix = "", simulations = TRUE, overwrite = FALSE, env = .GlobalEnv){
  # models: Character vector containing the names of the models for which
  #   residuals are to be loaded.
  for(i in 1:length(models)) {
    rname <- paste0(prefix, models[i], ".res")
    res <- readRDS(paste0(path, rname, ".rds"))
    if (simulations) {
      res$simulations <- ffload_list(file = paste0(path, rname, ".sim"), 
                                     overwrite = overwrite, rootpath = tempdir())
    } else {
      res$simulations <- NA
    }
    assign(rname,
           res,
           pos = env)
    rm(res)
  }
}


unload_residuals <- function(models, prefix = "", env = .GlobalEnv){
  obj <- ls(name = env)
  mnames <- paste0(prefix, models)
  res_rm <- obj[obj %in% paste0(mnames, ".res")]
  # Properly delete ff objects
  for (i in 1:length(res_rm)) {
    res <- get(res_rm[i], pos = env)
    for (s in 1:length(res$simulations)) {
      delete(res$simulations[[s]])
    }
  }
  # Remove R objects
  rm(list = res_rm, pos = env)
}


ffsave_list <- function(list, file, rootpath = getOption("fftempdir")) {
  savesims <- new.env()
  for(i in 1:length(list)){
    assign(paste0("sim", i), list[[i]], pos=savesims)
  }
  ffsave(list = ls(savesims), file = file, envir = savesims, rootpath = rootpath)
  rm(savesims)
}

ffload_list <- function(file, overwrite = FALSE, rootpath = getOption("fftempdir")) {
  loadsims <- new.env()
  ffload(file, envir = loadsims, overwrite = overwrite, rootpath = rootpath)
  loaded <- as.list(loadsims)
  rm(loadsims)
  return(loaded)
}

sim_post <- function(model, 
                     posterior, 
                     newdata = NULL,
                     obs = "all",
                     coef = "all",
                     row.chunk = 1e3,
                     post.chunk = 200,
                     on.disk = FALSE,
                     n.threads = 1,
                     discrete = FALSE,
                     progress = TRUE
                     ) {
  # Same as eval_post, but simulation is done *before* ff matrix is
  # filled to reduce disk writes.
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
  fam <- fix.family.rd(model$family)
  weights <- model$prior.weights
  scale <- model$sig2
  row.from <- seq(1, n, row.chunk)
  if(length(row.from) > 1) {
    row.to <- c(row.from[2:length(row.from)]-1, n)
  } else {
    row.to <- n
  }
  post.from <- seq(1, m, post.chunk)
  if(length(post.from) > 1) {
    post.to <- c(post.from[2:length(post.from)]-1, m)
  } else {
    post.to <- m
  }
  post.chunks <- post.to - c(0, post.to[-length(post.to)])
  # Set excluded coefficients to 0
    if(is.numeric(coef)) {
      posterior[,-coef] <- 0
    }
  simulations <- list()
  for(s in 1:length(post.from)) {
    if(on.disk) {
      simulations[[s]] <- ff(dim = c(n, post.chunks[s]), vmode = vmode(fitted(model)), 
                                 pattern = paste0(tempdir(), "/qres"), finalizer = "delete")
    } else {
      simulations[[s]] <- matrix(nrow = n, ncol = post.chunks[s])
    }
  }
  if(progress) {
    prog <- txtProgressBar(min = 0, max = length(row.from), initial = 0,
                          char = "=", width = NA, title = "Progress", style = 3)
  }
  for(i in 1:length(row.from)) {
    Xp <- predict(model, 
                  data[row.from[i]:row.to[i],],
                  type = "lpmatrix",
                  block.size = row.chunk,
                  newdata.guaranteed = TRUE,
                  n.threads = n.threads,
                  discrete = discrete)
    for(j in 1:length(post.from)) {
      lp <- Xp %*% t(posterior[post.from[j]:post.to[j],])
      simulations[[j]][row.from[i]:row.to[i],] <- 
        apply(fam$linkinv(lp), 2, fam$rd, 
            wt = weights[row.from[i]:row.to[i]],  scale = scale)
      rm(lp)
    }
    rm(Xp)
    gc()
    if(progress) {
      setTxtProgressBar(prog, i)
    }
  }
  if(progress) close(prog)
  return(simulations)
}


eval_post <- function(
                      model, 
                      posterior,
                      newdata = NULL,
                      type = "link",
                      obs = "all",
                      coef = "all",
                      row.chunk = 1e3,
                      post.chunk = 200,
                      on.disk = FALSE,
                      n.threads = 1,
                      discrete = FALSE,
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
  weights <- model$prior.weights
  scale <- model$sig2
  row.from <- seq(1, n, row.chunk)
  if(length(row.from) > 1) {
    row.to <- c(row.from[2:length(row.from)]-1, n)
  } else {
    row.to <- n
  }
  post.from <- seq(1, m, post.chunk)
  if(length(post.from) > 1) {
    post.to <- c(post.from[2:length(post.from)]-1, m)
  } else {
    post.to <- m
  }
  post.chunks <- post.to - c(0, post.to[-length(post.to)])
  # Set excluded coefficients to 0
  if(is.numeric(coef)) {
    posterior[,-coef] <- 0
  }
  evaluated <- list()
  for(node in 1:length(post.from)) {
    if(on.disk) {
      evaluated[[node]] <- ff(dim = c(n, post.chunks[node]), vmode = vmode(fitted(model)), 
                                 pattern = paste0(tempdir(), "/epos"), finalizer = "delete")
    } else {
      evaluated[[node]] <- matrix(nrow = n, ncol = post.chunks[node])
    }
  }
  if(progress) {
    prog <- txtProgressBar(min = 0, max = length(row.from), initial = 0,
                          char = "=", width = NA, title = "Progress", style = 3)
  }
  for(i in 1:length(row.from)) {
    Xp <- predict(model, 
                  data[row.from[i]:row.to[i],],
                  type = "lpmatrix",
                  block.size = row.chunk,
                  newdata.guaranteed = TRUE,
                  n.threads = n.threads,
                  discrete = discrete)
    for(j in 1:length(post.from)) {
      lp <- Xp %*% t(posterior[post.from[j]:post.to[j],])
      if(type == "response") {
        fam <- fix.family.rd(model$family)
        evaluated[[j]][row.from[i]:row.to[i],] <- fam$linkinv(lp)
      } else {
        evaluated[[j]][row.from[i]:row.to[i],] <- lp
      }
      rm(lp)
    }
    rm(Xp)
    gc()
    if(progress) {
      setTxtProgressBar(prog, i)
    }
  }
  if(progress) close(prog)
  return(evaluated)
}

hratio <- function(model, type = "joint", unconditional = TRUE, ci = TRUE) {
  # Calculates the hazard ratio for parametric effects.
  
  V <- vcov(model, unconditional = unconditional)
  # Parametric effects 
  ip <- 1:model$nsdf
  coef <- model$coefficients[ip]
  se <- diag(V)[ip]

  if (type == "joint") {
    # Marginal coefficients and SE
    mod_terms <- data.table(term = names(coef),
                            coef.m = coef,
                            se.m = se)
    # Joint coefficients and SE for interaction + main effects
    interactions <- mod_terms[grep(":", term),"term"]
    interactions[, c("main1", "main2") := tstrsplit(term, "\\:")]
    for (i in 1:nrow(interactions)) {
      int <- interactions[i, term]
      main <- interactions[i, c(main1, main2)]
      interactions$coef.j[i] <- sum(mod_terms[term %in% main, coef.m], 
                                    mod_terms[term == int, coef.m])
      interactions$se.j[i] <- sqrt(sum(V[c(int, main), c(int, main)]))
    }
    
    mod_terms <- interactions[mod_terms, on = "term"]
    mod_terms[term == "(Intercept)", type := "intercept"]
    mod_terms[term != "(Intercept)", type := ifelse(is.na(main1), 
                                                    "main", "interaction")]
    mod_terms[is.na(coef.j), 
              `:=`(coef.j = coef.m,
                   se.j = se.m)]
    setcolorder(mod_terms,
                c("type", "term", "main1", "main2",
                  "coef.m", "se.m", "coef.j", "se.j"))

    mod_terms[,
              `:=`(hratio.m = exp(coef),
                   hratio = exp(coef.j))]
    if (ci) {
      mod_terms[,
                `:=`(hratio.ciu = exp(coef.j + 1.96 * se.j),
                     hratio.cil = exp(coef.j - 1.96 * se.j))
                ]
    }
  } else { 
    mod_terms <- data.table(term = names(coef), coef, se)
    mod_terms[, hratio := exp(coef)]
    
    if (ci) {
      mod_terms[,`:=`(hratio.ciu = exp(coef + 1.96 * se),
                      hratio.cil = exp(coef - 1.96 * se))
                ]
    }
  }

  return(mod_terms)
}


nobs_cond <- function(data, columns, proportional = FALSE) {
  # Number of observations for (combinations of) boolean columns
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  n <- length(columns)
  counts <- matrix(0, nrow = n, ncol = n, dimnames = list(columns, columns))
  for (i in 1:length(columns)) {
    for (j in 1:length(columns)) {
      c1 <- columns[i]
      c2 <- columns[j]
      if (all(c1 %in% names(data), c2 %in% names(data),
              is.logical(data[[c1]]), is.logical(data[[c2]]))) {
        counts[i, j] <- data[get(c1) == TRUE & get(c2) == TRUE, .N]
      } else {
        counts[i, j] <- NA
      }
    }
  }
  if (proportional) {
    props <- (counts / data[,.N]) >= 0.05
    return(props)
  } else {
    return(counts)
  }
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

diag_residuals <- function(model, 
                           residuals,
                           sample = NULL,
                           hist.bins = 100,
                           trend.res = 1000,
                           trend.k = 10,
                           col.empirical = palette.colors(palette = "Set 1")[1],
                           col.theoretical = palette.colors(palette = "Set 1")[2],
                           qq.point.size = 1.5,
                           line.size = 0.8,
                           plot.theme = theme_classic()
                           ) 
  {
  # Extract residuals and fitted values
  if(is.null(sample)){
    qres <- data.table(residuals = residuals$quantile_residuals,
                       linpred = predict(model, type = "link",
                                         newdata.guaranteed = TRUE),
                       response =  model$model[,1])
  } else {
    qres <- data.table(residuals = residuals$quantile_residuals[sample],
                       linpred = predict(model, newdata = model$model[sample,],
                                         type = "link",
                                         newdata.guaranteed = TRUE),
                       response =  model$model[sample, 1],
    fitted = fitted(model)[sample])
  }

  # Preparatory calculations
  # Histogram
  qres[, bin := cut(residuals, seq(0, 1, 1/hist.bins), labels = FALSE)]

  # Residuals vs. linear predictor
  qres[, linpred_r := frank(linpred)]

  # Model for smooth trend
  # m_res_linpred <- gam(residuals ~ s(predicted), data = qres, select = TRUE)
  m_res_linpred <- bam(residuals ~ s(linpred, k = trend.k), 
                    data = qres, 
                    select = TRUE, 
                    discrete = TRUE)

  # Trend for untransformed predictor
  t_res_linpred <- data.table(linpred = seq(from = min(qres$linpred), 
                                         to = max(qres$linpred), 
                                         length.out = trend.res))
  t_res_linpred[,trend := predict(m_res_linpred, t_res_linpred)]

  # Trend for rank-transformed predictor
  quantiles <- (1:(trend.res -1)) / (trend.res - 1)
  t_res_linpred_r <- data.table(linpred = c(min(qres$linpred), 
                                       quantile(qres$linpred, quantiles)),
                           linpred_r = (0:(trend.res - 1) / (trend.res - 1) * 
                                    (length(qres$linpred)  - 1)) + 1
                           )
  t_res_linpred_r[,trend := predict(m_res_linpred, t_res_linpred_r)]

  # Plots
  p_qq <- ggplot(qres, aes(sample = residuals)) +
          geom_qq(distribution = qunif, pch = 16, size = qq.point.size) +
          geom_abline(slope = 1, intercept = 0, 
                      size = line.size,
                      col = col.theoretical) +
          labs(title = "Uniform Q-Q",
               x = "Theoretical quantiles",
               y = "Quantile residuals") +
          plot.theme
  p_hist <- ggplot(qres, aes(x = bin/hist.bins - 1/(2*hist.bins), y = ..count..)) +
            geom_hline(yintercept = nrow(qres) / hist.bins, 
                       size = line.size,
                       col = col.theoretical) +
            geom_bar(width = 1/hist.bins) +
            labs(title = paste0("Histogram of residuals (",
                                hist.bins, " bins)"),
                 x = "Quantile residuals",
                 y = "Frequency") +
            plot.theme
  p_res_linpred <- ggplot(qres, aes(x = linpred, y = residuals)) +
              geom_bin2d(bins = sqrt(1e5), show.legend = FALSE) +
              #geom_hex(bins = sqrt(1e5), show.legend = FALSE) +
              geom_hline(yintercept = 0.5, 
                         size = line.size,
                         col = col.theoretical) +
              geom_line(data = t_res_linpred, 
                        mapping = aes(y = trend), 
                        linetype = "dashed",
                        size = line.size,
                        col = col.empirical) +
              scale_fill_gradient(low = "grey90", high = "grey10", na.value = "grey90") +
              labs(title = "Residuals vs. linear predictor",
                   x = "Linear predictor",
                   y = "Quantile residuals") +
              plot.theme 
  p_res_linpred_r <- ggplot(qres, aes(x = linpred_r, y = residuals)) +
                     #geom_hex(bins = sqrt(1e5), show.legend = FALSE) +
                     geom_bin2d(bins = sqrt(1e5), show.legend = FALSE) +
                     geom_hline(yintercept = 0.5, 
                                size = line.size,
                                col = col.theoretical) +
                     geom_line(data = t_res_linpred_r, 
                               mapping = aes(y = trend),
                               linetype = "dashed",
                               size = line.size,
                               col = col.empirical) +
                     scale_fill_gradient2(low = "grey90", high = "grey10", na.value = "grey90") +
                     #scale_fill_viridis_c()+
                     labs(title = "Residuals vs. linear predictor (rank)",
                          x = "Linear predictor (rank transformed)",
                          y = "Quantile residuals") +
                     plot.theme

  # Arrange using `patchwork` package
  p_arranged <- p_qq + p_hist + p_res_linpred  + p_res_linpred_r
  return(p_arranged)
  }


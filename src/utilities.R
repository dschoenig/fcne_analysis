library(mgcv)
library(ff)

fit_models <- function(models, data, chunk.size = 1e6, path = "results/models/", prefix = ""){
  # data: A `data.frame` with containing the response and predictor variables
  #   as columns. The order of rows is important when determining
  # models: A `data.frame` with the following columns: `id` (character),
  #   `response` (character) for the response variable, `predictor` (character)
  #   for the predictor side of the R model formula, `link` (character) for the
  #   link function to be used, and `select` (boolean) for the corresponsing
  #   argument in `bam()`.
  n <- nrow(models)
  # List to hold basic information to be returned by function call
  modres <- vector(mode = "list", length = n)
  withCallingHandlers({
      for (i in 1:n) {
        print(paste0("Fitting model ", models$id[i], " (", i, " of ", n, ")"))
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
                      discrete = TRUE,
                      nthreads = c(2,1),
                      chunk.size = chunk.size,
                      # drop.intercept = !models$intercept[i],
                      # gamma = models$gamma[i],
                      select = models$select[i]
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
        rm(modfit)
      }
    }, 
    warning = function(x) {
      fit_warnings <<- append(fit_warnings, x$message)
      invokeRestart("muffleWarning")
    })
  return(modres)
}

load_models <- function(models, path = "results/models/", prefix = "", summaries = TRUE, env = .GlobalEnv){
  # models: Character vector containing the names of the models to be loaded.
  for(i in 1:length(models)) {
    mname <- paste0(prefix, models[i])
    assign(mname,
           readRDS(paste0(path, mname, ".rds")),
           pos = env)
    if(summaries) {
      assign(paste0(mname, ".sum"),
             summary(get(mname, envir = env)),
             pos = env)
    }
  }
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

quantile_residuals <- function(model, n.sim = 1000, seed = NULL) {

  n <- nobs(model)
  observed.response <- model.frame(model)[,1] 

  # Set RNG
  if (!is.null(seed)) {
    if (exists(".Random.seed")) {
      prev.random.state <- .Random.seed
    } else {
      prev.random.state <- NULL
    }
    set.seed(seed)
  }



  # Break up simulation matrix if R integer limit is surpassed
  if (length(n.sim) == 1 && prod(n * n.sim) > .Machine$integer.max) {
    nelements <- prod(n * n.sim)
    nodes <- ceiling(nelements / .Machine$integer.max)
    breaks <- ceiling(seq(1, n.sim, length.out = nodes+1))
    n.sim <- breaks[2:length(breaks)] - breaks[1:(length(breaks)-1)]
  }

  # Construct simulation matrices
  simulations <- list()
  for (i in 1:length(n.sim)){
    simulations[[i]] <- ff(dim = c(n, n.sim[i]), vmode = vmode(fitted(model)), 
                           pattern = paste0(tempdir(), "/qres"), finalizer = "delete")
  }

  # Simulate response and fill matrices
  for (m in 1:length(n.sim)) {
    for (i in 1:ncol(simulations[[m]])) {
      simulations[[m]][,i] <- as.matrix(simulate(model, 1))
    }
  }

  sim.lower <- sim.upper <- quantile.residuals <- rep(0, n)

  # Compute quantile residuals based on probability integral transform

  for (m in 1:length(n.sim)) {
    sims <- simulations[[m]]
    sim.lower <- sim.lower +
      ffcolapply(rowSums(sims[,i1:i2] < observed.response),
                            X=sims, RETURN = TRUE, CFUN = "csum") / sum(n.sim)
    sim.upper <- sim.upper + 
      ffcolapply(rowSums(sims[,i1:i2] <= observed.response),
                             X=sims, RETURN = TRUE, CFUN = "csum") / sum(n.sim)
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

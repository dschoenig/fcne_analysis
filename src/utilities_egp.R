library(data.table)
library(mgcv)
library(mvnfast)
library(kohonen)
library(Matrix)
library(sf)
library(spdep) # For poly2nb et al
library(lwgeom) # For minimum bounding circle


## EGP functions

som_init_pc <- function(grid, data) {
  # Calculate principal components
  init.pca <- prcomp(x = data, center = FALSE, scale = FALSE)
  init.min <- apply(init.pca$x[, 1:2], 2, min)
  init.max <- apply(init.pca$x[, 1:2], 2, max)
  grid.min <- apply(grid$pts, 2, min)
  grid.max <- apply(grid$pts, 2, max)
  grid.scale <- grid.max - grid.min
  init.scale <- (init.max - init.min) / grid.scale
  # Scale coordinates
  init.coord.pc <- t(apply(grid$pts, 1, \(x) init.scale * (x - grid.min) + init.min))
  # Map to covariate space
  init.coord.cov <- init.coord.pc %*% t(init.pca$rotation[,1:2])
  attr(init.coord.cov, "som.dim") <- c(grid$xdim, grid$ydim)
  return(init.coord.cov)
}

quantization_error <- function(som, data = NULL, ...) {
  if(is.null(data)) {
    distances <- som$distances
  } else {
    embedded <- egp_embed(x = data,
                          som = som,
                          dist = TRUE,
                          dist.name = "dist.mapped",
                          ...) 
    distances <- embedded[, "dist.mapped"]
  }
  if(som$dist.fcts == "sumofsquares") {
    dist.sq <- distances
  } else {
    dist.sq <- distances^2
  }
  quant.e <- mean(dist.sq)
  return(quant.e)
}

variance_explained <- function(som, data = NULL, ...) {
  qe <- quantization_error(som = som, data = data, ...)
  if(is.null(data)) {
    data <- som$data[[1]]
  }
  var.data <- mean(colSums((t(data) - colMeans(data, na.rm = TRUE))^2, na.rm = TRUE))
  var.ex <- 1 - (qe/var.data)
  return(var.ex)
}

topological_error <- function(som, ...) {
  data <- som$data[[1]]
  units <- som$codes[[1]]
  if(som$dist.fcts %in% c("sumofsquares", "euclidean")) {
    bmus <- t(apply(data, 1, \(x) order(colSums((t(units) - x)^2))[1:2]))
  }
  if(som$dist.fcts == "manhattan") {
    bmus <- t(apply(data, 1, \(x) order(colSums(abs(t(units) - x)))[1:2]))
  }
  if(som$dist.fcts == "tanimoto") {
    bmus <- t(apply(data, 1, \(x) order(colSums(t(units) != x) / length(x))[1:2]))
  }
  if(som$grid$topo == "rectangular") {
    # Necessary to recalculate neighbours to use queen instead of rook
    # neighbours
    grid.nb <-
      poly2nb(som$grid.poly, queen = TRUE) |>
      nb2listw(style = "B") |> 
      listw2mat() |>
      Matrix()
  } else {
    grid.nb <- som$grid.nb
  }
  nb <- apply(bmus, 1, \(x) sum(grid.nb[x,x]) > 1)
  topo.e <- 1 - (sum(nb) / length(nb))
  return(topo.e)
}


unit_diff <- function(unit.var,
                      group.var,
                      data = NULL,
                      type = c("ov_frac", "js_div")) {

  if(!is.null(data)) {
    if(length(unit.var) > 1 | length(group.var) > 1) {
      stop("When providing `data`, the arguments `unit.var` and `group.var`\n",
           "must be character vectors of length one, corresponding to column\n",
           "names in the data provided.")
    }
    data.dt <- as.data.table(data)
    data.dt <- data.dt[, c(unit.var, group.var), with = FALSE]
  } else {
    data.dt <-
      data.table(unit.var = unit.var,
                 group.var = group.var)
    unit.var <- "unit.var"
    group.var <- "group.var"
  }

  group.lev <-
    data.dt[,
            as.character(unique(group.col)),
            env = list(group.col = group.var)]

  cast.form <- as.formula(paste(unit.var, group.var, sep = "~"))

  counts <-
    data.dt[, .(.n = .N), by = c(unit.var, group.var)] |>
    dcast(cast.form, value.var = ".n", fill = 0)

  ov <- NULL
  js <- NULL

  if("ov_frac" %in% type) {
    ov <-
      counts[grp1 > 0 | grp2 > 0,
             sum(fifelse(grp1 > 0 & grp2 > 0, 1, 0)) / .N,
             env = list(grp1 = group.lev[1],
                        grp2 = group.lev[2])] 
  }
  if("js_div" %in% type) {
    js <-
      counts[,
             js_div(x = grp1/sum(grp1),
                    y = grp2/sum(grp2),
                    type = "raw"),
             env = list(grp1 = group.lev[1],
                        grp2 = group.lev[2])] 
  }

  result <- c(ov, js)
  names(result) <- type
  if(length(result) > 1) {
    result <- as.list(result)
  }
  return(result)
}


egp_som <- function(data,
                    x.dim = NULL,
                    y.dim = x.dim,
                    n = NULL,
                    vars = NULL,
                    scale = TRUE,
                    dim.equal = TRUE,
                    topo = "hexagonal",
                    nb.fun = "gaussian",
                    dist.fun = "sumofsquares",
                    radius = max(x.dim, y.dim),
                    epochs = 1000,
                    init = som_init_pc,
                    mode = "pbatch",
                    parallel = 1) {

  if(is.null(x.dim) & is.null(n)) {
    stop("Must provide either `x.dim` or `n`.")
  }

  if(inherits(data, "matrix")) {
    if(!is.null(vars)) {
      x <- data[,vars]
    }
  } else {
    if(is.null(vars)) {
      x <- as.matrix(as.data.frame(data))
    } else {
      x <- as.matrix(as.data.frame(data)[,vars])
    }
  }

  x <- na.omit(x)

  if(scale) {
    x.scaled <- scale(x, center = TRUE, scale = TRUE)
  } else {
    x.scaled <- x
  }

  scale.att <- list(mean = apply(x, 2, mean, na.rm = TRUE),
                    sd = apply(x, 2, sd, na.rm = TRUE))

  if(is.null(x.dim)) {
    if(dim.equal) {
      x.dim <- round(sqrt(n))
      y.dim <- x.dim
    } else {
      pc <- prcomp(x = x.scaled, center = FALSE, scale = FALSE, rank. = 2)
      er <- pc$sdev[1]^2 / pc$sdev[2]^2
      x.dim <- round(sqrt(er * n))
      y.dim <- round(n/x.dim)
    }
  }

  n.units <- x.dim * y.dim

  som.grid <- somgrid(xdim = x.dim,
                      ydim = y.dim, 
                      topo = topo)

  if(topo == "hexagonal") {
    grid.poly <- grid_hex_poly(centroids = som.grid$pts)
  } else {
    grid.poly <- grid_rect_poly(centroids = som.grid$pts)
  }

  grid.nb <-
    poly2nb(grid.poly, queen = FALSE) |>
    nb2listw(style = "B") |> 
    listw2mat() |>
    Matrix()

  if(epochs * nrow(x.scaled) < 1e6)
    warning("Number of epochs may be too small for the number of observations.")

  if(is.function(init)) {
    init <- init(som.grid, x.scaled)
  }
 
  som.fit <- som(x.scaled,
                 grid = som.grid, 
                 rlen = epochs,
                 radius = radius,
                 init = init,
                 dist.fcts = dist.fun,
                 mode = "pbatch", 
                 cores = parallel,
                 normalizeDataLayers = FALSE)
  
  if(dist.fun == "sumofsquares") {
    dist.mat <-
      dist(som.fit$codes[[1]], method = "euclidean") |>
      as.matrix()
    dimnames(dist.mat) <- list(NULL, NULL)
    dist.mat <- dist.mat^2
  }

  if(dist.fun %in% c("euclidean", "manhattan")) {
    dist.mat <-
      dist(som.fit$codes[[1]], method = dist.fun) |>
      as.matrix()
    dimnames(dist.mat) <- list(NULL, NULL)
  }

  if(dist.fun == "tanimoto") {
    dist.mat <-
      t(apply(codes, 1, \(x) colSums(t(codes) != x) / length(x)))
    dimnames(dist.mat) <- list(NULL, NULL)
  }

  som.fit$unit.dist <-
    apply(dist.mat, 1, \(x) data.table(id.to = 1:length(x),
                                       dist = x)) |>
    rbindlist(idcol = "id.from")

  som.fit$n.units <- n.units
  som.fit$grid.poly <- grid.poly
  som.fit$grid.nb <- grid.nb
  som.fit$init <- init
  som.fit$scale <- scale.att

  class(som.fit) <- c(class(som.fit), "egp_som")

  return(som.fit)
}



grid_hex_poly <- function(centroids = NULL,
                          x.dim = NULL,
                          y.dim = NULL,
                          d = 1,
                          phi = 0) {
  if(phi != 0) {
    rot <- matrix(c(cos(phi), sin(phi), -sin(phi), cos(phi)), ncol = 2)
  }
  if(is.null(centroids)) {
    x <- 1L:x.dim
    y <- 1L:y.dim
    centroids <- as.matrix(expand.grid(x = x, y = y))
    centroids[, 1L] <- centroids[, 1L] + 0.5 * (centroids[, 2L]%%2)
    centroids[, 2L] <- sqrt(3)/2 * centroids[, 2L]
    if(phi != 0) {
      centroids <- centroids %*% t(rot)
    }
  }
  R <- d/sqrt(3)
  c30 <- cos(pi/6)
  s30 <- sin(pi/6)
  corners <-
    matrix(c(c30, s30,
             0, 1,
             -c30, s30,
             -c30, -s30,
             0, -1,
             c30, -s30,
             c30, s30),
          byrow = TRUE, nrow = 7) * R
  if(phi != 0) {
    corners <- corners %*% t(rot)
  }
  hex <- 
    apply(centroids, 1,
          \(x) 
          st_polygon(list(matrix(rep(x, 7), nrow = 7, byrow = TRUE) + corners)),
          simplify = FALSE) |>
    st_sfc() |>
    st_sf() |>
    st_set_geometry("geometry")
  hex$id <- 1:nrow(hex)
  return(hex)
}

grid_rect_poly <- function(centroids = NULL,
                           x.dim = NULL,
                           y.dim = NULL,
                           d = 1) {
  if(is.null(centroids)) {
    x <- 1L:x.dim
    y <- 1L:y.dim
    centroids <- as.matrix(expand.grid(x = x, y = y))
  }
  corners <-
    matrix(c(-0.5, -0.5,
              0.5, -0.5,
              0.5,  0.5,
             -0.5,  0.5,
             -0.5, -0.5),
           byrow = TRUE, nrow = 5) * d
  square <- 
    apply(centroids, 1,
          \(x) 
          st_polygon(list(matrix(rep(x, 5), nrow = 5, byrow = TRUE) + corners)),
          simplify = FALSE) |>
    st_sfc() |>
    st_sf() |>
    st_set_geometry("geometry")
  square$id <- 1:nrow(square)
  return(square)
}


egp_embed <- function(x,
                      som,
                      vars = NULL,
                      scale = TRUE,
                      bmu.name = "som.unit",
                      coord = TRUE,
                      coord.names = c("som.x", "som.y"),
                      dist = FALSE,
                      dist.name = "som.dist",
                      append = FALSE,
                      list = FALSE) {
  if(inherits(x, "matrix")) {
    append.mode <- "mat"
    if(!is.null(vars)) {
      x <- x[,vars]
    }
  } else {
    append.mode <- ifelse(is.data.table(x), "dt", "df")
    if(is.null(vars)) {
      vars <- names(som$scale$mean)
    }
    x <- as.matrix(as.data.frame(x)[,vars])
  }
  x <- na.omit(x)
  if(scale) {
    x.scaled <- t(apply(x, 1, \(x) (x - som$scale$mean) / som$scale$sd))
  } else {
    x.scaled <- x
  }
  mapped <- map(som, x.scaled)
  embedding <- mapped$unit.classif
  if(coord) {
    coord.mapped <- som$grid$pts[embedding,]
    colnames(coord.mapped) <- coord.names
    embedding <- cbind(embedding, coord.mapped)
  }
  if(dist) {
    dist.mapped <- mapped$distances
    embedding <- cbind(embedding, dist.mapped)
  }
  if(is.matrix(embedding)) {
    colnames(embedding)[1] <- bmu.name
  }
  if(append) {
    embedding <- cbind(x, embedding)
    if(append.mode %in% c("dt", "df")) {
      embedding <- as.data.frame(embedding)
    }
    if(append.mode == "dt") {
      setDT(embedding)
    }
  }
  if(list) {
    embedding <- as.list(as.data.frame(embedding))
  }
  return(embedding)
}

get_bmu <- function(x,
                    coord = TRUE,
                    bmu.name = "som.unit",
                    coord.names = c("som.x", "som.y"),
                    list = FALSE) {
  embedding <- x$unit.classif
  if(coord) {
    coord.mapped <- x$grid$pts[embedding,]
    colnames(coord.mapped) <- coord.names
    embedding <- cbind(embedding, coord.mapped)
  }
  if(is.matrix(embedding)) {
    colnames(embedding)[1] <- bmu.name
  }
  if(list) {
    embedding <- as.list(as.data.frame(embedding))
    if(coord == FALSE) names(embedding) <- bmu.name
  }
  return(embedding)
}

get_coord <- function(x) {x$grid$pts[x$unit.classif,]}
get_codes <- function(x) {x$codes}
get_grid <- function(x) {x$grid}
get_poly <- function(x) {x$grid.poly}
get_nb <- function(x) {x$grid.nb}


.ids_by_group <- function(data,
                          id.var,
                          group.vars = NULL,
                          group.labels = NULL,
                          add.label = TRUE,
                          expand.label = TRUE,
                          label.prefix = NULL,
                          ...){
  data.dt <- as.data.table(data)
  if(is.null(group.vars)) {
    groups <-
        data.dt[order(id.col),
                .(id.col = list(id.col)),
                  env = list(id.col = id.var)]
  } else {
    groups <-
        data.dt[order(id.col),
                .(id.col = list(id.col)),
                  by = group.vars,
                  env = list(id.col = id.var)]
    setorderv(groups, group.vars)
    if(add.label) {
      labels <- groups[, ..group.vars]
    }
  }
  if(!is.null(group.labels)) {
    for(i in 1:length(group.vars)) {
      old.labels <- as.character(groups[[group.vars[i]]])
      new.labels <- factor(group.labels[[i]][old.labels], 
                           levels = group.labels[[i]])
      groups[, (group.vars[i]) := new.labels]
    }
    setorderv(groups, group.vars)
  }
  if(add.label == TRUE & !is.null(group.vars)) {
    groups <- .add_group_label(data = groups,
                               cols = group.vars,
                               expand.label = expand.label,
                               ...)
  }
  if(is.character(add.label) & !is.null(group.vars)) {
    groups <- .add_group_label(data = groups,
                               cols = group.vars,
                               label.name = add.label,
                               expand.label = expand.label,
                               ...)
  }
  if(!is.null(label.prefix)) {
    groups[[add.label]] <- paste0(label.prefix, groups[[add.label]])
  }
  return(groups)
}


.add_group_label <- function(data,
                             cols,
                             label.name = "group.label",
                             expand.label = TRUE,
                             col.sep = ":",
                             cat.sep = "."
                             ) {
    data <- copy(data)
    labels <- data[, ..cols]
    if(expand.label) {
      for(i in 1:length(cols)) {
        labels[[cols[i]]] <- paste(cols[i], labels[[cols[i]]], sep = cat.sep)
      }
    }
    labels.c <- do.call(paste, c(labels, sep = col.sep))
    data[, label.col := labels.c, env = list(label.col = label.name)]
    setorderv(data, c(cols, label.name))
    setcolorder(data, c(label.name, cols))
    return(data)
}


.nb_sequential <- function(dist,
                           n,
                           n.min = 1,
                           deg.max = NULL) {

  if(is.null(deg.max)) {
    deg.max <- length(n)
  }

  unit.n <- data.table(id.to = 1:length(n),
                       n = n)

  dist <-
    copy(dist) |>
    merge(unit.n, by = "id.to")
  setkey(dist, id.from)

  dist.ord <-
    dist[order(id.from, dist, n)
         ][n > 0,
           .(id.to,
             n,
             deg = 1:length(dist),
             nc = cumsum(n),
             dist),
           by = id.from
           ]

  nb.dt <-
    merge(dist.ord,
          dist.ord[nc >= n.min,
                   .(n.min.u = min(nc)),
                   by = id.from]) |>
    _[nc <= n.min.u,
      .(neighbourhood = list(id.to),
        n = unique(n.min.u),
        degree = max(deg)),
      by = id.from]

  return(as.list(nb.dt[, -"id.from"]))
}


.nb_expand <- function(A,
                       n,
                       n.min = 1,
                       deg.max = NULL) {

  if(is.null(deg.max)) {
    deg.max <- length(n)
  }

  nbh <- list() 

  n.nbh <- n
  deg.nbh <- integer(length(n))

  # deg 0 nbh

  bmu.sel <- which(n.nbh >= n.min)
  nbh[bmu.sel] <- bmu.sel

  deg <- 0
  while(any(n.nbh < n.min)) {
    deg <- deg + 1
    # deg 1 nbh
    bmu.sel <- which(n.nbh < n.min)
    if(deg < 2) {
      A.nbh <- A
    } else {
      A.nbh <- A.nbh %*% A
    }

    nbh.sel <- apply(A.nbh[bmu.sel,,drop = FALSE], 1,
                    \(x) which(x > 0),
                    simplify = FALSE)
    n.nbh[bmu.sel] <- unlist(lapply(nbh.sel, \(x) sum(n[x])))
    deg.nbh[bmu.sel] <- deg
    nbh[bmu.sel] <- nbh.sel
    if(deg == deg.max) {
      nbh[which(n.nbh < n.min)] <- NA
      break
    }
  }

  neighbourhood <-
    list(neighbourhood = nbh,
         n = n.nbh,
         degree = deg.nbh)

  return(neighbourhood)
}


egp_define_counterfactual <-
  function(data,
           som,
           cf.ids = NULL,
           fac.ids = NULL,
           compare.by = NULL,
           group.by = NULL,
           som.var = "som.unit",
           geo.vars = NULL,
           geo.kernel = "matern32",
           geo.range = NULL,
           id.var = "id",
           group.name = "group.id",
           unit.name = "cf.unit",
           assign.name = "assigned",
           assign.cat = c("counterfactual", "factual"),
           n.min = 1,
           nb.strategy = "sequential",
           deg.max = NULL,
           agg.size = NULL,
           progress = TRUE) {

  data.dt <- as.data.table(data)
  rm(data)

  if(!id.var %in% names(data.dt)) {
    data.dt[, id.col := 1:.N, env = list(id.col = id.var)]
  }

  if(is.list(group.by)) {
    group.by.c <- unique(do.call(c, group.by))
  }
  if(!is.list(group.by)) {
    group.by.c <- group.by
    group.by <- list(group.by)
  }

  vars.sel <- c(id.var, compare.by, group.by.c, som.var, geo.vars)

  data.dt <- data.dt[, ..vars.sel]

  if(is.null(fac.ids) & is.null(cf.ids)) {
    stop("Either `fac.ids` or `cf.ids` must be provided")
  }
  if(!is.null(fac.ids) & is.null(cf.ids)) {
    cf.ids <-
      data.dt[!id.col %in% fac.ids,
              id.col,
              env = list(id.col = id.var)]
  }
  if(is.null(fac.ids) & !is.null(cf.ids)) {
    fac.ids <-
      data.dt[!id.col %in% cf.ids,
              id.col,
              env = list(id.col = id.var)]
  }

  if(length(intersect(fac.ids, cf.ids)) > 0) {
    stop("Some observations are assigned to both the factual and counterfactual.")
  }

  # Reference SOM units

  data.dt <-
    data.dt[.(id.col = unique(c(fac.ids, cf.ids))),
            on = id.var,
            nomatch = NULL,
            env = list(id.col = id.var)]
  setkeyv(data.dt, id.var)

  bmu.ref <- data.dt[.(cf.ids), on = id.var]

  if(progress) message("Assigning counterfactual observations …")

  if(is.null(compare.by)) {

    n.bmu <- integer(nrow(som$grid.nb))
    n.ref <- bmu.ref[order(som.col),
                     .(n = .N),
                     by = som.col,
                     env = list(som.col = som.var)]
    n.bmu[n.ref[, som.col, env = list(som.col = som.var)]] <- n.ref$n
    n.bmu[is.na(n.bmu)] <- 0


    if(nb.strategy == "sequential") {
      nbh <-
        .nb_sequential(dist = som$unit.dist,
                       n = n.bmu,
                       n.min = n.min,
                       deg.max = deg.max)
    }
    if(nb.strategy == "expand") {
      nbh <-
        .nb_expand(A = som$grid.nb,
                   n = n.bmu,
                   n.min = n.min,
                   deg.max = deg.max)
    }

    cf.bmu.dt <- 
      data.dt[.(fac.ids),
              on = id.var
              ][,
                .(id.col,
                  cf.col = nbh$neighbourhood[som.col]),
                env = list(id.col = id.var,
                           cf.col = unit.name,
                           som.col = som.var)]
    # cf.bmu.dt[,
    #           .n := as.integer(lapply(cf.col, length)),
    #           env = list(cf.col = unit.name)]

    cf.ids.dt <-
      data.dt[.(cf.ids),
              on = id.var
              ][order(som.col, id.col),
                .(id.col = list(id.col)),
                by = (cf.col = som.col),
                env = list(id.col = id.var,
                           som.col = som.var,
                           cf.col = unit.name)]

  } else {


    bmus <- 1:nrow(som$grid.nb)

    bmu.by <-
      data.dt[,
              .(som.col = 1:nrow(som$grid.nb)),
              by = compare.by,
              env = list(som.col = som.var)]

    comp <-
      data.dt[, .(.n = .N), by = c(compare.by)]
    comp[, .cfgrp := 1:.N] 

    comp.bmu.cf <-
      data.dt[.(cf.ids), on = id.var
              ][, .(.n = .N), by = c(som.var, compare.by)] |>
      merge(bmu.by, by = c(som.var, compare.by), all = TRUE)
 
    nbh.l <- list()
    cf.ids.l <- list()

    if(progress) {
      prog <- txtProgressBar(min = 0,
                             max = nrow(comp),
                             char = "=", width = NA, title = "Progress", style = 3)
    }

    for(i in 1:nrow(comp)) {
      
      n.ref <- comp.bmu.cf[comp[i, compare.by, with = FALSE],
                           on = compare.by
                           ][order(som.col),
                             .(som.col, .n),
                             env = list(som.col = som.var)]

      n.bmu <- integer(nrow(som$grid.nb))
      n.bmu[n.ref[, som.col, env = list(som.col = som.var)]] <- n.ref$.n
      n.bmu[is.na(n.bmu)] <- 0

      if(nb.strategy == "sequential") {
        nbh.l[[i]] <-
          .nb_sequential(dist = som$unit.dist,
                         n = n.bmu,
                         n.min = n.min,
                         deg.max = deg.max)$neighbourhood
      }
      if(nb.strategy == "expand") {
        nbh.l[[i]] <-
          .nb_expand(A = som$grid.nb,
                     n = n.bmu,
                     n.min = n.min,
                     deg.max = deg.max)$neighbourhood
      }

      
      cf.ids.l[[i]] <-
        data.dt[.(cf.ids),
                on = id.var
                ][comp[i, compare.by, with = FALSE],
                  on = compare.by
                  ][order(som.col, id.col),
                    .(id.col = list(id.col)),
                    by = .(cf.col = som.col),
                    env = list(id.col = id.var,
                               som.col = som.var,
                               cf.col = unit.name)]

      if(progress) {
        setTxtProgressBar(prog, i)
      }

    } # End loop over comparison groups


    if(progress) {
      close(prog)
    }

    comp[, `:=`(.nbh = nbh.l)]

    comp.cf <-
      comp[,
           .(cf.col = unlist(.nbh, recursive = FALSE),
             som.col = bmus),
           by = c(".cfgrp", compare.by),
           env = list(som.col = som.var,
                      cf.col = unit.name)
           ]

    cf.bmu.dt <-
      cbind(
            data.dt[.(fac.ids),
                    on = id.var
                    ][order(id.col),
                      env = list(id.col = id.var)
                      ][,
                        c(id.var, compare.by), with = FALSE],
            comp.cf[data.dt[.(fac.ids),
                            on = id.var
                            ][order(id.col),
                              env = list(id.col = id.var)
                              ][,
                                c(compare.by, som.var),
                                with = FALSE],
                                .(cf.col),
                                on = c(compare.by, som.var),
                                env = list(cf.col = unit.name)])

    cf.ids.dt <- 
      rbindlist(cf.ids.l, idcol = ".cfgrp") |>
      merge(comp[, -c(".n", ".nbh")], by = ".cfgrp") |>
      setcolorder(c(compare.by, unit.name, id.var)) |>
      _[, -".cfgrp"]



  }


  cf.ids.dt[,
            .n := unlist(lapply(id.col, length)),
            env = list(id.col = id.var)]


  data.cf <- data.dt[.(cf.ids), on = id.var ]
  data.cf[,
          assign.col := assign.cat[1],
          env = list(assign.col = assign.name)]

  data.fac <- data.dt[.(fac.ids), on = id.var]
  data.fac[,
           assign.col := assign.cat[2],
           env = list(assign.col = assign.name)]
  
  data.ret <- rbind(data.fac, data.cf)
  setorderv(data.ret, id.var)

  # Groups

  if(progress) message("Assigning groups …")

  id.names <- paste(id.var, assign.cat, sep = ".")
  geo.names.cf <- paste(geo.vars, assign.cat[1], sep = ".")
  geo.names.fac <- paste(geo.vars, assign.cat[2], sep = ".")

  groups.l <- list()
  for(i in seq_along(group.by)){
    groups.l[[i]] <-
      .ids_by_group(data.dt[.(fac.ids)],
                    id.var = id.var,
                    group.vars = group.by[[i]],
                    add.label = FALSE)
  }
  groups <- rbindlist(groups.l, fill = TRUE)
  setnames(groups, id.var, id.names[2])
  groups[,
         group.col := 1:nrow(groups),
         env = list(group.col = group.name)]
  setorderv(groups, group.name)

  groups.long <-
    groups[,
           .(id.fac = as.integer(unlist(id.fac))),
           by = group.name,
           env = list(id.fac = id.names[2])]
  groups.long[, .n.fac := .N, by = group.name]
  setkeyv(groups.long, id.names[2])

  # groups.n <-
  #   groups.long[, .(.n.fac = .N), by = group.name]
  # setkey(groups.n, .n.fac)


  units.fac <-
    cf.bmu.dt[,
               .(cf.col = as.integer(unlist(cf.col))),
               by = c(compare.by, id.var),
               env = list(cf.col = unit.name)
               ]
  setnames(units.fac, id.var, id.names[2])
  sel.cf.ids <- c(compare.by, unit.name, ".n")
  units.fac <-
    units.fac |>
    merge(SJ(cf.ids.dt[, ..sel.cf.ids]),
          by = c(compare.by, unit.name),
          sort = FALSE,
          all.x = TRUE,
          all.y = FALSE) |>
    merge(data.fac[,
                   .(id.fac = id.col,
                     fac.x = geo.x,
                     fac.y = geo.y),
                   env = list(id.fac = id.names[2],
                              id.col = id.var,
                              fac.x = geo.names.fac[1],
                              geo.x = geo.vars[1],
                              fac.y = geo.names.fac[2],
                              geo.y = geo.vars[2])],
          by = id.names[2]) |>
    na.omit(".n")

  # REMOVE later
  if(!all(data.fac[[id.var]] %in% units.fac[[id.names[2]]])) warning("Not all treatment observations are taken into account.")

  units.cf <-
    cf.ids.dt[,
               .(id.cf = as.integer(unlist(id.col))),
               by = c(compare.by, unit.name),
               env = list(id.cf = id.names[1],
                          id.col = id.var)
               ] |>
    merge(data.cf[,
                  .(id.cf = id.col,
                    cf.x = geo.x,
                    cf.y = geo.y),
                  env = list(id.cf = id.names[1],
                             id.col = id.var,
                             cf.x = geo.names.cf[1],
                             geo.x = geo.vars[1],
                             cf.y = geo.names.cf[2],
                             geo.y = geo.vars[2])],
          by = id.names[1])

  rm(data.fac, data.cf)
  gc()

  if(progress) message("Calculating weights …")

  if(is.null(agg.size)) agg.size <- sum(units.fac$.n)

  units.fac[order(-.n), .nc := cumsum(.n)]
  units.fac[, .agg.id := ceiling(.nc / agg.size)]
  units.fac <- units.fac[, -c(".n", ".nc")]
  agg.ids <- na.omit(unique(units.fac[order(.agg.id), .agg.id]))

  if(!is.null(geo.vars)) {
    range.sam <- 1e6
    if(is.null(geo.range)) {
      if(nrow(data.dt) > range.sam) {
        pts <- st_multipoint(x = as.matrix(data.dt[sample(1:.N, range.sam, replace = TRUE),
                                                   ..geo.vars]),
                             dim = "XY")
      } else {
        pts <-
          st_multipoint(x = as.matrix(data.dt[, ..geo.vars]),
                             dim = "XY")
      }
      pts.bb <- st_bbox(st_minimum_bounding_circle(pts))
      geo.range <- pts.bb[["xmax"]] - pts.bb[["xmin"]]
      rm(pts.bb)
    }
  } else {
    geo.kernel = "nodist"
    geo.range <- 1
  }

  rm(data.dt)
  gc()

  dist.fun <- function(x) sqrt(rowSums((x[, 1:2] - x[, 3:4])^2))

  geo.fun <-
    switch(geo.kernel,
           "matern12" = function(x) {exp(-x)},
           "matern32" = function(x) {
                          sqrt3 <- sqrt(3)
                          (1 + sqrt3 * x) * exp(-sqrt3 * x)},
           "matern52" = function(x) {
                          sqrt5 <- sqrt(5)
                          (1 + sqrt5 * x + sqrt5/3 * x^2) * exp(-sqrt5 * x)},
          )


  if(progress) {
    prog <- txtProgressBar(min = 0,
                           max = length(agg.ids),
                           char = "=", width = NA, title = "Progress", style = 3)
  }

  weights.cf.l <- list()

  for(i in seq_along(agg.ids)) {

    if(!is.null(geo.vars)) {

      dist.obs <-
        units.cf[SJ(units.fac[J(agg.ids[i]),
                    on = ".agg.id"]),
                 on = c(compare.by, unit.name),
                 allow.cartesian = TRUE
                 ][,
                   .(fac.id,
                     cf.id,
                     .dist = dist.fun(as.matrix(.SD)) / geo.range),
                   .SDcols = c(geo.names.fac, geo.names.cf),
                   env = list(fac.id = id.names[2],
                              cf.id = id.names[1])]
      dist.obs[, .geosim := geo.fun(.dist)]

    } else {

      dist.obs <-
        units.cf[SJ(units.fac[J(agg.ids[i]),
                    on = ".agg.id"]),
                 on = unit.name,
                 allow.cartesian = TRUE
                 ][,
                   .(fac.id,
                     cf.id,
                     .geosim = 1),
                   env = list(fac.id = id.names[2],
                              cf.id = id.names[1])]


    }

    dist.obs[,
             .w := .geosim / sum(.geosim),
             by = c(id.names[2])]
    setkeyv(dist.obs, id.names[2])

    weights <- groups.long[dist.obs, on = id.names[2], allow.cartesian = TRUE]

    weights.cf.l[[i]] <-
      weights[,
              .(.w = sum(.w)),
              by = c(group.name, id.names[1])]

    if(progress) {
      setTxtProgressBar(prog, i)
    }

  }

  rm(dist.obs)

  if(progress) {
    close(prog)
  }

  weights.cf <-
    rbindlist(weights.cf.l) |>
    _[, .(.w = sum(.w)),
       by = c(group.name, id.names[1])]

  rm(weights.cf.l)
  gc()

  
  if(progress) message("Processing weights for each group …")

  weights.cf[, .w := .w/sum(.w), by = group.name]

  groups.cf <-
    weights.cf[order(cf.id),
               .(cf.id = list(cf.id),
                 .w = list(.w)),
               by = group.name,
               env = list(cf.id = id.names[1])
               ][order(group.col),
                 env = list(group.col = group.name)]

  rm(weights.cf)
  gc()

  groups.comb <- merge(groups, groups.cf)
  setcolorder(groups.comb, c(group.name, group.by.c, id.names[2], id.names[1], ".w")) 

  counterfactual <- list(data = data.ret,
                         groups = groups.comb,
                         assignment = cf.bmu.dt,
                         units = cf.ids.dt[, -".n"],
                         compare.by = compare.by,
                         group.by = group.by,
                         group.by.c = group.by.c,
                         id.var = id.var,
                         som.var = som.var,
                         geo.vars = geo.vars,
                         geo.kernel = geo.kernel,
                         geo.range = geo.range,
                         group.var = group.name,
                         unit.var = unit.name,
                         assign.var = assign.name,
                         assign.cat = assign.cat)

  class(counterfactual) <- c(class(counterfactual), "egp_cf_def")

  return(counterfactual)
}


get_weights <- function(cf.def,
                        group = NULL) {

  id.var <- cf.def$id.var
  group.var <- cf.def$group.var
  assign.var <- cf.def$assign.var
  assign.cat <- cf.def$assign.cat
  id.names <- paste(id.var, assign.cat, sep = ".")

  env.w <-
    list(id.col = id.var,
         group.col = group.var,
         assign.col = assign.var,
         fac.col = id.names[2],
         cf.col = id.names[1])


  idx.f <-
    cf.def$groups[,
                  .(id.col = unlist(fac.col)),
                  by = group.col, env = env.w]
  idx.f[, .w := 1/.N, by = group.col, env = env.w]
  idx.cf <-
    cf.def$groups[,
                  .(id.col = unlist(cf.col),
                    .w = unlist(.w)),
                  by = group.col, env = env.w]

  w <-
    merge(cf.def$data,
          rbind(idx.f, idx.cf),
          by = id.var,
          all = TRUE)
  setorderv(w, c(group.var, id.var), na.last = TRUE)
  setcolorder(w, c(group.var, id.var))

  w[is.na(.w), .w := 0]

  if(is.null(group)) {
    return(copy(w))
  } else {
    return(copy(w[group.col %in% group, env = env.w]))
  }
}



imbalance <- function(data,
                      variables,
                      id.trt,
                      id.ref,
                      type = "raw",
                      w.trt = NULL,
                      w.ref = NULL,
                      measure = c("d_cohen",
                                  "var_ratio",
                                  "ks_stat",
                                  "js_div"),
                      js.type = "density",
                      js.dens.n = 512,
                      js.dens.bw = "SJ",
                      js.disc.breaks = "FD") {

  if(is.null(w.trt) & is.null(w.ref)) type <- "raw"

  data <- as.data.table(data)[, variables, with = FALSE]
  data[, (variables) := lapply(.SD, as.numeric), .SDcols = variables]

  measure.eff.l <- list()
  measure.raw.l <- list()

  data.raw.f <- data[id.trt,]
  data.raw.f[, .type := "trt"]
  data.raw.cf <- data[id.ref,]
  data.raw.cf[, .type := "ref"]

  if(type != "raw") {
    if(is.null(w.trt)) w.trt <- rep(1, length(id.trt)) / length(id.trt)
    if(is.null(w.ref)) w.ref <- rep(1, length(id.ref)) / length(id.ref)
    data.raw.f[, .w := w.trt]
    data.raw.cf[, .w := w.ref]
  }

  data.raw <-
    rbind(data.raw.f, data.raw.cf) |>
  melt(measure.vars = variables,
       variable.name = ".variable",
       value.name = ".value")

  rm(data.raw.f, data.raw.cf)

  data.raw[, .type := factor(.type, levels = c("ref", "trt"))]


  if(type %in% c("effective", "both")) {

    measure.fun.eff <-
      list("d_cohen" = d_cohen_w,
           "var_ratio" = var_ratio_w,
           "ks_stat" = ks_stat_w,
           "js_div" = \(x, y, wx, wy) js_div_w(x, y, wx, wy,
                                               type = js.type,
                                               disc.breaks = js.disc.breaks,
                                               dens.n = js.dens.n,
                                               dens.bw = js.dens.bw))

    m.i <- which(names(measure.fun.eff) %in% measure)
    for(i in m.i) {
      measure.eff.l[[i]] <-
        data.raw[,
                 .(.measure = names(measure.fun.eff[i]),
                   .type = "effective",
                   .imbalance = measure.fun.eff[[i]](x = .value[.type == "trt"],
                                                     y = .value[.type == "ref"],
                                                     wx = .w[.type == "trt"],
                                                     wy = .w[.type == "ref"])),
                 by = c(".variable")]
    }
    measure.names <- names(measure.fun.eff)
  }

  if(type %in% c("raw", "both")) {
    measure.fun.raw <-
      list("d_cohen" = d_cohen,
           "var_ratio" = var_ratio,
           "ks_stat" = ks_stat,
           "js_div" = \(x, y) js_div(x, y,
                                     type = js.type,
                                     disc.breaks = js.disc.breaks,
                                     dens.n = js.dens.n,
                                     dens.bw = js.dens.bw))

    m.i <- which(names(measure.fun.raw) %in% measure)
    for(i in m.i) {
      measure.raw.l[[i]] <-
        data.raw[,
                 .(.measure = names(measure.fun.raw[i]),
                   .type = "raw",
                   .imbalance = measure.fun.raw[[i]](x = .value[.type == "trt"],
                                                     y = .value[.type == "ref"])),
                 by = c(".variable")]
    }
    measure.names <- names(measure.fun.raw)
  }


  measures <-
    rbindlist(c(measure.raw.l, measure.eff.l)) |>
    dcast(.variable + .measure ~ .type, value.var = ".imbalance")

  measures[,
           `:=`(.variable = factor(.variable, levels = variables),
                .measure = factor(.measure, levels = measure.names))]
  setorderv(measures, c(".variable", ".measure"))

  return(copy(measures))

}





imbalance_raw <- function(data,
                          variables,
                          id.trt,
                          id.ref,
                          measure = c("d_cohen",
                                      "var_ratio",
                                      "ks_stat",
                                      "js_div"),
                          js.type = "density",
                          js.dens.n = 512,
                          js.dens.bw = "SJ",
                          js.disc.breaks = "FD") {

  data <- as.data.table(data)[, variables, with = FALSE]
  data[, (variables) := lapply(.SD, as.numeric), .SDcols = variables]

  measure.raw.l <- list()

  data.raw.f <- data[id.trt,]
  data.raw.f[, .type := "trt"]
  data.raw.cf <- data[id.ref,]
  data.raw.cf[, .type := "ref"]

  data.raw <-
    rbind(data.raw.f, data.raw.cf) |>
  melt(measure.vars = variables,
       variable.name = ".variable",
       value.name = ".value")

  rm(data.raw.f, data.raw.cf)

  data.raw[, .type := factor(.type, levels = c("ref", "trt"))]

  measure.fun.raw <-
    list("d_cohen" = d_cohen,
         "var_ratio" = var_ratio,
         "ks_stat" = ks_stat,
         "js_div" = \(x, y) js_div(x, y,
                                   type = js.type,
                                   disc.breaks = js.disc.breaks,
                                   dens.n = js.dens.n,
                                   dens.bw = js.dens.bw))

  m.i <- which(names(measure.fun.raw) %in% measure)
  for(i in m.i) {
    measure.raw.l[[i]] <-
      data.raw[,
               .(.measure = names(measure.fun.raw[i]),
                 .type = "raw",
                 .imbalance = measure.fun.raw[[i]](x = .value[.type == "trt"],
                                                   y = .value[.type == "ref"])),
               by = c(".variable")]
  }
  measure.names <- names(measure.fun.raw)

  measures <-
    rbindlist(measure.raw.l) |>
    dcast(.variable + .measure ~ .type, value.var = ".imbalance")

  measures[,
           `:=`(.variable = factor(.variable, levels = variables),
                .measure = factor(.measure, levels = measure.names))]
  setorderv(measures, c(".variable", ".measure"))

  return(copy(measures))

}



egp_imbalance <- function(data,
                          variables,
                          cf.def,
                          group = 1,
                          type = "both",
                          measure = c("d_cohen",
                                      "var_ratio",
                                      "ks_stat",
                                      "js_div"),
                          js.type = "density",
                          js.dens.n = 512,
                          js.dens.bw = "SJ",
                          js.disc.breaks = "FD"
                          ){

  id.var <- cf.def$id.var
  group.var <- cf.def$group.var
  assign.var <- cf.def$assign.var
  assign.cat <- cf.def$assign.cat
  id.names <- paste(id.var, assign.cat, sep = ".")

  env.w <-
    list(id.col = id.var,
         group.col = group.var,
         assign.col = assign.var,
         fac.col = id.names[2],
         cf.col = id.names[1])

  data <- data[, c(id.var, variables), with = FALSE]
  data[, (variables) := lapply(.SD, as.numeric), .SDcols = variables]

  measure.eff.l <- list()
  measure.raw.l <- list()

  if(type %in% c("effective", "both")) {

    cf.w <-
      get_weights(cf.def, group) |>
      _[, c(group.var, id.var, assign.var, ".w"), with = FALSE]
    
    data.eff <-
      merge(cf.w,
            data,
            by = id.var,
            all = FALSE) |>
      melt(measure.vars = variables,
           variable.name = ".variable",
           value.name = ".value")

    data.eff <- data.eff[!is.na(group.col), env = env.w]

    measure.fun.eff <-
      list("d_cohen" = d_cohen_w,
           "var_ratio" = var_ratio_w,
           "ks_stat" = ks_stat_w,
           "js_div" = \(x, y, wx, wy) js_div_w(x, y, wx, wy,
                                               type = js.type,
                                               disc.breaks = js.disc.breaks,
                                               dens.n = js.dens.n,
                                               dens.bw = js.dens.bw))

    m.i <- which(names(measure.fun.eff) %in% measure)
    for(i in m.i) {
      measure.eff.l[[i]] <-
        data.eff[,
                 .(.measure = names(measure.fun.eff[i]),
                   .type = "effective",
                   .imbalance = measure.fun.eff[[i]](x = .value[assign.col == assign.cat[2]],
                                                     y = .value[assign.col == assign.cat[1]],
                                                     wx = .w[assign.col == assign.cat[2]],
                                                     wy = .w[assign.col == assign.cat[1]])),
                by = c(group.var, ".variable"),
                env = env.w]
    }
    measure.names <- names(measure.fun.eff)
  }



  if(type %in% c("raw", "both")) {
    data.raw.f <-
      merge(cf.w[assign.col == assign.cat[2],
                 .(group.col, id.col, assign.col),
                 env = env.w],
            data,
            all = FALSE)

    data.raw.cf <- 
      merge(cf.def$data[assign.col == assign.cat[1],
                        .(id.col, assign.col),
                        env = env.w],
            data,
            all.y = FALSE)


    cf.groups <-
      CJ(.id = data.raw.cf[[id.var]],
         .group.id = unique(data.raw.f$group.id))
    setnames(cf.groups, c(".id", ".group.id"), c(id.var, group.var))

    data.raw <-
      rbind(data.raw.f,
            merge(cf.groups,
                  data.raw.cf)) |>
      melt(measure.vars = variables,
           variable.name = ".variable",
           value.name = ".value")

    rm(data.raw.f, data.raw.cf)

    measure.fun.raw <-
      list("d_cohen" = d_cohen,
           "var_ratio" = var_ratio,
           "ks_stat" = ks_stat,
           "js_div" = \(x, y) js_div(x, y,
                                     type = js.type,
                                     disc.breaks = js.disc.breaks,
                                     dens.n = js.dens.n,
                                     dens.bw = js.dens.bw))

    m.i <- which(names(measure.fun.raw) %in% measure)
    for(i in m.i) {
      measure.raw.l[[i]] <-
        data.raw[,
               .(.measure = names(measure.fun.raw[i]),
                 .type = "raw",
                 .imbalance = measure.fun.raw[[i]](x = .value[assign.col == assign.cat[2]],
                                                   y = .value[assign.col == assign.cat[1]])),
              by = c(group.var, ".variable"),
              env = env.w]
    }
    measure.names <- names(measure.fun.raw)
  }


  cast.form <-
    as.formula(paste0(group.var, " + .variable + .measure ~ .type"))

  measures <-
    rbindlist(c(measure.raw.l, measure.eff.l)) |>
    dcast(cast.form, value.var = ".imbalance")

  measures[,
           `:=`(.variable = factor(.variable, levels = variables),
                .measure = factor(.measure, levels = measure.names))]
  setorderv(measures, c(group.var, ".variable", ".measure"))

  return(copy(measures))

}


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


egp_posterior_draw <- function(model,
                               n = 1000,
                               unconditional = TRUE,
                               dist = "normal",
                               t.df = 3,
                               package = "mgcv",
                               parallel = 1) {
  if(dist == "normal") {
    if(package == "mgcv") {
      post <-
          mgcv::rmvn(n = n,
                     mu = coef(model),
                     V = vcov(model, unconditional = unconditional))
    }
    if(package == "mvnfast") {
      post <-
        mvnfast::rmvn(n = n,
                      mu = coef(model),
                      sigma = vcov(model, unconditional = unconditional),
                      ncores = parallel)
    }
  }
  if(dist == "t") {
    if(package == "mgcv") {
      post <-
          mgcv::r.mvt(n = n,
                      mu = coef(model),
                      V = vcov(model, unconditional = unconditional),
                      df = t.df)
    }
    if(package == "mvnfast") {
      post <-
        mvnfast::rmvt(n = n,
                      mu = coef(model),
                      sigma = vcov(model, unconditional = unconditional),
                      df = t.df,
                      ncores = parallel)
    }
  }
  colnames(post) <- names(coef(model))
  post <- Matrix(post)
  return(post)
}


.evaluate_posterior <- 
  function(
           model, 
           posterior,
           data,
           id.var = "id",
           draw.name = ".draw",
           pred.name = "predicted",
           type = "link",
           epred = FALSE,
           fun.sim = NULL,
           obs = NULL,
           coef = NULL,
           weights = NULL,
           marginals = NULL,
           marginal.ids = NULL,
           predict.chunk = NULL,
           post.chunk = NULL,
           progress = TRUE
           ) {
  mod.fam <- fix.family.rd(model$family)
  if(is.null(fun.sim)) {
    fun.sim <- mod.fam$rd
  }
  mod.scale <- model$sig2
  if(is.null(weights)) {
    pred.wt <- rep(1, nrow(data))
  } else {
    pred.wt <- weights
  }
  posterior <- Matrix(posterior)
  data.dt <- copy(as.data.table(data))
  if(!id.var %in% names(data.dt)) {
    data.dt[, id.col := 1:.N, env = list(id.col = id.var)]
  }
  if(!is.null(obs)) {
    no.obs <-
      data.dt[id.col %in% obs, length(id.col) < 1, env = list(id.col = id.var)]
    if(no.obs) return(NULL)
    data.dt <- copy(data.dt[.(obs), on = id.var])
  }
  setkeyv(data.dt, id.var)
  data.dt
  n <- nrow(data.dt)
  m <- nrow(posterior)
  if(is.null(predict.chunk)) predict.chunk <- n
  if(is.null(post.chunk)) post.chunk <- m
  predict.chunks <- chunk_seq(1, n, predict.chunk)
  post.chunks <- chunk_seq(1, m, post.chunk)
  # Set excluded coefficients to 0
  if(!is.null(coef)) {
    posterior[,-coef] <- 0
  }
  if(is.null(marginals)) {
    marginals <- list(marginal = 1:ncol(posterior))
  }
  if(is.null(marginal.ids)) {
    marginal.ids <- list(marginal = data.dt[[id.var]])
  }
  if(length(marginals) != length(marginal.ids))
    stop("Number of elements in `marginals` and `marginal.ids` must match.")
  id.lu <- cbind(data.dt[[id.var]], 1:nrow(data.dt))
  colnames(id.lu) <- c(id.var, ".row")
  id.lu <- as.data.table(id.lu)
  setorder(id.lu, .row)
  mar.lu <- list()
  for(i in seq_along(marginal.ids)) {
    mar.lu[[i]] <- id.lu[id.col %in% marginal.ids[[i]],
                         env = list(id.col = id.var)]
    mar.lu[[i]][, marginal := names(marginals)[i]]
  }
  mar.lu <- rbindlist(mar.lu)
  mar.lu$chunk <-
    cut(mar.lu$.row,
        with(predict.chunks, c(to[1] - size[1], to)),
        labels = FALSE)
  mar.lu <-
    mar.lu[,
           .(n = .N,
             row.ids = list(mar.lu$.row[.I]),
             ids = list(mar.lu[[id.var]][.I])),
           by = c("marginal", "chunk")]
  mar.lu[, `:=`(eval.id.from = 1 + cumsum(n) - n, eval.id.to = cumsum(n)), marginal]
  evaluated <- list()
  for(i in seq_along(marginals)) {
      # idx <- id.lu[eval(parse(text = paste(id.col, "%in% marginal.ids[[i]]")))]
      idx <- id.lu[id.col %in% marginal.ids[[i]],
                   env = list(id.col = id.var)]
      evaluated[[i]] <- Matrix(numeric(0), nrow = nrow(idx), ncol = m)
      dimnames(evaluated[[i]])[1] <- list(paste0(id.var, ":", idx[[id.var]]))
  }
  if(length(predict.chunks$from) < 2) progress <- FALSE
  if(progress) {
    prog <- txtProgressBar(min = 0, max = length(predict.chunks$from), initial = 0,
                           char = "=", width = NA, title = "Progress", style = 3)
  }
  for(i in 1:length(predict.chunks$from)) {
    Xp <-
      predict(model,
              newdata = data.dt[predict.chunks$from[i]:predict.chunks$to[i],],
              type = "lpmatrix",
              block.size = predict.chunks$size[i],
              newdata.guaranteed = TRUE,
              discrete = FALSE) |>
      Matrix()
    for(j in 1:length(marginals)) {
      chunk.lu <- mar.lu[chunk == i & marginal == names(marginals)[j]]
      if(nrow(chunk.lu) < 1) next
      pc.rows <- unlist(chunk.lu$row.ids) - with(predict.chunks, to[i] - size[i])
      eval.rows <- with(chunk.lu, eval.id.from:eval.id.to)
      m.predict.chunk <- Matrix(numeric(0), nrow = chunk.lu$n, ncol = m)
      for(k in 1:length(post.chunks$from)) {
        lp <-
          Xp[pc.rows, marginals[[j]]] %*%
          t(posterior[post.chunks$from[k]:post.chunks$to[k], marginals[[j]]])
        if(type == "response") {
          resp <- mod.fam$linkinv(as.matrix(lp))
          if(epred == TRUE) {
            m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- resp
          } else {
            chunk.wt <- pred.wt[predict.chunks$from[i]:predict.chunks$to[i]]
            postpred <-
              apply(resp, 2, \(x) fun.sim(mu = x, wt = chunk.wt, scale = mod.scale))
            m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- postpred
          }
        } else {
          m.predict.chunk[, post.chunks$from[k]:post.chunks$to[k]] <- lp
        }
        rm(lp)
      }
      evaluated[[j]][eval.rows,] <- m.predict.chunk
      rm(m.predict.chunk)
    }
    rm(Xp)
    gc()
    if(progress) {
      setTxtProgressBar(prog, i)
    }
  }
  if(progress) close(prog)
  evaluated.dt <- list()
  for(i in seq_along(evaluated)) {
    eval.dt <- as.data.table(as.matrix(t(evaluated[[i]])))
    eval.dt[, draw.col := 1:nrow(eval.dt), env = list(draw.col = draw.name)]
    toid <- list(as.integer)
    names(toid) <- id.var
    evaluated.dt[[i]] <-
      melt(eval.dt,
           measure.vars = measurev(toid, pattern = paste0(id.var, ":(.*)")),
           value.name = pred.name)
    setkeyv(evaluated.dt[[i]], id.var)
    setindexv(evaluated.dt[[i]], draw.name)
    setorderv(evaluated.dt[[i]], c(draw.name, id.var))
  }
  if(length(marginals) == 1) {
    return(evaluated.dt[[1]])
  } else {
    names(evaluated.dt) <- names(marginals)
    return(evaluated.dt)
  }
}


egp_posterior_predict <- function(model,
                                  posterior,
                                  data = NULL,
                                  id.var = "id",
                                  type = "response",
                                  epred = FALSE,
                                  ids = NULL,
                                  pred.name = NULL,
                                  predict.chunk = NULL,
                                  post.chunk = NULL,
                                  progress = TRUE,
                                  ...
                                  ) {
  if(is.null(pred.name)) {
    if(type == "response") {
      pred.name <- all.vars(update(model$formula, . ~ 1))
    } else {
      pred.name <- ".eta"
    }
  }
  predicted <-
    .evaluate_posterior(model = model,
                        posterior = posterior,
                        data = data,
                        id.var = id.var,
                        pred.name = pred.name,
                        type = type,
                        epred = epred,
                        obs = ids,
                        predict.chunk = predict.chunk,
                        post.chunk = post.chunk,
                        progress = progress,
                        ...)
  return(predicted)
}



.aggregate_variables <- function(predictions, ...) {
  UseMethod(".aggregate_variables", predictions)
}



.aggregate_variables.data.table <- 
  function(predictions,
           agg.fun = mean,
           trans.fun = NULL,
           ids = NULL,
           weights = NULL,
           pred.var = "predicted",
           draw.var = ".draw",
           id.var = "id",
           agg.name = "aggregated",
           group.name = "group.id",
           draw.ids = NULL,
           draw.chunk = NULL,
           agg.size = NULL,
           parallel = NULL,
           progress = TRUE,
           ...) {
  predictions.dt <- as.data.table(predictions)
  setkeyv(predictions.dt, id.var)
  setindexv(predictions.dt, draw.var)
  if(is.numeric(parallel)) {
    dt.threads.old <- getDTthreads()
    setDTthreads(parallel)
  }
  if(is.null(draw.ids)) {
    draw.ids <- predictions.dt[, unique(draw.col), env = list(draw.col = draw.var)]
  }
  if(is.null(draw.chunk)) {
    draw.chunk <- length(draw.ids)
  }
  if(is.null(ids)) {
    ids <- list(predictions.dt[, unique(id.col), env = list(id.col = id.var)])
  }
  if(is.null(weights)) {
    weights <- lapply(ids, \(x) rep(1, length(x)))
  }
  draw.chunks <- chunk_seq(1, length(draw.ids), draw.chunk)
  if(is.null(agg.size))
    agg.size <- min(unlist(lapply(ids, length)))
  ids.dt <-
    list(1:length(ids), ids, weights) |>
    as.data.table() |>
    setnames(c(group.name, id.var, ".w"))
  setorderv(ids.dt, group.name)
  ids.dt[,N := as.numeric(unname(unlist(lapply(ids, length))))]
  ids.dt[order(-N), Nc := cumsum(N)]
  ids.dt[,
         `:=`(aggregate = ifelse(N < agg.size, TRUE, FALSE))
         ][aggregate == TRUE, 
           agg.id := ceiling(Nc / agg.size)]
  setkeyv(ids.dt, group.name)
  agg.ids <- na.omit(unique(ids.dt[order(agg.id), agg.id]))
  single.ids <- ids.dt[aggregate == FALSE,
                       group.col,
                       env = list(group.col = group.name)]
  if(all(ids.dt$N == 0)) return(NULL)
  ids.dt.l <-
    ids.dt[order(-N),
           .(id.col = unlist(id.col),
             .w = unlist(.w)),
           by = c(group.name, "agg.id"),
           env = list(id.col = id.var)]
  setkeyv(ids.dt.l, id.var)
  setindexv(ids.dt.l, list(group.name, "agg.id"))
  if(!is.null(trans.fun)) {
    predictions.dt[,
                   pred.col := trans(pred.col),
                   env = list(pred.col = pred.var,
                              trans = trans.fun)]
  }
  if(progress) {
    prog <- txtProgressBar(min = 0, max = max(ids.dt$Nc) * length(draw.chunks$from), initial = 0,
                           char = "=", width = NA, title = "Progress", style = 3)
    prog.counter <- 0
  }
  draws.agg.l <- list()
  for(i in seq_along(draw.chunks$from)) {
    draws.sum <- draw.ids[draw.chunks$from[i]:draw.chunks$to[i]]
    predictions.draws <- predictions.dt[.(draws.sum),
                                        on = draw.var,
                                        nomatch = NULL]
    setkeyv(predictions.draws, id.var)
    single.l <- list()
    for(j in seq_along(single.ids)) {
      ids.sum <- ids.dt.l[.(single.ids[j]), on = group.name]
      single.l[[j]] <-
        predictions.draws[.(ids.sum),
                          nomatch = NULL,
                          on = id.var
                          ][order(draw.col, group.col),
                            .(agg.col = agg.fun(.w*pred.col)),
                            by = c(draw.var, group.name),
                            env = list(group.col = group.name,
                                       draw.col = draw.var,
                                       pred.col = pred.var,
                                       agg.col = agg.name,
                                       agg = agg.fun)]
      if(progress) {
        prog.counter <- prog.counter + ids.dt[group.col == single.ids[j],
                                              N,
                                              env = list(group.col = group.name)]
        setTxtProgressBar(prog, prog.counter)
      }
    }
    agg.l <- list()
    for(k in seq_along(agg.ids)) {
      match.ids.groups <- ids.dt.l[.(agg.ids[k]), on = "agg.id"]
      agg.l[[k]] <-
        predictions.draws[match.ids.groups,
                          nomatch = NULL,
                          on = id.var,
                          allow.cartesian = TRUE
                          ][order(draw.col, group.col),
                            .(agg.col = agg.fun(.w*pred.col)),
                            by = c(draw.var, group.name),
                            env = list(group.col = group.name,
                                       draw.col = draw.var,
                                       pred.col = pred.var,
                                       agg.col = agg.name,
                                       agg = agg.fun)]
      if(progress) {
        prog.counter <- prog.counter + sum(ids.dt[agg.id == agg.ids[k], N])
        setTxtProgressBar(prog, prog.counter)
      }
    }
    draws.agg.l[[i]] <- rbindlist(c(single.l, agg.l), fill = TRUE)
  }
  draws.agg <- rbindlist(draws.agg.l, fill = TRUE)
  setorderv(draws.agg, c(draw.var, group.name))
  if(progress) close(prog)
  if(is.numeric(parallel)) {
    setDTthreads(dt.threads.old)
  }
  return(draws.agg)
}


egp_summarize_units <- function(predictions,
                                cf.def,
                                pred.var = NULL,
                                draw.chunk = NULL,
                                agg.size = NULL,
                                parallel = NULL,
                                progress = TRUE,
                                ...
                                ){

  predictions.dt <- as.data.table(predictions)

  id.var <- cf.def$id.var
  unit.var <- cf.def$unit.var
  compare.by <- cf.def$compare.by
  units <- copy(cf.def$units)

  if(is.null(pred.var)) {
    pred.sel <- which(!names(predictions.dt) %in% c(id.var, ".draw"))[1]
    pred.var <- names(predictions.dt)[pred.sel]
  }

  unit.sum <-
    .aggregate_variables(predictions.dt,
                         agg.fun = mean,
                         ids = units[[id.var]],
                         pred.var = pred.var,
                         draw.var = ".draw",
                         id.var = id.var,
                         agg.name = pred.var,
                         group.name = ".uid",
                         draw.chunk = draw.chunk,
                         agg.size = agg.size,
                         parallel = parallel,
                         progress = progress)

  units[, .uid := 1:.N]

  unit.sum <-
    merge(units[, -id.var, with = FALSE], unit.sum, all.x = FALSE) |>
    _[, -".uid", with = FALSE]
  setindexv(unit.sum, ".draw")
  setorderv(unit.sum, c(compare.by, unit.var, ".draw"))
  return(unit.sum)
}


egp_evaluate_factual <- function(predictions,
                                 cf.def,
                                 name = "factual",
                                 group.eval = NULL,
                                 pred.var = NULL,
                                 draw.chunk = NULL,
                                 agg.size = NULL,
                                 parallel = NULL,
                                 progress = TRUE,
                                 ...
                                 ){

  predictions.dt <- as.data.table(predictions)

  id.var <- cf.def$id.var
  id.fac <- paste(id.var, cf.def$assign.cat[2], sep = ".")
  id.cf <- paste(id.var, cf.def$assign.cat[1], sep = ".")
  group.var <- cf.def$group.var

  if(is.null(pred.var)) {
    pred.sel <- which(!names(predictions.dt) %in% c(id.var, ".draw"))[1]
    pred.var <- names(predictions.dt)[pred.sel]
  }

  if(is.null(group.eval)) {
    group.eval <- cf.def$groups[[group.var]]
  }

  factual <-
    .aggregate_variables(predictions.dt,
                         agg.fun = mean,
                         ids = cf.def$groups[.(group.eval),
                                             id.col,
                                             on = group.var,
                                             env = list(id.col = id.fac)],
                         pred.var = pred.var,
                         draw.var = ".draw",
                         id.var = id.var,
                         agg.name = name,
                         group.name = group.var,
                         draw.chunk = draw.chunk,
                         agg.size = agg.size,
                         parallel = parallel,
                         progress = progress)

  factual[,
          group.col := group.eval[group.col],
          env = list(group.col = group.var)]
  factual.aug <-
    merge(cf.def$groups[, -c(id.fac, id.cf, ".w"), with = FALSE], factual, all.x = FALSE)

  setkeyv(factual.aug, group.var)
  setindexv(factual.aug, ".draw")
  return(factual.aug)
}


egp_evaluate_counterfactual <- function(predictions,
                                        cf.def,
                                        name = "counterfactual",
                                        group.eval = NULL,
                                        pred.var = NULL,
                                        draw.chunk = NULL,
                                        agg.size = NULL,
                                        parallel = NULL,
                                        progress = TRUE,
                                        ...
                                        ){

  predictions.dt <- as.data.table(predictions)

  id.var <- cf.def$id.var
  id.fac <- paste(id.var, cf.def$assign.cat[2], sep = ".")
  id.cf <- paste(id.var, cf.def$assign.cat[1], sep = ".")
  group.var <- cf.def$group.var

  if(is.null(pred.var)) {
    pred.sel <- which(!names(predictions.dt) %in% c(id.var, ".draw"))[1]
    pred.var <- names(predictions.dt)[pred.sel]
  }

  if(is.null(group.eval)) {
    group.eval <- cf.def$groups[[group.var]]
  }

  counterfactual <-
    .aggregate_variables(predictions.dt,
               agg.fun = sum,
               ids = cf.def$groups[.(group.eval),
                                   id.col,
                                   on = group.var,
                                   env = list(id.col = id.cf)],
               weights = cf.def$groups[.(group.eval),
                                       .w,
                                       on = group.var],
               pred.var = pred.var,
               draw.var = ".draw",
               id.var = id.var,
               agg.name = name,
               group.name = group.var,
               draw.chunk = draw.chunk,
               agg.size = agg.size,
               parallel = parallel,
               progress = progress)

  counterfactual[,
                 group.col := group.eval[group.col],
                 env = list(group.col = group.var)]
  counterfactual.aug <-
    merge(cf.def$groups[, -c(id.fac, id.cf, ".w"), with = FALSE], counterfactual, all.x = FALSE)

  setkeyv(counterfactual.aug, group.var)
  setindexv(counterfactual.aug, ".draw")
  return(counterfactual.aug)
}


egp_marginal <- function(factual,
                         counterfactual,
                         type = "absolute",
                         marginal.name = "marginal",
                         fac.var = NULL,
                         cf.var = NULL) {

  names.com <- intersect(names(factual),
                         names(counterfactual))
  if(is.null(fac.var)) {
    fac.var <-
      names(factual)[which(!names(factual) %in% names.com)[1]]
  }
  if(is.null(cf.var)) {
    cf.var <-
      names(counterfactual)[which(!names(counterfactual) %in% names.com)[1]]
  }

  marginal <-
    merge(factual, counterfactual, by = names.com)

  if(type == "absolute") {
    marginal[,
             marginal.col := fac.col - cf.col,
             env = list(marginal.col = marginal.name,
                        fac.col = fac.var,
                        cf.col = cf.var)]
  }
  if(type == "relative") {
    marginal[,
             marginal.col := fac.col / cf.col,
             env = list(marginal.col = marginal.name,
                        fac.col = fac.var,
                        cf.col = cf.var)]
  }
  
  return(marginal)
}


## Imbalance measures

d_cohen <- function(x, y, na.rm = FALSE) {
  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  mu_x <- mean(x)
  mu_y <- mean(y)
  s <- sqrt((sum((x-mu_x)^2) + sum((y-mu_y)^2)) / (length(x) + length(y) - 2))
  d <- (mu_x - mu_y) / s
  return(d)
}


d_cohen_w <- function(x, y, wx, wy, na.rm = FALSE) {
  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  wx <- wx/sum(wx)
  wy <- wy/sum(wy)
  mu_x <- weighted.mean(x, wx)
  mu_y <- weighted.mean(y, wy)
  s2_x <- sum(wx * (x - mu_x)^2)
  s2_y <- sum(wy * (y - mu_y)^2)
  n_x <- length(x)
  n_y <- length(y)
  s <- sqrt((((n_x - 1) * s2_x) + ((n_x - 1) * s2_x)) / (n_x + n_y - 2))
  d <- (mu_x - mu_y) / s
  return(d)
}


var_ratio <- function(x, y, na.rm = FALSE) {
  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  var(x) / var(y)
}


var_ratio_w <- function(x, y, wx, wy, na.rm = FALSE) {
  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  wx <- wx/sum(wx)
  wy <- wy/sum(wy)
  mu_x <- weighted.mean(x, wx)
  mu_y <- weighted.mean(y, wy)
  s2_x <- sum(wx * (x - mu_x)^2)
  s2_y <- sum(wy * (y - mu_y)^2)
  s2_x / s2_y
}


ks_stat <- function(x, y, na.rm = FALSE) {
  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  n.x <- length(x)
  n.y <- length(y)
  v <- c(x, y)
  w <- c(rep(1/n.x, length(x)), rep(-1/n.y, length(y)))
  ind <- order(v)
  z <- abs(cumsum(w[ind]))
  z <- z[diff(v[ind]) != 0]
  ks <- ifelse(length(z) > 0, max(z), 0)
  return(ks)
}

ks_stat_w <- function(x, y, wx, wy, na.rm = FALSE) {
  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  n.x <- length(x)
  n.y <- length(y)
  wx <- wx / sum(wx)
  wy <- -wy / sum(wy)
  v <- c(x, y)
  w <- c(wx, wy)
  ind <- order(v)
  z <- abs(cumsum(w[ind]))
  z <- z[diff(v[ind]) != 0]
  ks <- ifelse(length(z) > 0, max(z), 0)
  return(ks)
}


get_breaks <- function(x, breaks = "FD", ...) {
  x.range <- range(x)
  if(is.character(breaks)) {
    breaks <- match.arg(tolower(breaks),
                        c("fd", "scott", "sturges"))
    breaks <- switch(breaks,
                     fd = nclass.FD(x),
                     scott = nclass.scott(x),
                     sturges = nclass.Sturges(x),
                     stop(paste("Unknown algorithm to calculate breaks.",
                                "Possible values are: `FD`, `Scott`, or `Sturges`.")))
  }
  if(is.numeric(breaks) & length(breaks) == 1) {
    breaks <- seq(x.range[1], x.range[2], length.out = breaks+1)
  }
  return(breaks)
}

assign_breaks <- function(x, breaks = "FD", ...) {
  breaks <- get_breaks(x, breaks = breaks)
  breaks.assigned = cut(x, breaks, include.lowest = TRUE, labels = FALSE)
  return(breaks.assigned)
}


to_pdist <- function(x, ...) {
    p <- density(x, ...)$y
    p <- p/sum(p)
    return(p)
}


to_pdist_w <- function(x, wx, ...) {
  wx <- wx / sum(wx)
  p <- density(x, weights = wx, warnWbw = FALSE, ...)$y
  p <- p/sum(p)
  return(p)
}


to_pmass <- function(x, breaks = "FD", ...) {
  x.dt <- data.table(x, h = assign_breaks(x, breaks = breaks))
  x.dt <- x.dt[, .(x = .N/nrow(x.dt)), h]
  if(nrow(x.dt) < length(breaks)) {
    i.br <- 1:length(breaks)
    x.dt <- rbind(x.dt, data.table(h = i.br[!(i.br %in% x.dt$h)], x = 0))
  }
  return(x.dt[is.na(x), x := 0][order(h), x])
}


to_pmass_w <- function(x, wx, breaks = "FD", ...) {
  x.dt <- data.table(x, h = assign_breaks(x, breaks = breaks), w = wx / sum(wx))
  x.dt <- x.dt[, .(x = sum(w)), h]
  if(nrow(x.dt) < length(breaks)) {
    i.br <- 1:length(breaks)
    x.dt <- rbind(x.dt, data.table(h = i.br[!(i.br %in% x.dt$h)], x = 0))
  }
  return(x.dt[is.na(x), x := 0][order(h), x])
}

kl_div <- function(
                   x,
                   y,
                   base = 2,
                   symmetric = FALSE,
                   type = "raw",
                   disc.breaks = "FD",
                   dens.range = NULL,
                   dens.n = 512,
                   dens.bw = "SJ") {
  if(type == "density") {
    if(is.null(dens.range)) {
      p.iqr <- max(c(IQR(x), IQR(y)))
      p.range <- range(c(range(x), range(y))) + (0.1 * c(-p.iqr, p.iqr))
    }
    if(is.null(dens.n)) {
      dens.n <- 512
    }
    x <- to_pdist(x, bw = dens.bw, from = p.range[1], to = p.range[2], n = dens.n)
    y <- to_pdist(y, bw = dens.bw, from = p.range[1], to = p.range[2], n = dens.n)
  }
  if(type == "discrete") {
    disc.breaks <- get_breaks(c(x, y), disc.breaks = disc.breaks)
    x <- to_pmass(x, breaks = disc.breaks)
    y <- to_pmass(y, breaks = disc.breaks)
  }
  x[x == 0] <- .Machine$double.eps
  y[y == 0] <- .Machine$double.eps
  if(length(x) != length(y)) {
    print(x)
    print(y)
    # stop("Wrong length.")
  }
  if(symmetric) {
    d <-
      0.5 *
      (kl_div(x, y, base = base, symmetric == FALSE, type = "raw") /
       kl_div(y, x, base = base, symmetric == FALSE, type = "raw"))
  } else {
    d <- sum(x * log(x / y, base = base))
  }
  return(d)
}


kl_div_w <- function(
                     x,
                     y,
                     wx,
                     wy,
                     base = 2,
                     symmetric = FALSE,
                     type = "raw",
                     disc.breaks = "FD",
                     dens.range = NULL,
                     dens.n = 512,
                     dens.bw = "SJ") {
  wx <- wx / sum(wx)
  wy <- wy / sum(wy)
  if(type == "density") {
    if(is.null(dens.range)) {
      p.iqr <- max(c(IQR(x), IQR(y)))
      p.range <- range(c(range(x), range(y))) + (0.1 * c(-p.iqr, p.iqr))
    }
    if(is.null(dens.n)) {
      dens.n <- 512
    }
    x <- to_pdist_w(x, wx, bw = dens.bw, from = p.range[1], to = p.range[2], n = dens.n)
    y <- to_pdist_w(y, wy, bw = dens.bw, from = p.range[1], to = p.range[2], n = dens.n)
  }
  if(type == "discrete") {
    disc.breaks <- get_breaks(c(x, y), disc.breaks = disc.breaks)
    x <- to_pmass_w(x, wx, breaks = disc.breaks)
    y <- to_pmass_w(y, wy, breaks = disc.breaks)
  }
  if(type == "raw") {
    x <- x * wx
    y <- y * wy
  }
  x[x == 0] <- .Machine$double.eps
  y[y == 0] <- .Machine$double.eps
  if(length(x) != length(y)) {
    stop("Wrong length.")
  }
  if(symmetric) {
    d <-
      0.5 *
      (kl_div(x, y, base = base, symmetric == FALSE, type = "raw") /
       kl_div(y, x, base = base, symmetric == FALSE, type = "raw"))
  } else {
    d <- sum(x * log(x / y, base = base))
  }
  return(d)
}


js_div <- function(x,
                   y,
                   base = 2,
                   type = "raw",
                   disc.breaks = "FD",
                   dens.range = NULL,
                   dens.n = 512,
                   dens.bw = "SJ",
                   na.rm = FALSE) {
  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  if(type == "density") {
    if(is.null(dens.range)) {
      p.iqr <- max(c(IQR(x), IQR(y)))
      p.range <- range(c(range(x), range(y))) + (0.1 * c(-p.iqr, p.iqr))
    }
    if(is.null(dens.n)) {
      dens.n <- 512
    }
    x <- to_pdist(x, bw = dens.bw, from = p.range[1], to = p.range[2], n = dens.n)
    y <- to_pdist(y, bw = dens.bw, from = p.range[1], to = p.range[2], n = dens.n)
  }
  if(type == "discrete") {
    disc.breaks <- get_breaks(c(x, y), breaks = disc.breaks)
    x <- to_pmass(x, breaks = disc.breaks)
    y <- to_pmass(y, breaks = disc.breaks)
  }
  x[x == 0] <- .Machine$double.eps
  y[y == 0] <- .Machine$double.eps
  z <- 0.5 * (x + y)
  d <-
    0.5 *
    (kl_div(x, z, base = base, symmetric = FALSE, type = "raw") +
     kl_div(y, z, base = base, symmetric = FALSE, type = "raw"))
  return(d)
}


js_div_w <- function(x,
                     y,
                     wx,
                     wy,
                     base = 2,
                     type = "raw",
                     disc.breaks = "FD",
                     dens.range = NULL,
                     dens.n = 512,
                     dens.bw = "SJ",
                     na.rm = FALSE) {
  wx <- wx / sum(wx)
  wy <- wy / sum(wy)
  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  if(type == "density") {
    if(is.null(dens.range)) {
      p.iqr <- max(c(IQR(x), IQR(y)))
      p.range <- range(c(range(x), range(y))) + (0.1 * c(-p.iqr, p.iqr))
    }
    if(is.null(dens.n)) {
      dens.n <- 512
    }
    x <- to_pdist_w(x, wx, bw = dens.bw, from = p.range[1], to = p.range[2], n = dens.n)
    y <- to_pdist_w(y, wy, bw = dens.bw, from = p.range[1], to = p.range[2], n = dens.n)
  }
  if(type == "discrete") {
    disc.breaks <- get_breaks(c(x, y), breaks = disc.breaks)
    x <- to_pmass_w(x, wx, breaks = disc.breaks)
    y <- to_pmass_w(y, wy, breaks = disc.breaks)
  }
  if(type == "raw") {
    x <- x * wx
    y <- y * wy
  }
  x[x == 0] <- .Machine$double.eps
  y[y == 0] <- .Machine$double.eps
  z <- 0.5 * (x + y)
  d <-
    0.5 *
    (kl_div(x, z, base = base, symmetric = FALSE, type = "raw") +
     kl_div(y, z, base = base, symmetric = FALSE, type = "raw"))
  return(d)
}



















bias <- function(x, y = 1, ...) {
  bias <- mean(x, ...) - y
  return(bias)
}

rmse <- function(x, y = mean(x), ...) {
  rmse <- sqrt(mean((x - y)^2, ...))
  return(rmse)
}

ser <- function(x, y = 1, ...) {
  ser <- sum(sign(y) * x < 0)/length(x)
  return(ser)
}

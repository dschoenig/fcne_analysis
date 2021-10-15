sm <- bam(forestloss ~ P + 
          s(som.x, som.y, bs = 'gp', k = 500) +
          s(som.x, som.y, bs = 'gp', by = adm0, k = 500) +
          s(adm0, bs = 're') +
          s(ed_east, ed_north, bs = 'gp', k = 500),
          family = binomial(link = "cloglog"),
          data = cam.mod.1e6,
          drop.intercept = TRUE,
          select = TRUE,
          paraPen = list(P = list(diag(9))),
          discrete = TRUE,
          nthreads = c(2,1)
          )



        modfit <- bam(as.formula(paste0(models$response[i], " ~ ", models$predictor[i])),
                      family = binomial(link = models$link[i]),
                      data = data,
                      drop.intercept = models$drop.intercept[i],
                      # gamma = models$gamma[i],
                      select = models$select[i],
                      paraPen = models$paraPen[[i]],
                      chunk.size = chunk.size,
                      discrete = TRUE,
                      nthreads = c(2,1),
                      gc.level = gc.level
        )

using <- function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
  }

using("mgcv", "data.table", "kohonen",
      "mvnfast", "posterior", "arrow", "dplyr",
      "bayesplot", "ggplot2", "ggdist", "patchwork")

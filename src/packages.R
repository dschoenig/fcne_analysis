# module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

install.packages(c(
                   "bayesplot",
                   "colorspace",
                   "data.table",
                   "doParallel",
                   "dplyr",
                   "ggdist",
                   "ggpattern",
                   "ggplot2",
                   "igraph",
                   "KernSmooth",
                   "kohonen",
                   "lwgeom",
                   "Matrix",
                   "mgcv",
                   "mvnfast",
                   "patchwork",
                   "posterior",
                   "sf",
                   "spdep",
                   "stars",
                   "stringi"
                   ))

data.table::update_dev_pkg()

# install.packages("arrow", configure.vars = "LIBARROW_MINIMAL=false")
install.packages("arrow")

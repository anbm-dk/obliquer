# Script for tersting

# install.packages("devtools")
library(devtools)

test_download <- FALSE

if (test_download) {
  install_github("anbm-dk/obliquer")
  library(obliquer)
} else {
  load_all()
}

library(terra)

f <- system.file("ex/elev.tif", package = "terra")
r <- rast(f)

r2 <- r*2

ogc_r <- obliquify(
  r2, filename = "ogc_test4.tif", overwrite = TRUE,
  na.rm = TRUE)

ogc_r <- obliquify(
  r2, filename = "ogc_test3.tif", overwrite = TRUE,
  na.rm = TRUE, n_cores = 2, n_digits = 2)

ogc_r

plot(ogc_r)

# END

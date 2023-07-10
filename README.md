
<!-- README.md is generated from README.Rmd. Please edit that file -->

# obliquer

The package is currently in development.

The goal of `obliquer` is to produce geographic layers with oblique
geographic coordinates (OGCs) for use as covariates for spatial machine
learning models. OGCs are the coordinates of the grid cells along a
series of axes rotated at oblique angles relative to the x-axis. These
coordinate rasters can be used as covariates for machine learning
models, in order to incorporate spatial relationships in the prediction.

For questions about the package, please contact Anders Bjørn Møller
([email](mailto:anbm@agro.au.dk)).

## Installation

Make sure the devtools package is installed first:

``` r
install.packages('devtools')

library(devtools)

install_github("anbm-dk/obliquer")
```

## Example

This example shows how to generate rasters with coordinates tilted at
six different angles for the Vindum dataset:

``` r
library(obliquer)
library(terra)
#> terra 1.7.39
my_cov <- unwrap(Vindum_covariates)
crs(my_cov) <- "EPSG:25832"
ogcs <- obliquify(my_cov, 6)
ogcs
#> class       : SpatRaster 
#> dimensions  : 246, 294, 6  (nrow, ncol, nlyr)
#> resolution  : 1.6, 1.6  (x, y)
#> extent      : 534870.4, 535340.8, 6247733, 6248126  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89 / UTM zone 32N (EPSG:25832) 
#> source(s)   : memory
#> names       :     pi00,    pi17,    pi33,    pi50,    pi67,    pi83 
#> min values  : 534871.2, 3587150, 5678172, 6247734, 5143128, 2660309 
#> max values  : 535338.4, 3587606, 5678659, 6248124, 5143497, 2660792
plot(ogcs)
```

![](README_files/figure-gfm/example1-1.png)<!-- -->

The oblique geographic coordinates can be used as covariates for spatial
predictions. This example shows how to use them for predicting the
amounts of soil organic matter (SOM) in the Vindum field, using Random
Forest models trained using the package `ranger`. If ranger is not
installed, first run:

``` r
install.packages('ranger', dependencies = TRUE)
```

Then run the example:

``` r
my_obs <- unwrap(Vindum_SOM)
crs(my_obs) <- "EPSG:25832"

library(ranger)
pts1 <- extract(ogcs, my_obs, bind = TRUE)

# Create a formula for the model
fm1 <- as.formula(paste0('logSOM ~ ', paste(names(ogcs), collapse = ' + ')))


# Train the model
rf_ogc_1 <- ranger(
  fm1,
  data = values(pts1),
  mtry = 3,
  splitrule = 'extratrees',
  importance = 'impurity',
  min.node.size = 1,
  keep.inbag = TRUE
  )

# Predict SOM contents and show the results
prediction_1 <- predict(
  ogcs,
  rf_ogc_1,
  na.rm = TRUE,
  fun = function(model, ...) predict(model, ...)$predictions
  )

plot(prediction_1)
```

![](README_files/figure-gfm/example2-1.png)<!-- -->

The oblique geographic coordinates can also be used in combination with
the existing covariates for the field:

``` r
cov_ogcs <- c(ogcs, my_cov)

# Predict SOM as in the previous example
pts2 <- extract(cov_ogcs, my_obs, bind = TRUE)

fm2 <- as.formula(paste0('logSOM ~ ', paste(names(cov_ogcs), collapse = ' + ')))

rf_ogc_2 <- ranger(
  fm2,
  data = values(pts2),
  mtry = 6,
  splitrule = 'extratrees',
  importance = 'impurity',
  min.node.size = 1,
  keep.inbag = TRUE
  )

prediction_2 <- predict(
  cov_ogcs,
  rf_ogc_2,
  na.rm = TRUE,
  fun = function(model, ...) predict(model, ...)$predictions
  )

plot(prediction_2)
```

![](README_files/figure-gfm/example3-1.png)<!-- -->

The importance of the coordinate rasters can be plotted intuitively in
the same manner as a wind rose. This example requires the packages
`ggplot2` and `gridExtra`. If these two packages are not installed,
first run:

``` r
install.packages(c('ggplot2', 'gridExtra'), dependencies = TRUE)
```

Then run the example:

``` r

library(ggplot2)
library(gridExtra)

# Put covariate importance for the two Random Forest models into a list
imp <- list()

imp[[1]] <- data.frame(Overall = importance(rf_ogc_1))
imp[[2]] <- data.frame(Overall = importance(rf_ogc_2)[1:6])

imp <- lapply(
  imp,
  function(x) {
    x <- rbind(x, x)
    x$dir <- c(1:nrow(x))
    return(x)}
)

# Plot covariate importance for OGC in the two models
brks <- seq(
  from = min(imp[[1]]$dir),
  by = (max(imp[[1]]$dir) + 1 - min(imp[[1]]$dir))/4,
  length.out = 4
  )

titles <- c('A: Six angles', 'B: Six angles + ancillary data')

fig <- list()

for(i in 1:2)
{
  fig[[i]] <- ggplot(imp[[i]], aes(x = dir, y = Overall)) +
    coord_polar(start = -pi/2 - pi/(nrow(imp[[i]])), direction = -1) +
    geom_col(width = 1, colour = 'black', fill = rgb(0,2/3,2/3,1/2)) +
    ggtitle(titles[[i]]) +
    scale_x_continuous(breaks = brks, labels = c('E', 'N', 'W', 'S')) +
    ylab('Importance (variance)') +
    theme_bw() +
    theme(axis.text.x = element_text(
      colour = 'black'),
      axis.title.x = element_blank(),
      axis.text.y = element_text(colour = 'black'),
      panel.grid.major = element_line(color = 'grey'),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(linewidth = 1)
    )
}

grid.arrange(fig[[1]], fig[[2]], nrow = 1)
```

![](README_files/figure-gfm/example4-1.png)<!-- -->

The sizes of the bars show the importance of the coordinates tilted at
the angle of the bar. The bars are repeated for opposite angles, as the
importance is directionless.

## Citation

If you use the contents of the package, please refer to:

Møller, A.B., Beucher, A.M., Pouladi, N., Greve, M.H. (2020): Oblique
geographic coordinates as covariates for digital soil mapping. SOIL
6(2), 269-289. <https://dx.doi.org/10.5194/soil-6-269-2020>

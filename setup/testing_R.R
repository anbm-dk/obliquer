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

my_cov <- unwrap(Vindum_covariates)
crs(my_cov) <- "EPSG:25832"

my_obs <- unwrap(Vindum_SOM)
crs(my_obs) <- "EPSG:25832"

ogcs <- obliquify(my_cov, 6)

ogcs

plot(ogcs)

library(ranger)

pts1 <- extract(ogcs, my_obs, bind = TRUE)


# Create formula for the model

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

# Stack rasters with covariates and OGCs

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

install.packages(c('ggplot2', 'gridExtra'), dependencies = TRUE)

# Load packages

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


# END

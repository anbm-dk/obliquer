## code to prepare `DATASET` dataset goes here

library(dplyr)
library(terra)
library(magrittr)


wd <- paste0(getwd(), "/data-raw/")

# Load covariates

cov <- wd %>%
  paste0(., "/cov/") %>%
  list.files(full.names = TRUE) %>%
  rast() %>%
  subset(
    c("bluespot",
      "ECa",
      "elevation",
      "mrvbf",
      "sagawi",
      "valdepth"
)
    ) %>%
  round(2)

# Remove CRS (add again in example)

crs(cov) <- NULL

# Wrap

Vindum_covariates <- wrap(cov)

# Load soil observations

obs <- paste0(wd, '/shapefiles/TrainPoint.shp') %>% vect()
val <- paste0(wd, '/shapefiles/ValidationPoint.shp') %>% vect()

obs$dataset <- "Training"
val$dataset <- "Validation"

obs_all <- vect(list(obs, val)) %>%
  sort("ID")

vals <- obs_all %>%
  values() %>%
  mutate(
    UTMX = round(X_coordina, 0),
    UTMY = round(Y_coordina, 0),
    SOM = round(humus, 1),
    logSOM = round(ln_humus, 2),
    dataset = as.factor(dataset)
  ) %>%
  select(c(ID, UTMX, UTMY, SOM, logSOM, dataset))

values(obs_all) <- vals

crs(obs_all) <- NULL

Vindum_SOM <- wrap(obs_all)

# Create dataset

usethis::use_data(
  Vindum_covariates,
  Vindum_SOM,
  overwrite = TRUE
  )

# END

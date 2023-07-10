#' Covariates for the field Vindum
#'
#' A wrapped `SpatRaster` object with six data layers containing ancillary data from the field Vindum in Denmark (1.6 m resolution), inlcuding variables derived from a digital elevation model and a DUALEM-1 sensor.
#'
#' @format A wrapped `SpatRaster` with six layers:
#' \describe{
#'   \item{bluespot}{Depth of sinks (m)}
#'   \item{ECa}{Apparent electrical conductivity}
#'   \item{elevation}{Elevation (m) above sea level}
#'   \item{mrvbf}{Multiresolution index of valley bottom flatness}
#'   \item{sagawi}{SAGA GIS modified topographic wetness index}
#'   \item{valdepth}{Depth of valleys (m)}
#' }
#' @source \url{https://doi.org/10.1016/j.geoderma.2019.02.019}
#' @examples
#' library(terra)
#' my_cov <- unwrap(Vindum_covariates)
#' crs(my_cov) <- "EPSG:25832"
"Vindum_covariates"

#' Soil observatuins for the field Vindum
#'
#' A wrapped `SpatVector` object containing soil organic matter (SOM) observations from the field Vindum in Denmark.
#'
#' @format A wrapped `SpatVector` with six attributes:
#' \describe{
#'   \item{ID}{Point ID}
#'   \item{UTMX}{Coordinate x according to ETRS89 UTM Zone 32N (m)}
#'   \item{UTMY}{Coordinate y according to ETRS89 UTM Zone 32N (m)}
#'   \item{SOM}{Measured soil organic matter content (%)}
#'   \item{logSOM}{Natural log-transformed SOM}
#'   \item{dataset}{Particion into datasets for training and validation}
#' }
#' @source \url{https://doi.org/10.1016/j.geoderma.2019.02.019}
#' @examples
#' library(terra)
#' my_obs <- unwrap(Vindum_SOM)
#' crs(my_obs) <- "EPSG:25832"
"Vindum_SOM"

#' Generate oblique geographic coordinates
#'
#' Produces as `SpatRaster` object with a specified number of oblique geographic coordinates (OGCs; Møller et al., 2020).
#'
#' The rotation angles for the OGCs will be evenly spaced by default. Alternatively, specific rotation angles can be given in radians as a numeric vector.
#'
#' The parameter `n_cores` sets the number of cores for parallel processing. Note that this will not always speed up computation. By default the function uses sequential processing.
#'
#' Setting a higher value for `n_blocks` will decrease the memory usage of the function, which can be useful for large rasters.
#'
#' The parameters `n_blocks` and `n_cores` can be used together. The function processes the blocks sequentially, so `n_cores` will only cause parallel processing at the cell level.
#'
#' @references Møller, A.B., Beucher, A.M., Pouladi, N., Greve, M.H. (2020): Oblique geographic coordinates as covariates for digital soil mapping. SOIL 6(2), 269-289. \url{https://dx.doi.org/10.5194/soil-6-269-2020}
#'
#' @param x An input `SpatRaster` object created by package [terra].
#' @param n_angles Integer. Number of angles for rotation. Defaults to the length of `angles` when `angles` is not `NULL`.
#' @param angles Numeric vector with specific angles for rotation in radians.
#' @param na.rm Logical. Should the function exclude areas that are `NA` in the input `SpatRaster`? The default `TRUE` will only calculate OGCs for the cells in `x` that have values. If `FALSE`, the output will cover the entire extent of `x`.
#' @param n_digits Integer. Number of digits for rounding the values in the output `SpatRaster`.
#' @param digits_names Integer. Number of digits for rounding the rotation angles given in the layer names of the output. Only affects the layer names.
#' @param filename File name for writing the output `SpatRaster`.
#' @param n_blocks Number of blocks used for processing `x`.
#' @param n_cores Number of cores used for parallel processing. If `NULL`, no parallel processing is used.
#' @param ... Optional arguments for writing the output `SpatRaster`.
#' @returns A `SpatRaster` with oblique geographic coordinates.
#' @export
#' @importFrom foreach registerDoSEQ
#' @examples
#' library(terra)
#' f <- system.file("ex/elev.tif", package = "terra")
#' r <- rast(f)
#' ogc_r <- obliquify(r)
obliquify <- function(
    x,
    n_angles = 4,
    angles = NULL,
    na.rm = TRUE,
    n_digits = NULL,
    digits_names = NULL,
    filename = "",
    n_blocks = 4,
    n_cores = NULL,
    ...) {
  . <- NULL # To avoid warnings in the package check.

  if (is.null(angles)) {
    angles <- pi * seq(0, 1, 1 / (n_angles))[1:(n_angles)]
  } else {
    n_angles <- length(angles)
  }
  if (is.null(digits_names)) {
    digits_names <- ceiling(log10(n_angles) + 1)
  }
  names_angles <- paste0(
    "pi",
    formatC(
      (angles * 10^digits_names) / pi,
      format = "f",
      width = digits_names,
      digits = 0,
      flag = "0"
    )
  ) %>%
    make.names()
  out <- terra::rast(x[[1]], nlyrs = n_angles)
  names(out) <- names_angles
  out_info <- terra::writeStart(out, filename = filename, n = n_blocks, ...)
  out_info$ncell <- out_info$nrows * ncol(out)
  out_info$cumcell <- cumsum(out_info$ncell)
  out_info$startcell <- out_info$cumcell - out_info$ncell + 1
  stopifnot(out_info$cumcell[out_info$n] == terra::ncell(out))

  if (is.null(n_digits)) {
    rotate_xy <- function(xy) {
      xy_rot <- cos(angles - atan((xy[2] / xy[1]))) * sqrt(xy[1]^2 + xy[2]^2)
      return(xy_rot)
    }
  } else {
    rotate_xy <- function(xy) {
      xy_rot <- round(
        cos(angles - atan((xy[2] / xy[1]))) * sqrt(xy[1]^2 + xy[2]^2),
        digits = n_digits
      )
      return(xy_rot)
    }
  }

  if (!is.null(n_cores)) {
    clust <- parallel::makeCluster(n_cores)

    parallel::clusterExport(
      clust,
      list(
        "angles",
        "n_digits",
        "rotate_xy"
      ),
      envir = environment()
    )
  }

  for (i in 1:out_info$n) {
    idx <- out_info$startcell[i]:(out_info$startcell[i] - 1 + out_info$ncell[i])
    xy_out <- terra::xyFromCell(x[[1]], idx)
    if (na.rm) {
      nas_out <- !stats::complete.cases(x[idx])
      xy_out[nas_out, ] <- NA
    }
    if (is.null(n_cores)) {
      ogc_out <- apply(
        xy_out,
        1,
        rotate_xy
      ) %>% t()
    } else {
      ogc_out <- parallel::parRapply(
        cl = clust,
        x = xy_out,
        FUN = rotate_xy
      ) %>%
        matrix(nrow = n_angles) %>%
        base::t(.)
    }

    terra::writeValues(
      x = out,
      v = ogc_out,
      start = out_info$row[i],
      nrows = out_info$nrows[i]
    )
  }

  if (!is.null(n_cores)) {
    parallel::stopCluster(clust)
    foreach::registerDoSEQ()
    rm(clust)
  }

  out <- terra::writeStop(out)
  return(out)
}

# END

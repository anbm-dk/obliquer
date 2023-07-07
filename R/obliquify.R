#' Generate oblique geographic coordinates
#'
#' @param x A spatraster object.
#' @param n_angles Number of angles for rotation. Defaults to length of angles when angles is not null.
#' @param angles Numeric vector with specific angles for rotation, given in radians.
#' @param na.rm Exclude areas which are NA in the original SpatRaster.
#' @param n_digits Number of digits for rounding the oblique geographic coordinates.
#' @param digits_names Number of digits for the angles given in the layer names of the output.
#' @param filename File name for writing the output SpatRaster.
#' @param n_blocks Number of blocks used for processing the SpatRaster.
#' @param n_cores Number of cores used for parallel processing. If NULL, no parallel processing is used.
#' @param ... Optional arguments for writing the output SpatRaster.
#' @returns A SpatRaster with oblique geographic coordinates.
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
    ...
) {

  . <- NULL  # To avoid warnings in the package check.

  if (is.null(angles)) {
    angles <- pi*seq(0, 1, 1/(n_angles))[1:(n_angles)]
  } else {
    n_angles <- length(angles)
  }
  if (is.null(digits_names)) {
    digits_names <- max(
      2,
      nchar(as.character(n_angles))
    )
  }
  names_angles <- paste0(
    "pi",
    format(
      angles/pi,
      digits = digits_names,
      nsmall = digits_names
    )
  )
  out <- terra::rast(x[[1]], nlyrs = n_angles)
  names(out)         <- names_angles
  out_info <- terra::writeStart(out, filename = filename, n = n_blocks, ...)
  out_info$ncell     <- out_info$nrows * ncol(out)
  out_info$cumcell   <- cumsum(out_info$ncell)
  out_info$startcell <- out_info$cumcell - out_info$ncell + 1
  stopifnot(out_info$cumcell[out_info$n] == terra::ncell(out))

  if (is.null(n_digits)) {
    rotate_xy <- function(xy) {
      xy_rot <- cos(angles - atan((xy[2]/xy[1])))*sqrt(xy[1]^2 + xy[2]^2)
      return(xy_rot)
    }
  } else {
    rotate_xy <- function(xy) {
      xy_rot <- round(
        cos(angles - atan((xy[2]/xy[1])))*sqrt(xy[1]^2 + xy[2]^2),
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
      ) %>% matrix(nrow = n_angles) %>% base::t(.)
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

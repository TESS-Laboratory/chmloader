#' A gdalwarp utility function
#' @param srcs character, the source files to warp
#' @param target sf or sfc object, the target area to warp to
#' @param res numeric, the resolution of the target area
#' @param filename character, the filename to save the warped data to
#' @param gdalwarp_options character, the options to pass to gdalwarp
#' @param gdal_config_options character, the options to pass to gdal_config
#' @noRd
#' @keywords internal
warp_util <- function(
    srcs,
    target,
    res,
    filename,
    gdalwarp_options,
    gdal_config_options) {
  if (is.null(res)) {
    if (inherits(target, "SpatRaster")) {
      res <- terra::res(target)
      bbox <- get_ext(target)
    } else {
      res <- c(1, 1)
      bbox <- get_ext(target) |>
        round_bbox(res[1])
    }
  } else {
    bbox <- get_ext(target) |>
      round_bbox(res)
    res <- c(res, res)
  }

  dim_x <- box_dim(bbox, res, "x")
  dim_y <- box_dim(bbox, res, "y")

  te_srs <- get_proj(target)

  gdalwarp_options <- c(
    gdalwarp_options,
    "-te", bbox,
    "-t_srs", te_srs,
    "-ts", dim_x, dim_y
  )

  sf::gdal_utils("warp", srcs, filename,
    options = gdalwarp_options, config = gdal_config_options,
    quiet = TRUE
  )

  return(filename)
}

warp_util <- function(
    srcs,
    target,
    res,
    filename,
    gdalwarp_options,
    gdal_config_options) {
  Sys.setenv(AWS_NO_SIGN_REQUEST = "YES")

  bbox <- sf::st_bbox(target) |>
    round_bbox(res)

  te_srs <- sf::st_crs(target)$wkt

  gdalwarp_options <- c(gdalwarp_options, "-te", bbox, "-t_srs", te_srs)

  sf::gdal_utils("warp", srcs, filename,
    options = gdalwarp_options, config = gdal_config_options,
    quiet = TRUE
  )

  return(filename)
}

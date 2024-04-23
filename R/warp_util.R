warp_util <- function(
    srcs,
    target,
    res,
    filename = paste0(proceduralnames::make_english_names(1), ".tif"),
    gdalwarp_options = c(
      "-r", "bilinear",
      "-multi",
      "-overwrite",
      "-co", "COMPRESS=DEFLATE",
      "-co", "PREDICTOR=2",
      "-co", "NUM_THREADS=ALL_CPUS"
    ),
    gdal_config_options = c(
      VSI_CACHE = "TRUE",
      GDAL_CACHEMAX = "30%",
      VSI_CACHE_SIZE = "10000000",
      GDAL_HTTP_MULTIPLEX = "YES",
      GDAL_INGESTED_BYTES_AT_OPEN = "32000",
      GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
      GDAL_HTTP_VERSION = "2",
      GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
      GDAL_NUM_THREADS = "ALL_CPUS"
    )) {
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

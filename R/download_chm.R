#' Download Canopy Height Model (CHM) data
#' Downloads CHM data from the Tolan et al. (2023) dataset by meta nd WRI.
#' @param target sf or sfc object, the target area to download CHM data for
#' @param chm character, the CHM dataset to download, for now this must be "tolan"
#' @param res numeric, the resolution of the CHM data to download in meters. see details
#' @param filename character, the filename to save the downloaded CHM data to
#' @param gdalwarp_options character, the options to pass to gdalwarp
#' @param gdal_config_options character, the options to pass to gdal_config
#' @examplesIf interactive()
#'
#' sundarbans <- sf::st_point(c(89.2, 22.0)) |>
#'   sf::st_sfc(crs = 4326) |>
#'   sf::st_buffer(5000)
#'
#' sundarbans_chm <- download_chm(sundarbans)
#' sundarbans_chm
#' @details for more information check out: https://registry.opendata.aws/dataforgood-fb-forests/
#' If the target sf object is provided in longlat, it will be transformed to
#' EPSG:3857 (Web Mercator) before downloading the CHM data. This is in part
#' because the data is provided in Web Mercator and because calcuating the
#' correct resolution of the data is made simpler when using a projected
#' coordinate system.
#'
#' When the `res` argument is NULL (the default), the CHM resolution will be 1
#' meter if the target is a spatial vector (either sf, sfc or SpatVector. If
#' the target is a SpatRaster object, the resolution will be the same as the
#' target raster. If the `res` argument is provided, the CHM data will be
#' resampled to the requested resolution.
#'
#' @return character, the path to the downloaded CHM data
#' @export
download_chm <- function(
    target, chm = "tolan", res = NULL,
    filename = paste0(proceduralnames::make_english_names(1), ".tif"),
    gdalwarp_options = c(
      "-r", "bilinear",
      "-multi",
      "-overwrite",
      "-co", "COMPRESS=DEFLATE",
      "-co", "PREDICTOR=2",
      "-co", "NUM_THREADS=ALL_CPUS",
      "-of", "COG"
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
      GDAL_NUM_THREADS = "ALL_CPUS",
      AWS_NO_SIGN_REQUEST = "YES"
    )) {
  chml_assert_class(target, c("sf", "sfc", "SpatVector", "SpatRaster"))

  chm <- rlang::arg_match(chm)

  check_existing_cog(filename, gdalwarp_options)

  if (sf::st_is_longlat(target)) {
    cond <- inherits(target, "SpatRaster") && is.null(res)
    if (!cond) {
      target <- proj_to_web_merc(target)
      cli::cli_warn(c("!" = "target is in longlat, transforming to EPSG:3857"))
    }
  }

  srcs <- build_chm_srcs(target)

  r <- warp_util(
    srcs, target, res, filename,
    gdalwarp_options, gdal_config_options
  )

  r <- switch(getOption("chmloader.out_raster_type"),
    character = r,
    SpatRaster = terra::rast(r),
    r
  )
  return(r)
}

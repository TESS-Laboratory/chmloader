#' Download Canopy Height Model (CHM) data
#' Downloads CHM data from the Tolan et al. (2023) dataset by meta nd WRI.
#' @param target sf or sfc object, the target area to download CHM data for
#' @param chm character, the CHM dataset to download, for now this must be "tolan"
#' @param res numeric, the resolution of the CHM data to download in meters
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
#' @description for more information check out: https://registry.opendata.aws/dataforgood-fb-forests/
#' @return character, the path to the downloaded CHM data
#' @export
download_chm <- function(
    target, chm = "tolan", res = 1,
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
  checkmate::assert_multi_class(target, c("sf", "sfc"))
  chm <- rlang::arg_match(chm)

  if (sf::st_is_longlat(target)) {
    target <- sf::st_transform(target, 3857)
    cli::cli_warn(c("!" = "target is in longlat, transforming to EPSG:3857"))
  }

  srcs <- build_chm_srcs(target)

  warp_util(srcs, target, 1, filename, gdalwarp_options, gdal_config_options)
}

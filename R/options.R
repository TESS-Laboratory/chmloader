#' Safely set chmloader options
#' Helper function to safely set chmloader options such as the output raster type
#' @param out_raster_type character, the output raster type
#' @return NULL
#' @details This function can be used to set specific options for the chmloader
#' package. The only option currently available is `out_raster_type` which
#' determines the class of the output raster object from the `download_chm`
#' function. The default is "SpatRaster", but alternatively it can be set to
#' "character" to return the path to the downloaded CHM data.
#' @export
chml_set_options <- function(
    out_raster_type = c("SpatRaster", "character")) {
  out_raster_type <- rlang::arg_match(out_raster_type)
  options(chmloader.out_raster_type = out_raster_type)
  invisible()
}

#' Get the S3 paths to the CHM tiles that cover a given target area
#' @param target sf or sfc object, the target area to download CHM data for
#' @return character, the S3 paths to the CHM tiles that cover the target area
#' @noRd
#' @keywords internal
build_chm_srcs <- function(target) {
  target_corners <- get_spat_corners(target)

  kuam_cent_qk <- quadkeyr::latlong_to_quadkey(
    lat = target_corners$Y, lon = target_corners$X, zoom = 9
  )

  qk <- unique(kuam_cent_qk$quadkey)

  paste("/vsis3",
    "dataforgood-fb-data", "forests/v1/alsgedi_global_v6_float/chm",
    paste0(qk, ".tif"),
    sep = "/"
  )
}

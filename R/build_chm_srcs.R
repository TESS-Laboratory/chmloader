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

  if (length(qk) > 3) {
    # this catches when the target area intersects more than 3 tiles, in this
    # case we can build a quadkey grid with thes corner coords and extract all
    # intersecting tiles.
    box <- get_ext(target) |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_set_crs(get_proj(target)) |>
      sf::st_transform(4326) |>
      sf::st_bbox()

    qkg <- quadkeyr::create_qk_grid(box[1], box[3], box[2], box[4], zoom = 9)
    qk <- qkg$data$quadkey
  }

  paste("/vsis3",
    "dataforgood-fb-data", "forests/v1/alsgedi_global_v6_float/chm",
    paste0(qk, ".tif"),
    sep = "/"
  )
}

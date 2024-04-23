build_chm_srcs <- function(target) {
  target_corners <- sf::st_transform(target, 4326) |>
    sf::st_transform(4326) |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_coordinates() |>
    as.data.frame()

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

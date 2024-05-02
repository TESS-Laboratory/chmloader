#' Get the S3 paths to the CHM tiles that cover a given target area
#' @param target sf or sfc object, the target area to download CHM data for
#' @return character, the S3 paths to the CHM tiles that cover the target area
#' @noRd
#' @keywords internal
build_chm_srcs <- function(target) {
  gtiles <- load_tiles()

  t_ext <- get_ext(target) |>
    sf::st_bbox(crs = get_proj(target)) |>
    sf::st_as_sfc() |>
    sf::st_transform(4326)

  qk <- sf::st_filter(gtiles, t_ext)$tile

  paste("/vsis3",
    "dataforgood-fb-data", "forests/v1/alsgedi_global_v6_float/chm",
    paste0(qk, ".tif"),
    sep = "/"
  )
}

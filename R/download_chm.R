download_chm <- function(target, chm = "tolan", res = 1) {
  checkmate::assert_multi_class(target, c("sf", "sfc"))
  chm <- rlang::arg_match(chm)

  if (sf::st_is_longlat(target)) {
    target <- sf::st_transform(target, 3857)
    cli::cli_warn("target is in longlat, transforming to EPSG:3857")
  }

  srcs <- build_chm_srcs(target)

  warp_util(srcs, target, 1)
}

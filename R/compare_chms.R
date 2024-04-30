#' create a dataframe of two rasters
#' @param r1 a SpatRaster object
#' @param r2 a SpatRaster object
#' @return a dataframe of the two rasters
#' @noRd
#' @keywords internal
two_ras_df <- function(r1, r2) {
  names(r1) <- "benchmark_chm"
  names(r2) <- "meta_chm"
  purrr::map(
    list(r1, r2),
    ~ as.data.frame(.x, xy = TRUE)
  ) |>
    purrr::reduce(~ dplyr::left_join(.x, .y, by = c("x", "y")))
}

#' plot a map of two rasters from a df
#' @param ras_df a dataframe of two rasters from `two_ras_df`
#' @return a ggplot object
#' @noRd
#' @keywords internal
two_ras_map <- function(ras_df) {
  ras_plot_df <- ras_df |>
    tidyr::pivot_longer(
      cols = c(benchmark_chm, meta_chm),
      names_to = "model", values_to = "canopy height"
    ) |>
    dplyr::mutate(model = dplyr::case_when(
      model == "benchmark_chm" ~ "benchmark CHM",
      model == "meta_chm" ~ "Meta/WRI CHM"
    ))

  ggplot2::ggplot(ras_plot_df) +
    ggplot2::aes(x = x, y = y, fill = `canopy height`) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c(option = "viridis") +
    ggplot2::facet_wrap(~model, ncol = 2) +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    ggplot2::labs(fill = "Canopy Height (m)") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

#' plot the 2d density of the correspondence between two rasters
#' @param df a dataframe of two rasters from `two_ras_df`
#' @param fill_trans a character string of the transformation to apply to the
#' density
#' @param n an integer, Number of grid points in each direction for the 2D
#' density
#' @param dz a logical, whether to drop zeros from the dataframe when both
#' the benchmark and meta/WRI CHM are zero.
#' @return a ggplot object
#' @noRd
#' @keywords internal
density_2d <- function(
    df,
    fill_trans,
    n = 100,
    dz = TRUE) {
  trans_func <- switch(fill_trans,
    "sqrt" = sqrt,
    "log" = log,
    "log10" = log10,
    "identity" = identity
  )

  legend_lab <- switch(fill_trans,
    "sqrt" = paste0("density (sqrt)"),
    "log" = paste0("density (log)"),
    "log10" = paste0("density (log10)"),
    "identity" = "density"
  )

  em <- error_mets(df$benchmark_chm, df$meta_chm)
  if (dz) {
    df <- df |>
      dplyr::filter(!(benchmark_chm == 0 & meta_chm == 0))
  }


  ggplot2::ggplot(df) +
    ggplot2::aes(x = benchmark_chm, y = meta_chm) +
    ggplot2::stat_density2d(
      ggplot2::aes(fill = trans_func(ggplot2::after_stat(density))),
      geom = "raster", contour = FALSE, n = n, alpha = 0.7 # , h = c(1, 1)
    ) +
    ggplot2::scale_fill_gradientn(
      colors = hcl.colors(100, palette = "mako", rev = TRUE)
    ) +
    ggplot2::geom_abline(
      col = "#464646", lwd = 0.5, alpha = 0.7, linetype = 2,
      slope = 1, intercept = 0
    ) +
    ggplot2::theme_light() +
    ggplot2::labs(
      caption = bquote(
        ~ R^2:~ .(em["rsq"]) ~ "| "
        ~ RMSE:~ .(em["rmse"]) ~ "| "
        ~ MAE:~ .(em["mae"])
      ),
      x = "benchmark Canopy Height (m)",
      y = "Meta/WRI Canopy Height (m)",
      fill = legend_lab
    )
}

#' calculate error metrics
#' @param x a numeric vector
#' @param y a numeric vector
#' @return a numeric vector of error metrics
#' @noRd
#' @keywords internal
error_mets <- function(x, y) {
  rmse <- \(v1, v2)  sqrt(mean((v1 - v2)^2))
  rsq <- \(v1, v2)  cor(v1, v2)^2
  mae <- \(v1, v2)  mean(abs(v1 - v2))

  round(c(
    rsq = rsq(x, y),
    rmse = rmse(x, y),
    mae = mae(x, y)
  ), 1)
}

#' get the continent and biome of a raster
#' @param r a SpatRaster object
#' @return a character vector of the continent and biome
#' @noRd
#' @keywords internal
get_biome_n_cont <- function(r) {
  user_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(sf::sf_use_s2(user_s2))
  s_crs <- terra::crs(r)
  wgs_centroid <- terra::ext(r) |>
    terra::vect(crs = s_crs) |>
    terra::centroids() |>
    terra::project("EPSG:4326") |>
    terra::geom(df = TRUE) |>
    dplyr::select(x, y)

  wgs_pnt <- sf::st_point(c(wgs_centroid$x, wgs_centroid$y)) |>
    sf::st_sfc() |>
    sf::st_set_crs(4326)

  conts <- continents()

  biome <- biomes()

  return(
    c(
      cont_name = conts[sf::st_nearest_feature(wgs_pnt, conts), ]$continent,
      biome_name = biome[sf::st_nearest_feature(wgs_pnt, biome), ]$BIOME_NAME
    )
  )
}

#' stack two plots
#' @param p1 a ggplot object
#' @param p2 a ggplot object
#' @param g_att a character vector of the continent and biome
#' @param rr a numeric vector of the raster resolution
#' @param r_units a character string of the raster units
#' @param title a character string of the plot title
#' @return a ggplot object
#' @noRd
#' @keywords internal
#' @import patchwork
stack_plots <- function(p1, p2, g_att, rr, r_units, title) {
  p1 / p2 +
    patchwork::plot_annotation(
      title = title,
      subtitle = glue::glue(
        "Continent: {g_att['cont_name']}
    Biome: {g_att['biome_name']}
    Cell size: {rr[1]} x {rr[2]} {r_units}"
      ),
    )
}

#' Standardised plot to compare two Canopy Height Models
#' Create a multi-panel plot showing the benchmark raster alongside the
#' meta/WRI raster, and a 2D density plot of the correspondence between the two
#' rasters.
#' @param r1 a SpatRaster object, The benchmark raster (user provided)
#' @param r2 a SpatRaster object, The meta/WRI raster
#' @param fill_trans a character string of the transformation to apply to the
#' density colour gradient.
#' @param title a character string of the plot title
#' @param drop_zeros a logical, whether to drop zeros from the 2D density plot
#' dataframe when both the benchmark and meta/WRI CHM are zero. default is TRUE
#' @return a ggplot object
#' @export
comparison_plot <- function(
    r1, r2,
    fill_trans = c("sqrt", "identity", "log", "log10"),
    title = "",
    drop_zeros = TRUE) {
  chml_assert_class(r1, "SpatRaster")
  if (missing(r2)) {
    chml_opt <- getOption("chmloader.out_raster_type")
    on.exit(chml_set_options(out_raster_type = chml_opt))
    chml_set_options(out_raster_type = "SpatRaster")
    cli::cli_alert_info("meta/WRI CHM not provided, downloading now...")
    r2 <- download_chm(r1, filename = tempfile(fileext = ".tif"))
    cli::cli_alert_success("CHM downloaded successfully!")
  }
  chml_assert_class(r2, "SpatRaster")
  chml_assert_class(title, "character")
  fill_trans <- rlang::arg_match(fill_trans)
  comp_ras_df <- two_ras_df(r1, r2)
  gmap <- two_ras_map(comp_ras_df)
  valplot <- density_2d(comp_ras_df, fill_trans)
  geo_attrs <- suppressMessages(get_biome_n_cont(r1))
  ras_res <- terra::res(r2)
  crs_units <- sf::st_crs(r2)$units
  ras_units <- ifelse(is.null(crs_units), "degrees", crs_units)
  stack_plots(gmap, valplot, geo_attrs, ras_res, ras_units, title)
}

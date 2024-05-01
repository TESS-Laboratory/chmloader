#' create a dataframe of two rasters
#' @param r1 a SpatRaster object
#' @param r2 a SpatRaster object
#' @return a dataframe of the two rasters
#' @noRd
#' @keywords internal
two_ras_df <- function(r1, r2) {
  names(r1) <- "benchmark_chm"
  names(r2) <- "meta_chm"

  purrr::map2(
    list(r1, r2), c(TRUE, FALSE),
    ~ as.data.frame(.x, xy = .y, cells = TRUE)
  ) |>
    purrr::reduce(~ dplyr::left_join(.x, .y, by = dplyr::join_by(cell)))
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
    n,
    dz) {
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

  if (dz) {
    df <- df |>
      dplyr::filter(!(benchmark_chm == 0 & meta_chm == 0))
  }

  em <- error_mets(df$benchmark_chm, df$meta_chm)

  footnote <- bquote(
    ~ R^2:~ .(em["rsq"]) ~ "| "
    ~ RMSE:~ .(em["rmse"]) ~ "| "
    ~ MAE:~ .(em["mae"])
  )
  if (dz) {
    footnote <- bquote(.(footnote) ~ "| "
    ~ "* values that are zero in both datasets are omitted")
  }

  ggplot2::ggplot(df) +
    ggplot2::aes(x = benchmark_chm, y = meta_chm) +
    ggplot2::stat_density2d(
      ggplot2::aes(fill = trans_func(ggplot2::after_stat(density))),
      geom = "raster", contour = FALSE, n = n, alpha = 0.7
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
      caption = footnote,
      x = "Benchmark Canopy Height (m)",
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
  sub_t <- glue::glue(
    "Continent: {g_att['cont_name']}
    Biome: {g_att['biome_name']}
    Cell size: {rr[1]} x {rr[2]} {r_units}"
  )

  p1 / p2 + patchwork::plot_annotation(
    title = title,
    subtitle = sub_t
  )
}

#' get the missing meta/WRI CHM
#' @param r a SpatRaster object
#' @return a SpatRaster object
#' @noRd
#' @keywords internal
get_missing_r2 <- function(r) {
  chml_opt <- getOption("chmloader.out_raster_type")
  on.exit(chml_set_options(out_raster_type = chml_opt))
  chml_set_options(out_raster_type = "SpatRaster")
  cli::cli_alert_info("meta/WRI CHM not provided, downloading now...")
  r2 <- download_chm(r, filename = tempfile(fileext = ".tif"))
  cli::cli_alert_success("CHM downloaded successfully!")
  return(r2)
}

plot_wrapper <- function(r1, r2, fill_trans, title, dz, n) {
  comp_ras_df <- two_ras_df(r1, r2)
  gmap <- two_ras_map(comp_ras_df)
  valplot <- density_2d(comp_ras_df, fill_trans, n, dz)
  geo_attrs <- suppressMessages(get_biome_n_cont(r1))
  ras_res <- round(terra::res(r2), 6)
  crs_units <- sf::st_crs(r2)$units
  ras_units <- ifelse(is.null(crs_units), "degrees", crs_units)
  stack_plots(gmap, valplot, geo_attrs, ras_res, ras_units, title)
}


#' Standardised plot to compare two Canopy Height Models
#' Create a multi-panel plot showing the benchmark raster alongside the
#' meta/WRI raster, and a 2D density plot of the correspondence between the two
#' rasters.
#' @param r1 a SpatRaster object, The benchmark raster (user provided)
#' @param r2 a SpatRaster object, The meta/WRI raster. If this is not provided,
#' the function will download the missing raster for the area coincident with
#' `r1`.
#' @param fill_trans a character string of the transformation to apply to the
#' density colour gradient.
#' @param title a character string of the plot title
#' @param drop_zeros a logical, whether to drop zeros from the 2D density plot
#' dataframe and comparative statistics. If TRUE, when both the benchmark and
#' meta/WRI CHM are zero, these rows are removed. useful when there is a lot
#' of bare ground. default is TRUE
#' @param aggregate a numeric vector of the factors to aggregate the rasters by.
#' default is NULL in which case the CHMs are compared only at the native
#' resolution of `r1`. the aggregate factor is relative to the native resolution
#' of the rasters. see `terra::aggregate` for more details. Multiple numeric
#' values can be provided to compare the rasters at many different resolutions.
#' @param n_2d an integer, Number of grid points in each direction for the 2D
#' density. default is 100. see `ggplot::stat_density2d` for more details.
#' @return a ggplot object if `aggregate` is NULL, otherwise a list of ggplot
#' objects.
#' @details This function provides a standardised way to evaluate the accuracy
#' of two canopy height models (or indeed any two rasters with equal
#' dimensions).
#'
#' The new Tolan, et al. (2024) global CHM presents a step change
#' in forest mapping. With this change comes some potential issues and varying
#' performance, but given how new these data are and the global extent, it
#' is challeging to develop a comprehensive evaluation. This
#' function offers a means by which to quickly and easily compare these data
#' with existing CHMs, derived either from LiDAR or through Machine Learning
#' workflows.
#'
#' We hope that this function can help us collectively learn more about the
#' relative merits of this dataset and how we may use it and the underlying
#' models (see https://github.com/facebookresearch/HighResCanopyHeight) to
#' improve our understanding of forest structure and function.
#'
#' The plot generates some general location information for the target area
#' including the continent and biome. Further information can be added to the
#' `title` argument if desired. We hope this reduces any barriers to sharing
#' the results.
#'
#' @examplesIf interactive()
#'
#' @export
compare_models <- function(
    r1, r2,
    fill_trans = c("sqrt", "identity", "log", "log10"),
    title = "",
    aggregate = NULL,
    drop_zeros = FALSE,
    n_2d = 100) {
  chml_assert_class(r1, "SpatRaster")

  if (missing(r2)) {
    r2 <- get_missing_r2(r1)
  }

  chml_assert_class(r2, "SpatRaster")
  chml_assert_class(title, "character")
  chml_assert_class(drop_zeros, "logical")
  chml_assert_class(aggregate, c("numeric", "NULL"))
  chml_assert_class(n_2d, "numeric")
  fill_trans <- rlang::arg_match(fill_trans)

  ph <- plot_wrapper(r1, r2, fill_trans, title, drop_zeros, n_2d)

  if (!is.null(aggregate)) {
    pl <- purrr::map(aggregate, function(.x) {
      rr1 <- terra::aggregate(r1, fact = .x)
      rr2 <- terra::aggregate(r2, fact = .x)
      plot_wrapper(rr1, rr2, fill_trans, title, drop_zeros, n_2d)
    })
    return(c(list(ph), pl))
  } else {
    return(ph)
  }
}

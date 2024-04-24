test_that("download_chm works", {
  gabon <- sf::st_point(c(9.5, -0.33)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_buffer(200) |>
    sf::st_transform(3857)

  gabon_chm <- download_chm(
    gabon,
    filename = tempfile(fileext = ".tif")
  )
  expect_type(gabon_chm, "character")

  gabon_chm <- terra::rast(gabon_chm)
  mm <- terra::minmax(gabon_chm, compute = TRUE)
  expect_equal(mm[1], 0)
  expect_equal(mm[2], 29)
})

test_that("download_chm warning on lonlat", {
  gabon <- sf::st_point(c(9.5, -0.33)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_buffer(200)

  expect_warning(download_chm(gabon, filename = tempfile(fileext = ".tif")))
})

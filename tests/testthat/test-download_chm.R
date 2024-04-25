test_that("download_chm works", {
  skip_on_cran()
  skip_if_offline()

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
  skip_on_cran()
  skip_if_offline()
  gabon <- sf::st_point(c(9.5, -0.33)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_buffer(50)

  expect_warning(download_chm(gabon, filename = tempfile(fileext = ".tif")))
})

test_that("cogs are deleted", {
  skip_on_cran()
  skip_if_offline()
  gabon <- sf::st_point(c(9.5, -0.33)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_buffer(50) |>
    sf::st_transform(3857)

  filename <- tempfile(fileext = ".tif")

  fp <- download_chm(
    gabon,
    filename = filename
  )

  testthat::expect_warning({
    fp <- download_chm(
      gabon,
      filename = filename
    )
  })
})


test_that("error wrong class", {
  testthat::expect_error(
    download_chm(2)
  )
})


test_that("download_chm with raster works", {
  skip_on_cran()
  skip_if_offline()

  # Create a SpatRaster
  gabon <- sf::st_point(c(9.5, -0.33)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_buffer(200) |>
    sf::st_transform(3857) |>
    terra::vect() |>
    terra::rast(res = 10.3)

  x1 <- download_chm(gabon, filename = tempfile(fileext = ".tif")) |>
    terra::rast()
  expect_equal(terra::res(x1)[1], 10.3)

  x2 <- download_chm(gabon, res = 1, filename = tempfile(fileext = ".tif")) |>
    terra::rast()

  expect_equal(terra::res(x2)[1], 1)
})

test_that("build_chm_srcs works", {
  eg <- sf::st_point(c(9.842141729029507, 0.00036618276893235106)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_transform(3857)

  eg500 <- sf::st_buffer(eg, 500)
  eg50 <- sf::st_buffer(eg, 50)

  expect_equal(length(build_chm_srcs(eg500)), 4)
  expect_equal(length(build_chm_srcs(eg50)), 2)
})

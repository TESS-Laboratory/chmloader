test_that("get_ext works", {
  vf <- system.file("ex/lux.shp", package = "terra")
  vr <- system.file("ex/elev.tif", package = "terra")
  vf_sf <- sf::read_sf(vf)

  expect_true(all.equal(get_ext(terra::vect(vf)), get_ext(vf_sf)))
  expect_true(all.equal(
    get_ext(terra::vect(vf)),
    get_ext(sf::st_geometry(vf_sf))
  ))

  expected_box <- c(5.74, 49.44, 6.53, 50.19)
  names(expected_box) <- c("xmin", "ymin", "xmax", "ymax")
  expect_equal(get_ext(terra::rast(vr)), expected_box, tolerance = 1e-2)
})

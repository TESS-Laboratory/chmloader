test_that("get_proj works", {
  vf <- system.file("ex/lux.shp", package = "terra")
  vr <- system.file("ex/elev.tif", package = "terra")
  expect_gt(nchar(get_proj(terra::rast(vr))), 1)
  expect_gt(nchar(get_proj(terra::vect(vf))), 1)

  vf_sf <- sf::read_sf(vf)
  vf_sfc <- sf::st_geometry(vf_sf)
  expect_gt(nchar(get_proj(vf_sf)), 1)
  expect_gt(nchar(get_proj(vf_sfc)), 1)
})

test_that("get_spat_corners works", {
  vf <- system.file("ex/lux.shp", package = "terra")

  expect_equal(get_spat_corners(sf::read_sf(vf)),
    get_spat_corners(terra::vect(vf)),
    tolerance = 1e-2
  )

  vr <- system.file("ex/elev.tif", package = "terra")
  df <- get_spat_corners(terra::rast(vr))
  target_sums <- c(30.29167, 248.70833)
  names(target_sums) <- c("X", "Y")
  expect_equal(colSums(df), target_sums, tolerance = 1e-2)
})

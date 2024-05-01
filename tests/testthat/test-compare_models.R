test_that("compare_models works", {
  wist_wood <- reference_data("wistmans_wood")
  expect_s4_class(wist_wood, "SpatRaster")
  ww_l <- compare_models(wist_wood, aggregate = 20, drop_zeros = TRUE)

  expect_s3_class(ww_l[[1]], "patchwork")
  expect_s3_class(ww_l[[2]], "patchwork")

  expect_identical(ww_l[[1]]$labels$x, "Benchmark Canopy Height (m)")
  expect_identical(ww_l[[1]]$labels$y, "Meta/WRI Canopy Height (m)")
})

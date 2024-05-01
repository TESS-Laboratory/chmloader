test_that("reference_data works", {
  wist_wood <- reference_data("wistmans_wood")
  expect_s4_class(wist_wood, "SpatRaster")
  fingle_woods <- reference_data("fingle_woods")
  expect_s4_class(fingle_woods, "SpatRaster")
  new_forest <- reference_data("new_forest")
  expect_s4_class(new_forest, "SpatRaster")
})

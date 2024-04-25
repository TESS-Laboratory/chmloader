test_that("round_bbox works", {
  box <- c(1.1, 2.2, 3.3, 4.4)
  rbox <- round_bbox(box, 2)
  expect_equal(rbox, c(0, 2, 4, 6))

  expect_lt(rbox[1], box[1])
  expect_lt(rbox[2], box[2])
  expect_gt(rbox[3], box[3])
  expect_gt(rbox[4], box[4])
})

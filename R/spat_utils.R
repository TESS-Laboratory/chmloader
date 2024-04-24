#' Round to nearest value
#'
#' A literal copy of `plyr::round_any()`
#'
#' @param x numeric
#' @param accuracy numeric. Target multiple to round to.
#' @param f function. Default is `round`
#'
#' @return numeric
#' @noRd
#' @keywords internal
round_nearest <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' round a bbox to desired interval
#'
#' @param .box numeric. Bounding box.
#' @param .res  numeric. Desired resolution.
#'
#' @return vector with bbox dims
#' @noRd
#' @keywords internal
round_bbox <- function(.box, .res) {
  big <- round_nearest(.box[c(2, 4)], .res, f = ceiling)
  small <- round_nearest(.box[c(1, 3)], .res, f = floor)
  c(small[1], big[1], small[2], big[2])
}

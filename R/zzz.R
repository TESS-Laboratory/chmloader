.onLoad <- function(lib, pkg) {
  op <- options()
  op.chmloader <- list(
    chmloader.out_raster_type = "SpatRaster"
  )
  toset <- !(names(op.chmloader) %in% names(op))
  if (any(toset)) options(op.chmloader[toset])

  invisible()
}

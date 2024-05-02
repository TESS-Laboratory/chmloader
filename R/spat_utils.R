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
  big <- round_nearest(.box[c(3, 4)], .res, f = ceiling)
  small <- round_nearest(.box[c(1, 2)], .res, f = floor)
  c(small, big)
}


#' Get Spatial Projection
#'
#' A class agnostic function to return the projection of a spatial object or
#' source. returned projection uses wkt format.
#'
#' @param x A spatial object, file path or source
#' @return A character - WKT projection string.
#'
#' @noRd
#' @keywords internalq
get_proj <- function(x) {
  UseMethod("get_proj")
}


#'
#' @export
get_proj.SpatRaster <- function(x) {
  terra::crs(x)
}


#' @export
get_proj.SpatVector <- function(x) {
  terra::crs(x)
}


#' @export
get_proj.sf <- function(x) {
  sf::st_crs(x)$wkt
}


#' @export
get_proj.sfc <- function(x) {
  sf::st_crs(x)$wkt
}



#' Get spatial extent
#'
#' A class agnostic function to return the bounding extent (i.e. bounding box)
#' of a spatial object.
#'
#' @param x A spatial object, file path or source
#' @return A numeric vector of length 4. Values are returned as:
#' "xmin", "xmax", "ymin", "ymax"
#' @noRd
#' @keywords internal
get_ext <- function(x) {
  UseMethod("get_ext")
}

#' @export
get_ext.SpatRaster <- function(x) {
  as.vector(terra::ext(x))[c(1, 3, 2, 4)]
}

#' @export
get_ext.SpatVector <- function(x) {
  as.vector(terra::ext(x))[c(1, 3, 2, 4)]
}

#' @export
get_ext.sf <- function(x) {
  sf_ext_method(x)
}

#' @export
get_ext.sfc <- function(x) {
  sf_ext_method(x)
}

sf_ext_method <- function(x) {
  x <- as.numeric(sf::st_bbox(x))
  names(x) <- c("xmin", "ymin", "xmax", "ymax")
  x
}

#' reproject to EPSG:3857 (Web Mercator)
#' @param x A spatial object
#' @return A spatial object in EPSG:3857
#' @noRd
#' @keywords internal
proj_to_web_merc <- function(x) {
  UseMethod("proj_to_web_merc")
}

#' @export
proj_to_web_merc.SpatRaster <- function(x) {
  terra::project(x, "EPSG:3857")
}

#' @export
proj_to_web_merc.SpatVector <- function(x) {
  terra::project(x, "EPSG:3857")
}

#' @export
proj_to_web_merc.sf <- function(x) {
  sf::st_transform(x, 3857)
}

#' @export
proj_to_web_merc.sfc <- function(x) {
  sf::st_transform(x, 3857)
}

#' get the dimensions of a bounding box
#' @param bbox numeric. A bounding box.
#' @param res numeric. The resolution of the bounding box.
#' @param dim character. The dimension to return.
#' @return numeric. The dimension of the bounding box.
#' @noRd
#' @keywords internal
box_dim <- function(bbox, res, dim = c("x", "y")) {
  dim <- rlang::arg_match(dim)
  res <- if (length(res) == 1) rep(res, 2) else res
  switch(dim,
    x = (bbox[[3]] - bbox[[1]]) / res[[1]],
    y = (bbox[[4]] - bbox[[2]]) / res[[2]]
  )
}

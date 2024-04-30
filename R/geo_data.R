#' Load a spatial vector of contentinents
#' Load a spatial vector of continents as an sf object
#' @return an sf object of continents
continents <- function() {
  sf::read_sf(
    system.file("geojson/ne_continents.geojson", package = "chmloader")
  )
}

#' Load a spatial vector of eco-regions
#' Load a spatial vector of eco-regions as an sf object
#' @return an sf object of eco-regions
biomes <- function() {
  sf::read_sf(
    system.file("geojson/eco_biomes.geojson", package = "chmloader")
  )
}

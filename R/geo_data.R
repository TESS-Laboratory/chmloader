#' Load a spatial vector of contentinents
#' Load a spatial vector of continents as an sf object
#' @return an sf object of continents
continents <- function() {
  sf::read_sf(
    system.file("continents/ne_continents.fgb", package = "chmloader")
  )
}

#' Load a spatial vector of eco-regions
#' Load a spatial vector of eco-regions as an sf object
#' @return an sf object of eco-regions
biomes <- function() {
  sf::read_sf(
    system.file("biomes/eco_biomes.fgb", package = "chmloader")
  )
}

#' Load example LiDAR-based CHM data.
#' function to access a set of example CHM data for demonstration and comparison
#' with the Tolan, et. al. (2024) CHM data.
#' @param name character, the name of the example CHM data to load
#' @return a SpatRaster object
#' @details At present these examples include data from the Environment Agency's
#' Vegetation Object Model (VOM). See https://www.data.gov.uk/dataset/227ab487-e8f2-4cbb-b26a-9e6d3b662265/lidar-vegetation-object-model-vom
#' for more information. If you want to explore these data further in R, check
#' out the gblidar package at: https://github.com/h-a-graham/gblidar
#' @examplesIf interactive()
#' wistmans_wood <- load_chm_data("wistmans_wood")
#' @export
reference_data <- function(name = c("wistmans_wood", "fingle_woods", "new_forest")) {
  name <- rlang::arg_match(name)
  pth <- switch(name,
    wistmans_wood = system.file("wistmans_woods/wistmans_wood_vom.tif", package = "chmloader"),
    fingle_woods = system.file("fingle_woods/fingle_woods_vom.tif", package = "chmloader"),
    new_forest = system.file("new_forest/new_forest_vom.tif", package = "chmloader")
  )

  terra::rast(pth)
}

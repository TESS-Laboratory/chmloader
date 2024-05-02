## code to prepare `tiles` dataset goes here

library(sf)

if (!dir.exists("inst/tiles")) {
  dir.create("inst/tiles", recursive = TRUE)
}

Sys.setenv("AWS_NO_SIGN_REQUEST" = "YES")
x <- sf::read_sf("/vsis3/dataforgood-fb-data/forests/v1/alsgedi_global_v6_float/tiles.geojson")

write_sf(x, "inst/tiles/tiles.fgb")

zip("inst/tiles/tiles.zip", "inst/tiles/tiles.fgb")

if (file.exists("inst/tiles/tiles.fgb")) {
  file.remove("inst/tiles/tiles.fgb")
}

x

read_sf("/vsizip/inst/tiles/tiles.zip")

usethis::use_data(tiles, internal = TRUE, overwrite = TRUE)

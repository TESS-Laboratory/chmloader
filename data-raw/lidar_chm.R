## code to prepare `lidar_chm` dataset goes here
# get the vegetation object models; see https://www.data.gov.uk/dataset/227ab487-e8f2-4cbb-b26a-9e6d3b662265/lidar-vegetation-object-model-vom
library(sf)
library(gblidar) # https://github.com/h-a-graham/gblidar

if (!file.exists("inst/fingle_woods")) dir.create("inst/fingle_woods", recursive = TRUE)
if (!file.exists("inst/wistmans_woods")) dir.create("inst/wistmans_woods", recursive = TRUE)
if (!file.exists("inst/new_forest")) dir.create("inst/new_forest", recursive = TRUE)

fingle_woods <- st_point(c(-3.7810, 50.6954)) |>
  st_sfc() |>
  st_set_crs(4326) |>
  st_buffer(300)

fw_chm <- eng_composite(fingle_woods,
  product = "vom",
  destination = "inst/fingle_woods/fingle_woods_vom.tif", warp_res = 1
)

wistmans_wood <- st_point(c(-3.9613, 50.5776)) |>
  st_sfc() |>
  st_set_crs(4326) |>
  st_buffer(350)

ww_chm <- eng_composite(wistmans_wood,
  product = "vom", warp_res = 1
) |>
  terra::rast() |>
  terra::project("EPSG:4326",
    filename = "inst/wistmans_woods/wistmans_wood_vom.tif", overwrite = TRUE
  )


new_forest <- st_point(c(425913, 108726)) |>
  st_buffer(300) |>
  st_sfc() |>
  st_set_crs(27700)

nf_chm <- eng_composite(new_forest,
  product = "vom",
  destination = "inst/new_forest/new_forest_vom.tif", warp_res = 1
)


<!-- README.md is generated from README.Rmd. Please edit that file -->

# chmloader

<!-- badges: start -->

<!-- badges: end -->

The goal of chmloader is to download the Canopy Height Model (CHM) data
from this recent work by [Tolan et
al. (2023)](https://research.facebook.com/blog/2023/4/every-tree-counts-large-scale-mapping-of-canopy-height-at-the-resolution-of-individual-trees/)
The data is downloaded from aws s3 storage - further details on the
bucket can be found
[here](https://registry.opendata.aws/dataforgood-fb-forests/)

## Installation

You can install the development version of chmloader like so:

``` r
pak::pkg_install("chmloader")
```

## Example

This is a basic example which shows you how to download some data:

``` r
library(chmloader)

sundarbans <- sf::st_point(c(89.2, 22.0)) |>
  sf::st_sfc(crs = 4326) |>
  sf::st_buffer(5000)

st <- Sys.time()
sundarbans_chm <- download_chm(sundarbans)
#> Warning: target is in longlat, transforming to EPSG:3857
round(difftime(Sys.time(), st, units = "secs"))
#> Time difference of 23 secs

sdb_chm <- terra::rast(sundarbans_chm)
sdb_chm[sdb_chm < 1] <- NA
terra::plot(sdb_chm,
  col = hcl.colors(256, "viridis")
)
```

<img src="man/figures/README-example-1.png" width="100%" />

This package has only the one function `download_chm` which downloads
the CHM data from the aws s3 storage. The function takes a `sf` object
as input and returns a character vector for the output raster file path.
The download uses gdalwarp (via `sf::gdal_utils`) to efficiently
retrieve only the required data from across multiple tiles - the default
resolution is 1 m but this can be reprojected on the fly as needed.

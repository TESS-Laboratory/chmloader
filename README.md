
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chmloader

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Permian-Global-Research/chmloader/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Permian-Global-Research/chmloader/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of chmloader is to download the Canopy Height Model (CHM) data
from this recent work by [Tolan et
al. (2024)](https://www.sciencedirect.com/science/article/pii/S003442572300439X).
A high level summary of this work can be found
[here](https://sustainability.fb.com/blog/2024/04/22/using-artificial-intelligence-to-map-the-earths-forests/).
The data is downloaded from aws s3 storage - further details on the
bucket can be found
[here](https://registry.opendata.aws/dataforgood-fb-forests/)

## Installation

You can install the development version of chmloader like so:

``` r
# install.packages("pak")
pak::pkg_install("Permian-Global-Research/chmloader")
```

## Example

This is a basic example which shows you how to download some data. The
`download_chm` function uses
[gdalwarp](https://gdal.org/programs/gdalwarp.html) (via
[`sf::gdal_utils`](https://r-spatial.github.io/sf/reference/gdal_utils.html))
to efficiently retrieve only the required data from across multiple
tiles - the default resolution is 1 m but this can be reprojected as
needed using the `res` argument.

``` r
library(chmloader)

sundarbans <- sf::st_point(c(89.2, 22.0)) |>
  sf::st_sfc(crs = 4326) |>
  sf::st_buffer(1000)

sundarbans_chm <- download_chm(
  sundarbans,
  filename = tempfile(fileext = ".tif")
)
terra::plot(sundarbans_chm, col = hcl.colors(256, "viridis"))
```

<img src="man/figures/README-example-1.png" width="100%" />

This package also provides a simple function to create a plots for
comparing different CHMs. The intention of this function is to enable
robust a simple evaluation of the Tolan et al. (2024) CHM data with
LiDAR-based models and other ML-derived products. The chmloader package
comes with a small set of LiDAR-based CHM example dataset, derived from
the English Environment Agency’s [Vegetation Object Model
dataset](https://www.data.gov.uk/dataset/227ab487-e8f2-4cbb-b26a-9e6d3b662265/lidar-vegetation-object-model-vom)
Below is an example using one of these excample datasets from Fingle
Woods, Devon, UK:

``` r
fingle_woods <- reference_data("fingle_woods")

compare_models(fingle_woods, aggregate = 10, drop_zeros = TRUE)
#> ℹ meta/WRI CHM not provided, downloading now...
#> ✔ CHM downloaded successfully!
```

<img src="man/figures/README-example2-1.png" width="100%" /><img src="man/figures/README-example2-2.png" width="100%" />

Note in this example, the `aggregate` argument is used to reduce the
resolution of both the refernce and Meta/WRI CHM by a factor of 10
(resulting in a 10 m model) and test both this coarser scale model in
addition to the original 1 m model. This functionality may help to
reveal what the true resolution of the Meta/WRI CHM is, and how it
compares to the LiDAR-based model across scales.

Also, the `drop_zeros` argument is used to remove zero values from the
both the 2d density plot and the derived statistics, where values from
the reference/benchmark data and the Meta/WRI CHM are both zero. This is
particularly useful where the main interest is to evaluate the tree
canopy rather than the absence of trees and/or where tree cover is
sparse.

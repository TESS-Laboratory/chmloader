#' @title chmloader: A package for downloading and evaluating Canopy Height Models
#'
#' @description The chmloader package allows users to download the 1 m
#' resolution Canopy Height Model (CHM) data from the recent Meta/WRI dataset by
#' Tolan et al. (2023). For more information on this dataset see:
#' https://www.sciencedirect.com/science/article/pii/S003442572300439X and
#' https://sustainability.fb.com/blog/2024/04/22/using-artificial-intelligence-to-map-the-earths-forests/ .
#'
#' @keywords internal
#' @section Downloading data:
#' \describe{
#'   \item{\code{\link{download_chm}}}{Download Canopy Height Model (CHM) data Downloads CHM data from the Tolan et al. (2023) dataset by meta and WRI.}
#' }
#' @section Compare Canopy height Models:
#' \describe{
#'  \item{\code{\link{compare_models}}}{Create a multi-panel plot showing the benchmark raster alongside the meta/WRI raster, and a 2D density plot of the correspondence between the two rasters.}
#' }
#'
#' @section Reference/Evaluation Data:
#' \describe{
#' \item{\code{\link{reference_data}}}{Access a set of example CHM data for demonstration and comparison with the Tolan, et. al. (2024) CHM data.}
#' }
#'
#' @section Control chmloader options:
#' \describe{
#' \item{\code{\link{chml_set_options}}}{Helper function to safely set chmloader options such as the output raster type}
#' }

"_PACKAGE"

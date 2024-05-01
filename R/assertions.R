#' check the class of an object
#' @param x the object to check the class of
#' @param classes the classes to check against
#' @noRd
#' @keywords internal
chml_assert_class <- function(x, classes) {
  if (!any(class(x) %in% classes)) {
    obj_name <- deparse(substitute(x))
    classes_str <- glue::glue(
      glue::glue_collapse(classes[-length(classes)], sep = c(", ")),
      " or ", classes[length(classes)]
    )
    cli::cli_abort(
      c("x" = "{.code {obj_name}} must be an object of class
          {.emph {.field {classes_str}}} class{?es}, not {.emph {.type {x}}}")
    )
  }
}

#' check if a COG file exists and delete it if it does
#' @param fname character, the filename to check
#' @param gdalopts character, the gdal options to check
#' @noRd
#' @keywords internal
check_existing_cog <- function(fname, gdalopts) {
  if (!file.exists(fname)) {
    return(invisible())
  }

  if (any(gdalopts[which(gdalopts == "-of") + 1] == "COG")) {
    cli::cli_warn(
      c(
        "!" = "Cloud Optimized Geotiff file exists already",
        "i" = "deleting file before writiting to maintain optimizations"
      ),
    )
    file.remove(fname)
  }
  invisible()
}

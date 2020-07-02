#' colorDF – colorful data frames in your terminal
#'
#' colorDF – colorful data frames in your terminal
#'
#' colorDF allows you to view data frames using the color and styling
#' capabilities of an ANSI terminal: 216 colors! 16 base colors! 24 shades of
#' gray! Italic, bold, inverse *and* underline! Well, OK, this may not seem much, but in
#' fact it allows at highlighting, showing different column types or coloring significant
#' p-values in red. Trust me, it is useful if you work a lot with huge data
#' frames.
#'
#' colorDF does not really introduce a new type of a data frame; when
#' applied to tibbles, data frames or data tables it does not change their
#' behavior except for the print method. In other words, it is only a
#' visualization layer on top of a data frame like object, which will
#' otherwise work exactly as expected.
#'
#' To get started, continue with the [colorDF()] help page.
#'
#' @section Known issues:
#'  * In Rstudio, inverse does not work correctly (known bug in Rstudio 1.3)
#'  * colorDF relies on the [crayon][crayon::crayon] package. In certain
#'    instances, the crayon package will enforce only 8 basic colors even if more
#'    colors can be displayed. See the vignette of colorDF for an example and on
#'    how to deal with this issue.
#' 
#' @seealso [colorDF()] on creating colorful data frames;
#'          [df_style()] on how to modify style of the colorful data frame; 
#'          [col_type()] on how to change column types;
#'          [colorDF_themes()] to list all themes; [colorDF_themes_show()]
#'          to view all themes.
#' @name colorDF-package
NULL

## environment holding some global configuration options
.colorDF_DataEnv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  num_colors(TRUE)
  if(!is.null(noitalic <- getOption("colorDF_noitalic")) && noitalic) {
    .colorDF_DataEnv[["noitalic"]] <- TRUE
  } else {
    .colorDF_DataEnv[["noitalic"]] <- FALSE
  }
}

.onAttach <- function(libname, pkgname) {
  if(is.null(getOption("colorDF_theme"))) {
    options(colorDF_theme="light")
  }

  num_colors(TRUE)
  packageStartupMessage(sprintf(  
    "%s: for best results, use terminals which support 256 colors.",
    pkgname
    ))
}

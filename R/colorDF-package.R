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
#' @seealso [colorDF()] on creating colorful data frames; [global options][colorDF-global-options] for colorDF;
#'          [df_style()] on how to modify style of the colorful data frame; 
#'          [col_type()] on how to change column types;
#'          [colorDF_themes()] to list all themes; [colorDF_themes_show()]
#'          to view all themes.
#' @name colorDF-package
NULL

#' Global options for colorDF
#' 
#' The behavior of colorful data frames can be influenced by a number of
#' global options set with [options()]. All options and their defaults can be viewed with
#' `colorDF_options()`.
#'
#' The following global options are interpreted by functions in the colorDF
#' package:
#'
#' 
#' * `colorDF_n` (default: `20`): how many rows at maximum are printed (set to
#'   `Inf` to always show all rows).
#' * `colorDF_theme` (default: `"light"`): theme assigned by default to the new
#'   objects by [colorDF()] (and also when passing a data frame directly to
#'   [summary_colorDF()]).
#' * `colorDF_tibble_style` (default: `FALSE`): if `TRUE`, then only column
#'   will be shown which fit on the screen (much like in the default print method
#'   for [tibbles][tibble::tibble()].
#' * `colorDF_noitalic` (default: `FALSE`): some terminals do not support
#'   italics and instead use video inverse. This will make some styles look
#'   really weird. If this option is set to `TRUE` at time that the colorDF
#'   package is loaded, then the italic style will be silently ignored.
#'   Changing this option will have no effect when set after the package is loaded, 
#'   so best put it in your `.Rprofile`.
#' * `colorDF_sep`: separator for the table columns
#' * `width`: width of the terminal in characters
#' @seealso [colorDF()] on creating colorful data frames; 
#'          [df_style()] on how to modify style of the colorful data frame; 
#'          [colorDF_themes()] to list all themes; [colorDF_themes_show()]
#'          to view all themes.
#' @examples
#' ## use the dark theme for a terminal with dark background
#' options(colorDF_theme="dark")
#' colorDF(mtcars)
#' @name colorDF-global-options
NULL

#' @rdname colorDF-global-options
#' @importFrom utils read.table
#' @export
colorDF_options <- function() {

  df <- read.table(text=
'Option\tDescription\tDefault
colorDF_n\tNumber of rows to show\t20
colorDF_noitalic\tSuppress italics (read on loading)\tFALSE
colorDF_tibble_style\tBehave like tibble\tFALSE
colorDF_sep\tColumn separator\t" "
colorDF_theme\tcolorDF theme to use\tlight
width\tWidth of the terminal\tgetOption("width")
', sep="\t", header=TRUE, quote="", stringsAsFactors=FALSE)

  curr <- sapply(df$Option, function(x) {
    .cur <- getOption(x)
    if(is.null(.cur)) .cur <- "Not set"
    if(.cur == " ") {
      .cur <- '" "'
    }
    .cur})

  df$Current <- curr
  print_colorDF(df)
  return(invisible(df))
}



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





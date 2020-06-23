#' colorDF – colorful data frames in your terminal
#'
#' colorDF – colorful data frames in your terminal
#'
#' colorDF allows you to view data frames using the color and styling
#' capabilities of an ANSI terminal: 216 colors! 16 base colors! 24 shades of
#' gray! Italic, bold, inverse *and* underline! Well, this may not seem much, but in
#' fact it allows at least some basic highlighting or coloring significant
#' p-values in red. Trust me, it is useful if you work a lot with huge data
#' frames.
#' @name colorDF-package
NULL

.onAttach <- function(libname, pkgname) {
  options(colorDF_theme="light")
  packageStartupMessage(sprintf(  
    "%s: for best results, use terminals which support 255 colors.\n",
    pkgname
    ))
}

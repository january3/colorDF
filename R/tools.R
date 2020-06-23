#' Highlight some rows in a data frame
#'
#' Highlight some rows in a data frame
#'
#' Uses [print.colorDF()] to highlight selected rows in a data frame.
#' 
#' @param x data frame like object
#' @param sel logical vector of length equal to number of rows in the data frame.
#' @examples
#' highlight(mtcars, mtcars$cyl == 6)
#' @export
highlight <- function(x, sel) {
  print.colorDF(x, highlight=sel)
}

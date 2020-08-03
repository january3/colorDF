#' Make a dataframe colorful
#'
#' Make a dataframe colorful
#'
#' These functions turn any data frame like object (i.e. object which 
#' inherits the data.frame class, such as a [tibble][tibble::tibble()] or 
#' a [data table][data.table::data.table()]). 
#'
#' Apart from adding the S3 class "colorDF", the `.style` attribute (and
#' later the `.coltp` attribute), the only thing that really changes is
#' the print method (see [print_colorDF()]). In other words, the behavior
#' of the object does not change (e.g., a [base::data.frame()] will by the default
#' drop dimensions if one column is selected, while a [tibble::tibble()] will
#' not). colorDF is just for visualization, never truly manipulation.
#'
#' Several color themes come with the package; see [colorDF_themes_show()]. 
#' When creating a colorful data frame, a theme might be directly selected;
#' otherwise the `getOption("colorDF_theme")` is consulted and if NULL, a
#' default theme will be selected. The theme associated with an object
#' becomes a style and can be further manipulated (see [df_style()]).
#'
#' [as.colorDF()] calls [colorDF()]; this function is only here for
#' completeness.
#' @param x a data frame or similar object, e.g. tibble, data.table or any
#'        object for which as.data.frame call returns a data frame
#' @param theme Which theme to use
#' @param ... further arguments are passed to [colorDF()].
#' @return a colorful data frame – identical object but with the `.style`
#'         attribute set and class "colorDF" added.
#' @seealso [Introduction to the package][colorDF-package];
#'          [df_style()] on how to modify style of the colorful data frame; 
#'          [col_type()] on how to change column types;
#'          [colorDF_themes()] to list all themes; [colorDF_themes_show()]
#'          to view all themes.
#' @examples
#' colorDF(mtcars)
#' colorDF(mtcars, theme="bw")
#' @export
colorDF <- function(x, theme=NULL) {

  err <- try(as.data.frame(x), silent=TRUE)

  if(inherits(err, "try-error")) {
    stop("colorDF: x does not seem to be a data frame-like object")
  }

  class(x) <- c("colorDF", class(x))
  x <- .set_style(x, .get_style(x, theme=theme))

  x
}


#' @rdname colorDF
#' @export
as.colorDF <- function(x, ...) {
  colorDF(x, ...)
}



## we need to wrap around the subset because 
## otherwise the default functions loose the attributes .coltp and .style
`[.colorDF` <- function(x, ...) {

  xcl <- class(x)
  class(x) <- setdiff(xcl, "colorDF")
  coltp <- attr(x, ".coltp")
  style <- attr(x, ".style")
  ret <- x[ ... ]
  attr(ret, ".coltp") <- coltp
  attr(ret, ".style") <- style
  class(ret) <- xcl
  ret

}

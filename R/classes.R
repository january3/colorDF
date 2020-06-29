#' Make a dataframe colorful
#'
#' Make a dataframe colorful
#' @param x a data frame or similar object (e.g. tibble)
#' @param theme Which theme to use
#' @seealso [colorDF_themes()] to list all themes; [colorDF_themes_show()]
#'          to view all themes.
#' @return a colorful data frame – identical object but with the `.style`
#'         attribute set.
#' @seealso [df_style()] on how to modify style of the colorful data frame
#' @export
colorDF <- function(x, theme=NULL) {

  x <- try(as.data.frame(x), silent=TRUE)

  if(inherits(x, "try-error")) {
    stop("colorDF: x does not seem to be a data frame-like object")
  }

  class(x) <- c("colorDF", class(x))
  x <- .set_style(x, .get_style(x, theme=theme))

  x
}


#' Make a dataframe colorful
#'
#' Make a dataframe colorful
#' @param x a data frame or similar object (e.g. tibble)
#' @param ... further arguments are passed to [colorDF()].
#' @return a colorful data frame – identical object but with the `.style`
#'         attribute set.
#' @seealso [df_style()] on how to modify style of the colorful data frame
#' @export
as.colorDF <- function(x, ...) {
  colorDF(x, ...)
}




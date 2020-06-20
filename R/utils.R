`%||%` <- function(x, y) {
  if(is.null(x) || is.na(x)) x <- y
  return(x)
}


#' @rdname df_style
#' @export
`df_style<-` <- function(x, element=NULL, value) {
  style <- attr(x, ".style") %||% list()

  if(length(element) > 1) {
    stop("Cannot set more than one element")
  }

  if(!is.null(element)) {
    style[[element]] <- value
  } else {
    if(!is.list(value) || is.null(names(value))) {
      stop("If `element` is empty, then `value` must be a named list")
    }
    style[names(value)] <- value
  }

  attr(x, ".style") <- style
  return(x)
}

#' Get or set style of a colorful data frame
#'
#' Get or set style of a colorful data frame
#'
#' Colorful data frames store the styles in the `.style` attribute of the
#' data frame object. This attribute is a list with a number of keywords:
#' 
#' * row.names, col.names, interleave: formatting styles for row names, table header and
#'   every second line in the table. If these elements are NULL, no styles
#'   will be applied. See "Formatting styles" below.
#' * autoformat (logical): whether column type should be guessed from column names
#'   (which allows automatically recognizing a column called "pvalue" as a
#'   p-value and color significant cells.
#' * col.styles: a list mapping the column names to formatting styles. 
#' * col.types: a list mapping the column names to column types. For example,
#'   if it is `list(result="pval")`, then the column with name "result" will
#'   be considered to be a p-value and styled accordingly.
#' * type.styles: a list mapping column types to formatting styles. See "Column types"
#'   below.
#'
#' @section Formatting styles
#'
#' Each formatting style is a list describing style of the formatting and
#' coloring the text elements. Following elements of that list are recognized:
#'
#'  * fg, bg: foreground and background colors specified as R name (use
#'    `colors()` to get available colors) or HTML hexadicimal code 
#'  * fg_sign, fg_ns: for p-values, foreground colors for significant / not
#'    significant values, respectively
#'  * is.pval: whether the values are to be treated as p-values
#'  * is.numeric: whether the values are to be treated as numeric
#'  * align: how the values should be aligned (right, left or center)
#'  * sign.thr: for p-values, the threshold of significance
#'  * decoration: a character vector which may include the following key
#'    words: inverse, bold, italic
#'
#' @section Column types
#' 
#' Rather than directly assigning a style to a column (which is possible
#' using the `col.styles` element) it is preferable to change a style
#' associated with a column type. Several such types are defined in the
#' default styles:
#'
#'  * character
#'  * numeric
#'  * integer
#'  * factor
#'  * identifier
#'  * pval
#'  * default
#' @return `df_style(x)` returns a list. Assignment results in a data frame
#' with a modified style.
#' @param x a colorful data frame
#' @param element element or elements of the style
#' @param value one or more values to set
#' @examples
#' df <- as.colorDF(mtcars)
#'
#' ## row names should be red on yellow background (yikes!)
#' df_style(df, "row.names") <- list(fg="red", bg="#FFFF00")
#'
#' ## example of assigning multiple values in one assignment:
#' df_style(df) <- list(interleave=list(fg="#FFFFFF", bg="blue"),
#'                      row.names=list(fg="blue", bg="#FFFF00"), 
#'                      col.names=list(bg="#FFFFFF", fg="#FF00FF", 
#'                      decoration=c("bold", "italic"))
#'
#' @export
df_style <- function(x, element) {

  style <- attr(x, ".style") 
  if(is.null(style)) return(style)

  return(style[element])
}




`col_type<-` <- function(x, value) {
  style <- attr(x, ".style")
  if(is.null(style)) {
    warning("Object does not have a defined style")
    return(x)
  }

  if(!is.null(value) && (!is.character(value) || is.null(names(value)))) {
    stop("Column types must be a named character vector")
  }

  if(!is.null(value) && !is.null(style$col.types)) {
    value <- c(value, style$col.types)
    value <- value[ !duplicated(names(value)) ]
  }

  style$col.types <- value

  attr(x, ".style") <- style
  return(x)
}

col_type <- function(x, cols=NULL) {

  style <- attr(x, ".style")
  if(is.null(style)) {
    warning("Object does not have a defined style")
    return(NULL)
  }

  if(!is.null(cols)) {
    return(style$col.types[cols])
  } else {
    return(style$col.types)
  }

}

.catf <- function(x, ...) cat(sprintf(x, ...))


remove_df_style <- function(x) {
  attr(x, ".style") <- NULL
  x
}

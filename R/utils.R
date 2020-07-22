## combines cat and sprintf
.catf <- function(x, ...) cat(sprintf(x, ...))

## combines cat and sprintf, plus a newline
.catfn <- function(x, ...) {
  .catf(x, ...)
  cat("\n")
}


## replaces null/na values by a default
## usage: x %OR% y
`%OR%` <- function(x, y) {
  if(is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) x <- y
  return(x)
}

## classes to class identifiers
cl2ids <- function(classes) {
  cl.ids <- c(character="chr", integer="int", numeric="dbl", factor="fct", list="lst", logical="lgl")
  ids <- cl.ids[classes]
  ids[ is.na(ids) ] <- "oth"
  ids <- sprintf("<%s>", ids)
  return(ids)
}


#' @rdname df_style
#' @export
`df_style<-` <- function(x, element=NULL, value) {
  style <- attr(x, ".style") %OR% list()

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
#' * fg, bg, decoration: formatting styles to be applied to the whole table
#'   (see "Formatting styles" below)
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
#' * type.styles: a list mapping column types to formatting styles. See "Formatting styles"
#'   below and help page for [col_type()].
#' * fixed.width: if not NULL, all columns have the same width
#' * sep: string separating the columns 
#' * digits: how many digits to use
#' * tibble.style: if not NULL, cut off columns that do not fit the width
#'
#' @section Formatting styles:
#'
#' Each formatting style is a list describing style of the formatting and
#' coloring the text elements. Following elements of that list are recognized:
#'
#'  * fg, bg: foreground and background colors specified as R name (use
#'    `colors()` to get available colors) or HTML hexadicimal code 
#'  * fg_sign: for p-values, foreground color for significant 
#'    values
#'  * fg_true, fg_false: foreground colors for logical vectors
#'  * fg_neg: for numeric values, foreground color for negative values
#'  * fg_na: color for NA values
#'  * is.pval: whether the values are to be treated as p-values
#'  * is.numeric: whether the values are to be treated as numeric
#'  * align: how the values should be aligned (right, left or center)
#'  * sign.thr: for p-values, the threshold of significance
#'  * digits: how many digits to use
#'  * decoration: a character vector which may include the following key
#'    words: inverse, bold, italic
#'
#' @return `df_style(x)` returns a list. Assignment results in a data frame
#' with a modified style.
#' @param x a colorful data frame
#' @param element element or elements of the style
#' @param value one or more values to set
#' @seealso [print.colorDF()] on printing options; [col_type()] for column
#' types.
#' @examples
#' df <- as.colorDF(mtcars)
#'
#' ## row names should be red on yellow background (yikes!)
#' df_style(df, "row.names") <- list(fg="red", bg="#FFFF00")
#'
#' ## you can use `$` to access the elements
#' ## here, show significant p-values in green
#' df_style(df)$type.styles$pval$fg_sign <- "green"
#'
#' ## example of assigning multiple values in one assignment:
#' df_style(df) <- list(interleave=list(fg="#FFFFFF", bg="blue"),
#'                   row.names=list(fg="blue", bg="#FFFF00"), 
#'                   col.names=list(bg="#FFFFFF", fg="#FF00FF", 
#'                                  decoration=c("bold", "italic")))
#'
#' @export
df_style <- function(x, element) {

  style <- attr(x, ".style") 
  if(is.null(style)) return(style)

  return(style[element])
}



#' @rdname col_type
#' @export
`col_type<-` <- function(x, cols=NULL, value) {
  col_type <- attr(x, ".coltp")
  if(is.null(col_type)) {
    col_type <- list()
  }

  if(!is.null(cols) && !is.null(value) && length(value) != 1 && length(cols) != length(value)) {
    stop(sprintf("Cannot asign %d column types to %d columns",
      length(value), length(cols)))
  }

  if(is.numeric(cols)) cols <- paste0("X.", cols)

  if(is.null(cols)) {
    col_type <- value
  } else {
    col_type[cols] <- value
  }


# if(!is.null(value) && (!is.character(value) || is.null(names(value)))) {
#   stop("Column types must be a named character vector")
# }
#
# if(!is.null(value) && !is.null(style$col.types)) {
#   value <- c(value, style$col.types)
#   value <- value[ !duplicated(names(value)) ]
# }
#
# style$col.types <- value

  attr(x, ".coltp") <- col_type
  return(x)
}


#' Set or retrieve a column type
#'
#' Set or retrieve a column type of a colorful data frame
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
#'  * match
#'  * hidden
#'  * default
#'
#' Of course, new column types may be defined and their formatting defined
#' in a theme or a particular data frame style.
#'
#' @param x a colorful data frame
#' @param cols column names to set or retrieve
#' @param value character vector with column types
#' @examples
#' mc <- colorDF(mtcars)
#' col_type(mc, "gear") <- "factor"
#' col_type(mc, "gear")
#' col_type(mc) <- list(gear="factor", cyl="integer")
#' ## Note: the *class* of the columns did not change!
#' ## Chaning column type merely changes the way it is displayed
#' class(mc[["gear"]])
#'
#' ## Hide some columns
#' col_type(mc, c("disp", "hp")) <- "hidden"
#'
#' ## Create a new type and style
#' col_type(mc, "carb") <- "carbstyle"
#' df_style(mc)$type.styles$carbstyle <- list(fg="red", decoration="bold")
#' @export
col_type <- function(x, cols=NULL) {

  col_type <- attr(x, ".coltp")

  if(is.null(col_type)) {
    return(NULL)
  }

  if(!is.null(cols)) {
    if(is.numeric(cols)) cols <- paste0("X.", cols)
    return(col_type[[cols]])
  } else {
    return(col_type)
  }

}


#' Remove the colorful dataframe style attribute
#' 
#' Remove the colorful dataframe style attribute
#' 
#' Strips the color data frame style, but leaves the .coltp and class
#' intact. 
#' @return colorless data frame
#' @param x a colorful dataframe 
#' @seealso To completely remove the colorDF class and attributes, use [uncolor()]
#' @export
remove_df_style <- function(x) {
  attr(x, ".style") <- NULL
  x
}

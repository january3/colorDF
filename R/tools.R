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


#' Strip the colorDF class and style
#'
#' Strip the colorDF class and style
#' @param x colorful data frame
#' @return the original data frame like object
uncolor <- function(x) {

  attr(x, ".style") <- NULL
  attr(x, ".coltp") <- NULL
  class(x) <- setdiff(class(x), "colorDF")
  return(x)
}
  
#' Test whether an object has the class of colorDF
#'
#' Test whether an object has the class of colorDF
#' @param x a data frame like object
#' @return TRUE if the object is of colorDF class
#' @export
is.colorDF <- function(x) {
  return("colorDF" %in% class(x))
}


#' Search and highlight occurences of a pattern
#'
#' Search and highlight occurences of a pattern in a data frame
#'
#' df_search is for highlighting cells matching a specific pattern.
#' @param x a data frame
#' @param pattern a pattern; if NULL, the search results will be removed
#' @param cols which columns to search for (if NULL, all columns will be searched)
#' @return a color data frame object with the search pattern set for the given columns (or reset, if the pattern was NULL)
#' @examples
#' options(colorDF_tibble_style=TRUE)
#' if(require(dplyr)) {
#'
#'   # Search for "blue" in any column
#'   starwars %>% df_search("blue")
#'
#'   # Search in a specific column
#'   starwars %>% df_search("(Human|Wookie)", "species")
#'
#'   # save the search pattern in a new object
#'   saved <- starwars %>% df_search("blue")
#'
#'   # remove the search patterns
#'   saved <- df_search(saved)
#' }
#' @export
df_search <- function(x, pattern=NULL, cols=NULL) {

  if(!is.colorDF(x)) {
    x <- as.colorDF(x)
  }

  if(is.null(cn <- colnames(x))) {
    cn <- paste0("X.", 1:length(x))
  }
  
  if(!is.null(cols)) {
    if(is.numeric(cols)) {
      cols <- paste0("X.", cols)
    }
    sel <- cn %in% cols
  } else {
    sel <- TRUE
  }

  cty <- "match"
  cn <- cn[sel]

  ## if pattern is NULL, remove matches
  if(is.null(pattern)) {
    ctypes <- col_type(x)
    cn <- names(ctypes)[ unlist(ctypes) == "match" ]
    cty <- NULL
  } 
  
  for(i in cn) {
    col_type(x, i) <- cty
  }

  df_style(x)$type.styles$match$pattern <- pattern

  return(x)
}

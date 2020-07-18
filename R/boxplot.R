#' Boxplot in a terminal
#'
#' Show a boxplot using characters in the terminal window
#'
#' @param formula a formula
#' @param data data frame or matrix
#' @param width width of the boxplot in characters
#' @return invisibly return the color summary data frame used to draw the boxplot
#' @examples
#' term_boxplot(mpg ~ cyl, data=mtcars)
#' term_boxplot(Sepal.Length ~ Species, data=iris, width=70)
#' @importFrom stats model.frame terms
#' @export
term_boxplot <- function(formula, data=NULL, width=getOption("width")) {
  if (missing(formula)) {
    stop("Formula is missing")
  }
    
  if ((length(formula) != 3) ||
    (length(attr(terms(formula[-2]), "term.labels")) != 1)) {
            stop("Formula is incorrect")
  }

  if(is.matrix(data)) {
    data <- data.frame(data)
  }

  mf <- model.frame(formula, data)

  l <- tapply(mf[,1], mf[,2], c)

  l$Range <- sprintf("Range: %s - %s", 
    min(mf[,1], na.rm=TRUE),
    max(mf[,1], na.rm=TRUE))

  sum <- summary_colorDF(l, numformat="graphics", width=width) 
  print_colorDF(sum, width=width)
  return(invisible(sum))
}

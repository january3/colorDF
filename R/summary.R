
.log_summary <- function(x) {
  if(all(is.na(x))) return("All values missing")
  sprintf("TRUE: %d FALSE: %d", sum(x, na.rm=TRUE), sum(!x, na.rm=TRUE))
}

.graphic_summary <- function(x, summary, digits, width) {

  sel <- map_lgl(x, ~ is.numeric(.) & !all(is.na(.))) #& map_lgl(x, ~ !all(is.na(.)))

  ## no suitable numeric values
  if(sum(sel) == 0) return(summary)

  vals <- unlist(x[sel])
  r <- range(vals, na.rm=TRUE)


  graph <- map_chr(x[sel], ~ {
    .x <- quantile(.x, na.rm=TRUE)
  .x <- round((.x - r[1])/(r[2] - r[1]) * (width-1)) + 1
    chrs <- rep(" ", width)
    chrs[.x[1]:.x[5]] <- "\u2500"
    #chrs[.x[1]:.x[5]] <- "-"
    chrs[.x[1]] <- "\u251C"
    chrs[.x[5]] <- "\u2524"
    #chrs[.x[c(1,5)]] <- "|"
    #chrs[.x[2]:.x[4]] <- "\u2587"
    #chrs[.x[2]:.x[4]] <- "\u25A0"
    chrs[.x[2]:.x[4]] <- "="
    chrs[.x[2]] <- "["
    #chrs[.x[2]] <- "\u25D6"
    chrs[.x[4]] <- "]"
    #chrs[.x[4]] <- "\u25D7"
    #chrs[.x[3]] <- "\u26AB"
    #chrs[.x[3]] <- "\u253C"
    chrs[.x[3]] <- "+"
    #chrs[.x[3]] <- "\u25D9"
    #chrs[.x[3]] <- "\u25CF"
    ret <- paste(chrs, collapse="")
    return(ret)
  })

  summary[sel] <- graph
  return(summary)

}

.num_summary <- function(x, numformat, digits) {
  if(all(is.na(x))) return("All values missing")

  if(numformat == "quantiles") {
    qq <- quantile(x, na.rm=TRUE)
    qq <- format(qq, digits=digits)
    ret <- sprintf("%s [%s <%s> %s] %s",
      qq[1], qq[2], qq[3], qq[4], qq[5])
  } else if(numformat == "mean") {
    nm <- format(c(mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)), digits=digits)
    ret <- sprintf("%s \u00B1 %s", nm[1], nm[2])
  } else {
    ret <- NA
  }

  return(ret)
}

.chr_summary <- function(x, width) {
  if(length(unique(x)) == length(x)) 
    return("All values unique")

  t <- sort(table(as.character(x)), decreasing=TRUE)
  nt <- names(t)

  ret <- paste(paste0(nt, ": ", t), collapse=", ")
  if(nchar(ret) > width) {
    ret <- paste0(substr(ret, 1, width - 1), "\u2026")
  }
  return(ret)
}

.lst_summary <- function(x, width) {
  x <- unlist(x)
  ret <- .chr_summary(x, width - 6)
  ret <- paste0("<lst>:", ret)
  return(ret)
}

.get_summary <- function(x, class, numformat, digits, width) {
  if(all(is.na(x))) return("All values missing")

  if(is.list(x)) {
    x <- unlist(x)
  }

  if(is.numeric(x)) {
    return(.num_summary(x, numformat, digits))
  } else if(is.logical(x)) {
    return(.log_summary(x))
  } else {
    return(.chr_summary(x, width)) 
  }
}



#' Meaningful summary of data frames
#'
#' Meaningful, row-wise summary function for data frames
#' 
#' While this function is a summary method for objects of the colorDF
#' class, it can also be applied to any other data frame-like object.
#'
#' The summary table has five columns and as many rows as there are columns
#' in the summarized data frame. First four columns contain, respectively, 
#' column name, column class (abbreviated as in [tibbles][tibble::tibble()]),
#' number of unique values
#' and number of missing values (`NA`'s). The contents of the fifth column depends on the
#' column class and column type as follows:
#' 
#' * first, any lists are unlisted
#' * numeric columns (including integers) are summarized (see below)
#' * for character vectors and factors, if all values are unique then this
#'   is stated explicitely
#' * otherwise, for character vectors and factors, the values will be listed, starting with the most
#'   frequent. The list will be shortened to fit the screen. 
#'
#' For numeric columns, by default the quantiles 0 (minimum), .25, .50 (median), .75
#' and 1 (maximum) are shown. Following alternatives can be specified using
#' the option `numformat`:
#'
#' `summary_colorDF` is the exported version of this function to facilitate
#' usage in cases when converting an object to a colorDF is not desirable.
#' @return A colorful data frame of class colorDF containing useful
#'         information on a dataframe-like object.
#' @param object a data frame (possibly a color data frame)
#' @param digits number of significant digits to show
#' @param ... passed to `summary_colorDF`
#' @examples
#' summary(colorDF(iris))
#' summary_colorDF(iris)
#' summary_colorDF(iris, numformat="g")
#' if(require(dplyr) && require(tidyr)) {
#'   starwars %>% summary_colorDF
#'
#'   ## A summary of iris data by species
#'   iris %>% 
#'     mutate(row=rep(1:50, 3)) %>% 
#'     gather(key="parameter", value="Size", 1:4)  %>%
#'     mutate(sp.pa=paste(parameter, Species, sep=".")) %>% 
#'     select(row, sp.pa, Size) %>% 
#'     spread(key=sp.pa, value=Size) %>% 
#'     select(-row) %>%
#'     summary_colorDF(numformat="g")
#' }
#' @importFrom stats quantile
#' @export
summary_colorDF <- function(object, numformat="quantiles", digits=getOption("digits")) {
  numformat <- match.arg(numformat, c("quantiles", "mean", "graphics"))
  x <- object
  if(!is.list(x)) { stop("x must be a list") }

  cnames <- colnames(x)
  classes <- map_chr(x, ~ class(.)[1])
  nas <- map_int(x, ~ sum(is.na(.)))
  if(is.null(cnames)) { cnames <- 1:length(x) }

  uniq <- map_int(x, ~ sum(!duplicated(.) & !is.na(.)))

  ## rough estimate of the available width
  def_style <- .get_style()
  sep_n <- nchar(def_style$sep %OR% " ")
  width <- getOption("width") - (
    max(c(4, nchar(cnames))) + 5 + 
    max(c(4, nchar(nas))) + 
    max(c(7, nchar(uniq))) + 3 + sep_n * 7)

  if(width < 20) width <- 20

  #modal <- map2_chr(x, uniq, ~ if(.y == nr) { "N/A" } else {names(table(.))[1]})
  summary <- map2_chr(x, classes, .get_summary, numformat=numformat, digits=digits, width=width)

  if(numformat == "graphics") {
    summary <- .graphic_summary(x, summary, digits, width)
  }

  ret <- data.frame(Col=cnames, Class=cl2ids(classes), NAs=nas, unique=uniq,
    Summary=summary,
    stringsAsFactors=FALSE)

  rownames(ret) <- NULL
  ret <- as.colorDF(ret)
  return(ret)
}



#' @rdname summary.colorDF
#' @export
summary.colorDF <- function(object, ...) {
  summary_colorDF(object, ...)
}

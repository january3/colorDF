
.log_summary <- function(x) {
  if(all(is.na(x))) return("All values missing")
  sprintf("TRUE: %d FALSE: %d", sum(x, na.rm=TRUE), sum(!x, na.rm=TRUE))
}

.graphic_summary <- function(x, summary, digits, width) {
  sel <- map_lgl(x, ~ is.numeric(.) & !all(is.na(.))) #& map_lgl(x, ~ !all(is.na(.)))
  uq_vals <- map_lgl(x, ~ length(unique(.)) > 1)
  sel <- sel & uq_vals


  ## no suitable numeric values
  if(sum(sel) == 0) return(summary)

  vals <- unlist(x[sel])
  r <- range(vals, na.rm=TRUE)


  graph <- map_chr(x[sel], ~ {
    .x <- quantile(.x, na.rm=TRUE)
  .x <- round((.x - r[1])/(r[2] - r[1]) * (width-1)) + 1
    chrs <- rep(" ", width)
    chrs[.x[1]:.x[5]] <- .colorDF_chars$dash
    #chrs[.x[1]:.x[5]] <- "-"
    #chrs[.x[1]] <- "\u251C"
    #chrs[.x[5]] <- "\u2524"
    chrs[.x[1]] <- .colorDF_chars$heavy_left
    chrs[.x[5]] <- .colorDF_chars$heavy_right

    #chrs[.x[c(1,5)]] <- "|"
    #chrs[.x[2]:.x[4]] <- "\u2587"
    #chrs[.x[2]:.x[4]] <- "\u25A0"
    #chrs[.x[2]:.x[4]] <- "\u2B1B"
    #chrs[.x[2]:.x[4]] <- "="
    chrs[.x[2]:.x[4]] <- " "
    #chrs[.x[2]] <- "\u25D6"
    chrs[.x[2]] <- .colorDF_chars$box_left
    chrs[.x[4]] <- .colorDF_chars$box_right
    #chrs[.x[4]] <- "\u25D7"
    #chrs[.x[3]] <- "\u26AB"
    #chrs[.x[3]] <- "\u253C"
    #chrs[.x[3]] <- "\u25C6"
    #chrs[.x[3]] <- "\u25D9"
    #chrs[.x[3]] <- "\u25CF"
    chrs[.x[3]] <- "+"
    ret <- paste(chrs, collapse="")
    return(ret)
  })

  summary[sel] <- graph
  return(summary)

}

.num_summary <- function(x, numformat, digits) {
  if(all(is.na(x))) {
    return("All values missing")
  }

  if(length(uq <- unique(x)) == 1L) {
    return(format(uq, digits=digits))
  }

  if(numformat == "quantiles") {
    qq <- quantile(x, na.rm=TRUE)
    qq <- format(qq, digits=digits)
    ret <- sprintf("%s [%s <%s> %s] %s",
      qq[1], qq[2], qq[3], qq[4], qq[5])
  } else if(numformat == "mean") {
    nm <- format(c(mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)), digits=digits)
    ret <- sprintf("%s %s %s", nm[1], .colorDF_chars$pmin, nm[2])
  } else {
    ret <- NA
  }

  return(ret)
}

.chr_summary <- function(x, width) {
  if(length(uq <- unique(x)) == 1L) {
    return(paste0("Only one value: ", uq))
  }

  if(length(unique(x)) == length(x)) {
    return("All values unique")
  }


  t <- sort(table(as.character(x)), decreasing=TRUE)
  nt <- names(t)

  ret <- paste(paste0(nt, ": ", t), collapse=", ")
  if(nchar(ret) > width) {
    ret <- paste0(substr(ret, 1, width - 1), .colorDF_chars$ellipsis)
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



#' Meaningful summary of lists and data frames
#'
#' Meaningful, row-wise summary function for lists and data frames
#' 
#' While this function is a summary method for objects of the colorDF
#' class, it can also be applied to any other data frame-like object.
#'
#' The summary table has five columns and as many rows as there are columns
#' in the summarized data frame (or elements in a list). First four columns contain, respectively, 
#' column name, column class (abbreviated as in [tibbles][tibble::tibble()]),
#' number of unique values
#' and number of missing values (`NA`'s). The contents of the fifth column depends on the
#' column class and column type as follows:
#' 
#' * first, any lists are unlisted
#' * numeric columns (including integers) are summarized (see below)
#' * for character vectors and factors, if all values are unique or missing (NA) then this
#'   is stated explicitely
#' * otherwise, for character vectors and factors, the values will be listed, starting with the most
#'   frequent. The list will be shortened to fit the screen. 
#'
#' For numeric columns, by default the quantiles 0 (minimum), .25, .50 (median), .75
#' and 1 (maximum) are shown. Following alternatives can be specified using
#' the option `numformat`:
#' 
#' * "mean": mean +- standard deviation
#' * "graphics": a graphical summary. Note that all numerical columns will
#'    be scaled with the same parameter, so this option makes sense only if the
#'    numerical columns are comparable. The graphics summary looks like
#'    this: ---|  +  |---- and corresponds to a regular box plot, indicating the 
#'    extremes and the three quartiles (- ... - indicates the data range, |...| the
#'    interquartile range and '+' stands for the median).
#'
#' `summary_colorDF` is the exported version of this function to facilitate
#' usage in cases when converting an object to a colorDF is not desirable.
#' @return A colorful data frame of class colorDF containing useful
#'         information on a dataframe-like object.
#' @param object a data frame (possibly a color data frame)
#' @param digits number of significant digits to show (default: 3)
#' @param numformat format of the summary for numerical values. Can be one
#'        of "quantiles", "mean" and "graphics"
#' @param width width of the summary table in characters
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
#'     mutate(pa.sp=paste(parameter, Species, sep=".")) %>% 
#'     select(row, pa.sp, Size) %>% 
#'     spread(key=pa.sp, value=Size) %>% 
#'     select(-row) %>%
#'     summary_colorDF(numformat="g")
#' }
#' @importFrom stats quantile sd
#' @export
summary_colorDF <- function(object, numformat="quantiles", digits=3, width=getOption("width")) {
  numformat <- match.arg(numformat, c("quantiles", "mean", "graphics"))
  x <- object
  if(!is.list(x)) { stop("x must be a list or a data frame") }

  cnames <- names(x)
  classes <- map_chr(x, ~ class(.)[1])
  nas <- map_int(x, ~ sum(is.na(.)))
  if(is.null(cnames)) { cnames <- 1:length(x) }

  uniq <- map_int(x, ~ sum(!duplicated(.) & !is.na(.)))

  ## rough estimate of the available width
  def_style <- .get_style()
  sep_n <- nchar(def_style$sep %OR% " ")
  width <- width - (
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



#' @rdname summary_colorDF
#' @export
summary.colorDF <- function(object, ...) {
  summary_colorDF(object, ...)
}


.log_summary <- function(x) {
  if(all(is.na(x))) return("All values missing")
  sprintf("TRUE: %d FALSE: %d", sum(x, na.rm=TRUE), sum(!x, na.rm=TRUE))
}

.num_summary <- function(x) {
  if(all(is.na(x))) return("All values missing")

  qq <- quantile(x, na.rm=TRUE)
  qq <- format(qq, digits=2)

  sprintf("%s [%s <%s> %s] %s",
    qq[1], qq[2], qq[3], qq[4], qq[5])
}

.chr_summary <- function(x) {
  if(length(unique(x)) == length(x)) 
    return("All values unique")

  t <- sort(table(as.character(x)), decreasing=TRUE)
  nt <- names(t)

  sel <- nchar(nt) > 15
  nt[sel] <- paste0(substr(nt[sel], 1, 12), '...')

  last <- which(cumsum(nchar(nt)) > 20)[1] - 1
  if(is.na(last)) last <- length(t)

  ret <- paste(paste0(nt[1:last], ": ", t[1:last]), collapse=", ")
  if(last < length(t)) ret <- paste0(ret, ", ...")
  return(ret)
}

get_summary <- function(x, class) {
  if(all(is.na(x))) return("All values missing")

   if(is.numeric(x)) {
     return(.num_summary(x))
   } else if(is.logical(x)) {
     return(.log_summary(x))
   } else return(.chr_summary(x)) 
}



#' Meaningful summary of data frames
#'
#' Meaningful summary of data frames
#' 
#' While this function is a summary method for objects of the colorDF
#' class, it can also be applied to any other dataframe-like object.
#'
#' `summary_colorDF` is the exported version of this function to facilitate
#' usage in cases when converting an object to a colorDF is not desirable.
#' @return A colorful data frame of class colorDF containing useful
#'         information on a dataframe-like object.
#' @param object a data frame (possibly a color data frame)
#' @param ... ignored
#' @examples
#' summary(colorDF(iris))
#' summary_colorDF(iris)
#' @importFrom stats quantile
#' @export
summary.colorDF <- function(object, ...) {
  x <- object
  if(!is.data.frame(x)) { stop("x must be a data frame") }

  cnames <- colnames(x)
  classes <- map_chr(x, ~ class(.)[1])
  nas <- map_int(x, ~ sum(is.na(.)))
  nr <- nrow(x)
  if(is.null(cnames)) { cnames <- 1:ncol(x) }

  uniq <- map_int(x, ~ length(unique(.)))

  #modal <- map2_chr(x, uniq, ~ if(.y == nr) { "N/A" } else {names(table(.))[1]})
  summary <- map2_chr(x, classes, get_summary)

  ret <- data.frame(Col=cnames, Class=cl2ids(classes), NAs=nas, unique=uniq,
    Summary=summary,
    stringsAsFactors=FALSE)

  rownames(ret) <- NULL
  ret <- as.colorDF(ret)
  return(ret)
}



#' @rdname summary.colorDF
#' @export
summary_colorDF <- function(object, ...) {
  summary.colorDF(object, ...)
}

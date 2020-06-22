#' Format a vector using styles
#' 
#' Format a vector (data frame column) aligning, rounding the numbers and
#' adding color.
#' @param x a vector
#' @param col_name optional: a column name (see Details)
#' @param style A list with style definition
#' @param format Whether the vector should be formatted and aligned
#' @param col_width optional: width to which elements of the vector should be aligned
#' @param df_style style for the whole data frame
#' @param prefix prefix (column separator) to add to each element of x
#' @export 
format_col <- function(x, col_name=NULL, style=NULL, df_style=NULL, format=TRUE, col_width=NULL, prefix="\u2502") {

  if(is.null(style)) style <- list()
  if(!is.numeric(x)) { style$is.numeric <- NULL ; style$is.pval <- NULL } 
  if(is.null(col_name)) { col_name <- "" }
  digits <- style$digits %||% df_style$digits %||% getOption("digits")
  if(!is.null(df_style$sep)) { prefix <- df_style$sep }

  x.ret <- as.character(x)

  if(format) {
    if(!is.null(style$is.pval)) {
      x.ret <- format.pval(x, digits=digits)
    } else if(!is.null(style$is.numeric)) {
      x.ret <- format(x, digits=digits)
    } 
  }


  if(is.null(col_width)) { col_width <- max(nchar(c(col_name, x.ret))) }
  if(format)             { x.ret <- col_align(x.ret, width=col_width, align=style$align) }

  na <- style$fg_na %||% df_style$fg_na
  if(any(is.na(x)) && !is.null(na)) {
    na.style <- make_style(na)
    x.ret[ is.na(x) ] <- na.style(x.ret[is.na(x)])
  }

  ## fancy
  if(is.numeric(x) && !is.null(style$is.pval)) {
    x.ret <- .format_col_pval(x, x.ret, col_name=col_name, style)
  }

  if(is.numeric(x) && !is.null(style$fg_neg)) {
    fg.style <- make_style(style$fg_neg)
    sel <- !is.na(x) & x < 0
    x.ret[sel] <- fg.style(x.ret[sel])
  }

  if(is.logical(x)) {

    if(!is.null(style$fg_true)) {
      sel <- !is.na(x) & x 
      x.ret[sel] <- make_style(style$fg_true)(x.ret[sel])
    }

    if(!is.null(style$fg_false)) {
      sel <- !is.na(x) & !x 
      x.ret[sel] <- make_style(style$fg_false)(x.ret[sel])
    }
  }


  if(!is.null(style[["fg"]])) {
    fg.style <- make_style(style$fg)
    x.ret <- fg.style(x.ret)
  }

  if(!is.null(style$bg)) {
    bg.style <- make_style(style$bg, bg=TRUE)
    x.ret <- bg.style(x.ret)
  }

  if(!is.null(style$decoration)) {
    if("bold" %in% style$decoration) { x.ret <- bold(x.ret) }
    if("italic" %in% style$decoration) { x.ret <- italic(x.ret) }
    if("inverse" %in% style$decoration) { x.ret <- inverse(x.ret) }
  }

  x.ret <- paste0(prefix, x.ret)

  attr(x.ret, ".width") <- col_width
  return(x.ret)
}

## special formatter for p values
.format_col_pval <- function(x, x.ret, col_name=NULL, style) {

  if(is.null(style$sign.thr)) style$sign.thr <- 0.05 

  sel <- !is.na(x) & x < style$sign.thr

  if(!is.null(style$fg_sign)) {
    fg.style <- make_style(style$fg_sign)
    x.ret[sel] <- fg.style(x.ret[sel])
  }

  return(x.ret)
}




.set_style <- function(x, style=NULL) {
  if(is.null(style)) style <- .get_style()
  attr(x, ".style") <- style
  x
}

## return 
.get_style <- function(x=NULL, style=NULL) {

  if(!is.null(style)) return(style)

  ncol <- num_colors()

  if(ncol == 256) {
    .default_style <- .default_style_256
  } else {
    .default_style <- .default_style_8
  }

  if(is.null(x)) {
    return(.default_style)
  }

  style <- attr(x, ".style")
  if(is.null(style)) {
    style <- .default_style
  }
  style
}



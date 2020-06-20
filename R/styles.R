#' Format a vector using styles
#' 
#' Format a vector (data frame column) aligning, rounding the numbers and
#' adding color.
#' @param x a vector
#' @param col_name optional: a column name (see Details)
#' @param style A list with style definition
#' @param format Whether the vector should be formatted and aligned
#' @param col_width optional: width to which elements of the vector should be aligned
#' @export 
format_col <- function(x, col_name=NULL, style=NULL, format=TRUE, col_width=NULL) {

  if(is.null(style)) style <- list()
  if(!is.numeric(x)) { style$is.numeric <- NULL ; style$is.pval <- NULL } 
  if(is.null(col_name)) { col_name <- "" }
  if(is.null(style$digits)) { style$digits <- getOption("digits") }

  x.ret <- as.character(x)

  if(format) {
    if(!is.null(style$is.pval)) {
      x.ret <- format.pval(x, digits=style$digits)
    } else if(!is.null(style$is.numeric)) {
      x.ret <- format(x, digits=style$digits)
    } 
  }

  if(is.null(col_width)) { col_width <- max(nchar(c(col_name, x.ret))) }
  if(format)             { x.ret <- col_align(x.ret, width=col_width, align=style$align) }

  if(any(is.na(x)) && !is.null(style$na)) {
    na.style <- make_style(style$na)
    x[ is.na(x) ] <- na.style(x[ is.na(x) ])
  }

  if(!is.null(style$fg)) {
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

  ## fancy

  if(is.numeric(x) && !is.null(style$is.pval)) {
    x.ret <- .format_col_pval(x, x.ret, col_name=col_name, style)
  }

  attr(x.ret, ".width") <- col_width
  return(x.ret)
}

## special formatter for p values
.format_col_pval <- function(x, x.ret, col_name=NULL, style) {

  if(is.null(style$sign.thr)) style$sign.thr <- 0.05 

  sel <- x < style$sign.thr

  if(!is.null(style$fg_sign)) {
    fg.style <- make_style(style$fg_sign)
    x.ret[sel] <- fg.style(x.ret[sel])
  }

  if(!is.null(style$fg_ns)) {
    fg.style <- make_style(style$fg_ns)
    x.ret[!sel] <- fg.style(x.ret[!sel])
  }
    
  return(x.ret)
}



.default_style_256 <- list(
  col.names  = list(bg="#5F0087", fg="white", decoration="bold", align="center"),
  row.names  = list(decoration="italic", fg="grey"),
  interleave = list(bg="grey98", grey=TRUE),
  col.types   = NULL,
  autoformat  = TRUE,
  data.styles = list(
    integer    = list(fg="cyan", is.numeric=TRUE, align="right"),
    character  = list(fg="red",  decoration="italic", align="left"),
    numeric    = list(fg="green", is.numeric=TRUE, align="right"),
    factor     = list(fg="blue", is.numeric=FALSE, align="left"),
    identifier = list(decoration="bold", align="right"),
    pval       = list(fg_sign="red", fg_ns="grey", sign.thr=0.05, is.pval=TRUE),
    default    = list(fg="black", align="left"))
)

.default_style_8 <- .default_style_256


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



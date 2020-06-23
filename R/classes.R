.getslots <- function(x, width) {

  cols.w <- map_int(1:length(x), ~  max(nchar(strip_style(x[[.x]]))))

#  cols.w <- cols.w + 1
  slots <- rep(0, length(cols.w))
  screen <- cumsum(cols.w) 
  nsl <- floor(sum(cols.w)/width)
  screen <- screen - nsl * width

  while(any(screen < 0)) {
    sel <- screen <= width & screen > 0
    slots[sel] <- nsl
    screen <- screen + width
    nsl <- nsl - 1
  }

  #print(rbind(cols.w, screen, slots))

  return(slots + 1)
}

.make_header <- function(c.names.formatted, row.names.w, style) {
if(!is.null(c.names.formatted)) {
    header <- paste(c.names.formatted, collapse="")
    header <- format_col(header, style=style[["col.names"]], format=FALSE, prefix="")
    header <- format_col(header, style=list(fg=style[["fg"]], bg=style[["bg"]], decoration=style[["decoration"]]), format=FALSE, prefix="")
    rnh    <- format_col("", style=style[["col.names"]], col_width=row.names.w, prefix="")
    header <- paste0(rnh, header, "\n")
    return(header)
  } else {
    return("")
  }
}

## figure out whether row names are present and if yes, if they are needed
.get_rownames <- function(x, row.names) {

  if(!row.names || is.null(r.names <- rownames(x))) {
    r.names <- rep("", nrow(x))
  }

  row.names.w <- max(nchar(r.names))
  r.names <- col_align(r.names, row.names.w, align="right")
  return(r.names)
}

## formatting columns: alignment style etc
## returns the data frame with attribute .width set to column widths
.format_cols <- function(x, c.names, ctypes, col_styles, df_style) {


  nx  <- length(x)

  cols.w <- NULL

  if(!is.null(df_style[["fixed.width"]])) {
    tmp <- map(1:nx, ~ {
      ret <- format_col(x=x[[.x]], col_name=c.names[.], style=col_styles[[.]], df_style=df_style)
    })
    cols.w <- max(map_int(tmp, ~ attr(., ".width")))
  }

  x <- map(1:nx, ~ {
    ret <- format_col(x=x[[.x]], col_name=c.names[.], style=col_styles[[.]], df_style=df_style, col_width=cols.w)
  })

  cols.w <- map_int(x, ~ attr(., ".width"))
  attr(x, ".width") <- cols.w

  return(x)
}


## Guess what the type of the column should be.
.autostyle <- function(c.names, ctypes, style) {
  if(is.null(c.names)) return(ctypes)

  st.n <- names(style[["data.styles"]]) %OR% list()

  if("pval" %in% st.n) {
    sel <- grepl("^(p.value|pvalue|pval|qval|qvalue|p|q)$", c.names, ignore.case=TRUE)
    ctypes[sel] <- "pval"
  }

  if("identifier" %in% st.n) {
    sel <- grepl("^(id|identifier)$", c.names, ignore.case=TRUE) | grepl("ID$", c.names)
    ctypes[sel] <- "identifier"
  }

  return(ctypes)
}

## find styles for columns depending on user predefined values in
## style$col.types, automatic recognition of column names and 
## column class.
.get_column_styles <- function(x, style, c.names, ctypes) {

  ctypes <- unlist(ctypes)

  ## automatically guess column type from column name
  ## (e.g. "pval" will probably be a p value)
  if(!is.null(style[["autoformat"]])) ctypes <- .autostyle(c.names, ctypes, style)

  ## if column style has been defined in `style$col.types`, then 
  ## we use that instead
  if(!is.null(c.names) && !is.null(style[["col.types"]])) {
    columns.known <- names(style[["col.types"]])
    columns.known.types <- map_chr(style[["col.types"]], ~ .x[[1]])


    sel <- columns.known %in% c.names & columns.known.types %in% names(style[["data.styles"]])
    for(i in which(sel)) {
      ctypes[ c.names == columns.known[i] ] <- style[["col.types"]][[i]]
    }
  }
 
  col_styles <- map(1:length(x), ~ {
    
    ct <- ctypes[.]
    ret <- x[[.]]

    # style defined for this column type
    if(ct %in% names(style[["data.styles"]])) {
      ret <- style[["data.styles"]][[ct]]
      # default style defined in `style`
    } else if(!is.null(style[["data.styles"]][["default"]])) {
      ret <- style[["data.styles"]][["default"]]
    } else {
      # empty style
      ret <- list()
    }
    return(ret)
  })

  return(col_styles)
}

#' Print method for colorful data frames
#'
#' Print method for colorful data frames
#'
#' 
#' @param n Number of rows to show (default=20, use Inf to show all)
#' @param x a colorful data frame (object with class colorDF)
#' @param width number of characters that the data frame should span on output
#' @param row.names if TRUE (default), row names will be shown on output
#' @param highlight a logical vector indicating which rows to highlight
#' @param ... further arguments are ignored
#' @import crayon
#' @importFrom purrr map map_int map_chr map2_chr
#' @importFrom utils head
#' @seealso [df_style()] on how to modify colorful data frames
#' @export
print.colorDF <- function(x, n=20, width=getOption("width"), 
  row.names=TRUE, 
  highlight=NULL,
  ...) {

  nc <- ncol(x) ; nr <- nrow(x)
  if(n > nr) n <- nr
  style <- .get_style(x)

  .catf("Color data frame %d x %d:\n", nc, nr)
  if(n < nr) { .catf(silver $ italic("(Showing rows 1 - %d out of %d)\n"), n, nr) }

  c.names <- colnames(x)

  x <- head(x, n)
  if(!is.null(highlight)) highlight <- head(highlight, n)

  r.names <- .get_rownames(x, row.names)
  row.names.w <- max(nchar(r.names))
  width <- width - max(nchar(r.names))

  nx <- length(x)

  ## column types
  ctypes <- map(x, ~ class(.)[1])

  ## format columns
  col_styles <- .get_column_styles(x, style, c.names, ctypes)
  cols.w <- NULL

  x <- .format_cols(x, c.names, ctypes, col_styles, df_style=style)

  cols.w <- attr(x, ".width") ## cols width without separator (prefix)
  cols.w.real <- map_int(x, ~  max(nchar(strip_style(.x))))

  ## disregard width if not sufficient to show row names + largest column
  if(max(cols.w.real) > width) { 
    width <- max(cols.w)
    warning(sprintf("Width to narrow, increasing to %d", max(cols.w) + row.names.w))
  }

  #.catf(red $ italic ("width=%d (full=%d)\n"), width, width + row.names.w)

  ## split the data frame into chunks fitting the width
  slots <- .getslots(x, width)

  ## align column names (no need to worry about NULL)
  #c.names.formatted <- col_align(c.names, cols.w, align=style$col.names$align)
  c.names.formatted <- format_col(c.names, col_width=cols.w, df_style=style)


  if(!is.null(style[["row.names"]])) {
    r.names <- format_col(r.names, style=style[["row.names"]], prefix="", format=FALSE)
  }

  ## color columns according to style

  nslots <- max(slots)

  if(!is.null(style[["tibble.style"]])) { nslots <- 1 }

  for(sl in 1:nslots) {
    cat(.make_header(c.names.formatted[ slots == sl ], row.names.w, style))

    rows <- map_chr(1:n, ~ {
      i <- .x
      paste(map_chr(x[ slots == sl ], ~ .x[[i]]), collapse="")
    })

    rows <- paste0(r.names, rows)

    if(!is.null(highlight)) {
      nas <- is.na(highlight)
      highlight[nas] <- FALSE
      rows[ highlight ] <- inverse(rows[ highlight ])
    }

    if(!is.null(style[["interleave"]])) {
      sel <- seq(2, n, by=2)
      rows[ sel ] <- format_col(rows[sel], style=style[["interleave"]], format=FALSE, prefix="")
    }

    ## global data frame style
    rows <- format_col(rows, style=list(fg=style[["fg"]], bg=style[["bg"]], decoration=style[["decoration"]]), format=FALSE, prefix="")

    cat(paste(rows, collapse="\n"))
    cat("\n\n")
  }

  if(!is.null(style[["tibble.style"]]) && any(slots != 1)) { 
    .catf(silver $ italic ("%d more columns"), sum(slots != 1))
    if(!is.null(c.names)) {
      .catf(silver $ italic(": %s"), 
        paste(c.names[ slots != 1 ], collapse=", "))
    }
    cat("\n")
  }


}

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




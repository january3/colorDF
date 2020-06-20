.getslots <- function(cols.w, width) {

  cols.w <- cols.w + 1
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

.make_header <- function(c.names, row.names.w, style) {
  if(!is.null(c.names)) {
      header <- paste(
        c(col_align("", width=row.names.w), c.names),
        collapse=" "
      )
    header <- format_col(header, style=style$col.names)
    return(paste0(header, "\n"))
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

## formatting columns: alignment, digits etc (no color yet)
## returns the data frame with attribute cols_w set to column widths
.format_cols <- function(x, digits, c.names, ctypes, col_styles) {

  nx  <- length(x)

  x <- map(1:nx, ~ {
    #ret <- col_styles[[.]](x[[.x]], c.names[.])

    #ret <- do.call(format_col, c(list(x=x[[.x]], col_name=c.names[.]), col_styles[[.]]))
    ret <- format_col(x=x[[.x]], col_name=c.names[.], style=col_styles[[.]])
  })

  cols.w <- map(x, ~ attr(., ".width"))
  attr(x, "cols_w") <- cols.w

  return(x)
}


## Guess what the type of the column should be.
.autostyle <- function(c.names, ctypes, style) {
  if(is.null(c.names)) return(ctypes)

  st.n <- names(style$data.styles)

  if("pval" %in% st.n) {
    sel <- grepl("^(p.value|pvalue|pval|qval|qvalue|p|q)$", c.names, ignore.case=TRUE)
    ctypes[sel] <- "pval"
  }

  if("identifier" %in% st.n) {
    sel <- grepl("^(id|identifier)$", c.names, ignore.case=TRUE)
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
  if(style$autoformat) ctypes <- .autostyle(c.names, ctypes, style)

  ## if column style has been defined in `style$col.types`, then 
  ## we use that instead
  if(!is.null(c.names) && !is.null(style$col.types)) {
    columns.known <- names(style$col.types)
    sel <- columns.known %in% c.names & style$col.types %in% names(style$data.styles)
    for(i in which(sel)) {
      ctypes[ c.names == columns.known[i] ] <- style$col.types[i]
    }
  }
 
  col_styles <- map(1:length(x), ~ {
    
    ct <- ctypes[.]
    ret <- x[[.]]

    # style defined for this column type
    if(ct %in% names(style$data.styles)) {
      ret <- style$data.styles[[ct]]
      # default style defined in `style`
    } else if(!is.null(style$data.styles$default)) {
      ret <- style$data.styles$default
    } else {
      # empty style
      ret <- create_col_style()
    }
    return(ret)
  })

  return(col_styles)
}

#' Print method for colorful data frames
#'
#' Print method for colorful data frames
#' @param n Number of rows to show (default=20, use Inf to show all)
#' @import crayon
#' @importFrom purrr map
#' @export
print.colorDF <- function(x, n=20, width=getOption("width"), 
  digits=getOption("digits"), row.names=TRUE, 
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
  x <- .format_cols(x, digits, c.names, ctypes, col_styles)
  #cols.w <- attr(x, "cols_w")
  if(!is.null(c.names)) {
    cols.w <- map_int(1:length(x), ~  max(nchar(c(c.names[.x], strip_style(x[[.x]])))))
  } else {
    cols.w <- map_int(1:length(x), ~  max(nchar(strip_style(x[[.x]]))))
  }


  ## disregard width if not sufficient to show row names + largest column
  if(max(cols.w) > width) { width <- max(cols.w) + row.names.w }

  .catf(red $ italic ("width=%d\n"), width)

  ## split the data frame into chunks fitting the width
  slots <- .getslots(cols.w, width)

  ## align column names (no need to worry about NULL)
  c.names.formatted <- col_align(c.names, cols.w, align=style$col.names$align)

  if(!is.null(style$row.names)) {
    r.names <- format_col(r.names, style=style$row.names)
  }

  ## color columns according to style

  for(sl in 1:max(slots)) {
    cat(.make_header(c.names.formatted[ slots == sl ], row.names.w, style))

    rows <- map_chr(1:n, ~ {
      i <- .x
      paste(map_chr(x[ slots == sl ], ~ .x[[i]]), collapse=" ")
    })

    rows <- paste0(r.names, rows)

    if(!is.null(style$interleave)) {
      sel <- seq(2, n, by=2)
      rows[ sel ] <- format_col(rows[sel], style=style$interleave, format=FALSE)
    }

    if(!is.null(highlight)) rows[ highlight ] <- inverse(rows[ highlight ])

    cat(paste(rows, collapse="\n"))
    cat("\n\n")
  }


}


#' @export
as.colorDF <- function(x, ...) {

  x <- try(as.data.frame(x), silent=TRUE)

  if(inherits(x, "try-error")) {
    stop("Object cannot be converted to a data frame")
  }

  class(x) <- c("colorDF", class(x))
  x <- .set_style(x, .get_style(x))

  x
}




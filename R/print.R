.getslots <- function(x, width) {

  cols.w <- map_int(1:length(x), ~  max(nchar(strip_style(x[[.x]]))))

  #cols.w <- cols.w + 1
  slots <- rep(0, length(cols.w))
  screen <- cumsum(cols.w) 
  nsl <- 1

  while(any(screen > 0)) {
    sel <- screen > 0 & screen < width
    slots[sel] <- nsl
    screen <- screen - max(screen[sel])
    nsl <- nsl + 1
  }

  return(slots)
}

.make_header <- function(c.names, row.names.w, cols.w, style) {
  c.names.formatted <- format_col(c.names, col_width=cols.w, df_style=style)

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

.make_header_tibble <- function(classes, row.names.w, cols.w, style) {
  ids <- cl2ids(classes)
  ids <- format_col(ids, col_width=cols.w, style=list(decoration="italic"), df_style=style)
  header <- paste(ids, collapse="")
  header <- format_col(header, style=list(fg=style[["fg"]], bg=style[["bg"]], decoration=style[["decoration"]]), format=FALSE, prefix="")
  rnh    <- format_col("", col_width=row.names.w, prefix="")
  header <- paste0(rnh, header, "\n")
  return(header)
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
.format_cols <- function(x, c.names, col_styles, df_style, classes) {
  nx  <- length(x)

  cols.w <- NULL

  x[ classes == "list" ] <- map(x[ classes == "list" ], ~ {
    .col <- .
    .col <- map_chr(.col, ~ {
      n <- length(.)
      sprintf("<list: %d el>", n)
    })
    .col
  })

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
.get_column_styles <- function(x, style, c.names, classes) {

  ctypes <- classes

  ## automatically guess column type from column name
  ## (e.g. "pval" will probably be a p value)
  if(!is.null(style[["autoformat"]])) ctypes <- .autostyle(c.names, ctypes, style)

  ## if column style has been defined in `style$col.types`, then 
  ## we use that instead
  col_types_predef <- attr(x, ".coltp")
  if(!is.null(c.names) && !is.null(col_types_predef)) {
    columns.known <- names(col_types_predef)
    columns.known.types <- map_chr(col_types_predef, ~ .x[[1]])


    sel <- columns.known %in% c.names & columns.known.types %in% names(style[["data.styles"]])
    for(i in which(sel)) {
      ctypes[ c.names == columns.known[i] ] <- col_types_predef[[i]]
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




#' @rdname print_colorDF
#' @export
print.colorDF <- function(x, ...) {
  print_colorDF(x, ...)
}

#' Print method for colorful data frames
#'
#' Print method for colorful data frames
#'
#' print_colorDF is the exported function, print.colorDF is the S3 method.
#' 
#' @param n Number of rows to show (default=20, use Inf to show all; this
#'          value can be set with options("colorDF_n"))
#' @param x a colorful data frame (object with class colorDF)
#' @param width number of characters that the data frame should span on output
#' @param row.names if TRUE (default), row names will be shown on output
#' @param highlight a logical vector indicating which rows to highlight
#' @param tibble_style whether to print with tibble style (overrides style setting)
#' @param sep column separator string (overrides style setting)
#' @param ... further arguments are ignored
#' @import crayon
#' @importFrom purrr map map_int map_chr map2_chr
#' @importFrom utils head
#' @seealso [df_style()] on how to modify colorful data frames
#' @examples
#' print(colorDF(mtcars))
#' print_colorDF(mtcars)
#' @export
print_colorDF <- function(x, n=getOption("colorDF_n"), width=getOption("width"), 
  row.names=TRUE, 
  tibble_style=getOption("colorDF_tibble_style"),
  highlight=NULL,
  sep=getOption("colorDF_sep"),
  ...) {

  if(is.null(n)) n <- 20

  nc <- ncol(x) ; nr <- nrow(x)
  if(n > nr) n <- nr
  style <- .get_style(x)

  if(!is.null(sep)) { style$sep <- sep }

  if((!is.null(tibble_style) && tibble_style == TRUE) ||
     is.null(tibble_style) && !is.null(style[["tibble_style"]]) &&
     style[["tibble_style"]] == TRUE) {
    tibble_style <- TRUE
  } else {
    tibble_style <- FALSE
  }

  name <- "Data frame"
  if(inherits(x, "colorDF")) {
    name <- "Color data frame"
  } else if(inherits(x, "tbl_df")) {
    name <- "Tibble"
  } else if(inherits(x, "data.table")) {
    name <- "Data table"
  }

  .catf("# %s %d x %d:\n", name, nc, nr)
  if(n < nr) { .catf(silver $ italic("(Showing rows 1 - %d out of %d)\n"), n, nr) }

  c.names <- colnames(x)

  x <- head(x, n)
  if(!is.null(highlight)) highlight <- head(highlight, n)

  r.names <- .get_rownames(x, row.names)
  row.names.w <- max(nchar(r.names))
  width <- width - max(nchar(r.names))

  nx <- length(x)

  ## column types
  classes <- map_chr(x, ~ class(.)[1])

  ## format columns
  col_styles <- .get_column_styles(x, style, c.names, classes)
  cols.w <- NULL

  x <- .format_cols(x, c.names, col_styles, df_style=style, classes)

  cols.w <- attr(x, ".width") ## cols width without separator (prefix)
  cols.w.real <- map_int(x, ~  max(nchar(strip_style(.x))))

  ## disregard width if not sufficient to show row names + largest column
  if(max(cols.w.real) > width) { 
    width <- max(cols.w)
    warning(sprintf("Width to narrow, increasing to %d", max(cols.w) + row.names.w))
  }

  ## split the data frame into chunks fitting the width
  slots <- .getslots(x, width)

  if(!is.null(style[["row.names"]])) {
    r.names <- format_col(r.names, style=style[["row.names"]], prefix="", format=FALSE)
  }

  ## color columns according to style

  nslots <- max(slots)

  if(tibble_style) { nslots <- 1 }

  ret <- ""

  for(sl in 1:nslots) {
    sel <- slots == sl

    ret <- ret %+% .make_header(c.names[ sel ], row.names.w, cols.w[ sel ], style)

    if(tibble_style) {
      ret <- ret %+% .make_header_tibble(classes[ sel ], row.names.w, cols.w[sel], style)
    }

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

    ret <- ret %+% paste(rows, collapse="\n")
    ret <- ret %+% "\n\n"
  }

  if(tibble_style && any(slots != 1)) { 
    ret <- ret %+% sprintf(silver $ italic ("# %d more columns: "), sum(slots != 1))
    if(!is.null(c.names)) {
      ret <- ret %+% sprintf(silver $ italic(
        paste(sprintf("%s (%s)", c.names[ slots != 1 ],
                                 cl2ids(classes[ slots != 1 ])),
                                 collapse=", ")
      ))
    }
    ret <- ret %+% "\n"
  }

  cat(ret)
  return(invisible(ret))
}



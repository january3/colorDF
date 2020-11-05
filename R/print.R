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
    header <- paste0(rnh, header)
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
  header <- paste0(rnh, header)
  return(header)
}

## figure out whether row names are present and if yes, if they are needed
.get_rownames <- function(x, row.names) {

  if(!row.names || is.null(r.names <- rownames(x))) {
    r.names <- rep("", nrow(x))
  }

  if(length(r.names) > 0) {
    row.names.w <- max(nchar(r.names))
    r.names <- col_align(r.names, row.names.w, align="right")
  } 

  return(r.names)
}

## formatting columns: alignment style etc
## returns the data frame with attribute .width set to column widths
.format_cols <- function(x, c.names, col_styles, df_style, classes, width=NULL) {
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

  ## for fixed width, we need to find out the maximum width of all columns
  if(!is.null(df_style[["fixed.width"]])) {
    tmp <- map(1:nx, ~ {
      ret <- format_col(x=x[[.x]], col_name=c.names[.], style=col_styles[[.]], df_style=df_style, max.width=width)
   })
    cols.w <- max(map_int(tmp, ~ attr(., ".width")))
  }

  x <- map(1:nx, ~ {
    ret <- format_col(x=x[[.x]], col_name=c.names[.], style=col_styles[[.]], df_style=df_style, col_width=cols.w, max.width=width)
  })

  cols.w <- map_int(x, ~ attr(., ".width"))
  attr(x, ".width") <- cols.w

  return(x)
}

## Guess what the type of the column should be based on column IDs
.autostyle <- function(c.names, ctypes, style) {
  if(is.null(c.names)) return(ctypes)

  st.n <- names(style[["type.styles"]]) %OR% list()

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

    sel <- columns.known %in% c.names & columns.known.types %in% names(style[["type.styles"]])
    for(i in which(sel)) {
      ctypes[ c.names == columns.known[i] ] <- col_types_predef[[i]]
    }
  }
 
  col_styles <- map(1:length(x), ~ {
    
    ct <- ctypes[.]
    ret <- x[[.]]

    # style defined for this column type
    if(ct %in% names(style[["type.styles"]])) {
      ret <- style[["type.styles"]][[ct]]
      # default style defined in `style`
    } else if(!is.null(style[["type.styles"]][["default"]])) {
      ret <- style[["type.styles"]][["default"]]
    } else {
      # empty style
      ret <- list()
    }
    return(ret)
  })

  return(col_styles)
}

## print a message similar to print.tbl
.tibble_message <- function(slots, c.names, classes, comment_style) {
  msg <- sprintf("# %d more columns: ", sum(slots != 1))
  
  ##sprintf(silver $ italic ("# %d more columns: "), sum(slots != 1))
  if(!is.null(c.names)) {
    msg <- msg %+% 
      paste(sprintf("%s (%s)", c.names[ slots != 1 ],
                               cl2ids(classes[ slots != 1 ])),
                               collapse=", ")
  }
  msg <- .apply_style(msg, comment_style)
	return(msg)
}


.is_tibble_style <- function(tibble_style, style) {

  ## if we have explicit information, use it 
  ## otherwise, is style[["tibble_style"]] true?
  ## both variables can be NULL, hence the %OR%
  if((tibble_style %OR% FALSE) ||
     is.null(tibble_style) && (style[["tibble_style"]] %OR% FALSE)) {
    tibble_style <- TRUE
  } else {
    tibble_style <- FALSE
  }

  return(tibble_style)
}

.hidden_cols <- function(x) {
  col_types_predef <- attr(x, ".coltp")
  if(is.null(col_types_predef) || length(col_types_predef) == 0L) {
    return(c())
  }

  hidden <- sapply(names(col_types_predef), function(x) {
    "hidden" %in% col_types_predef[[x]]
  })

  return(names(col_types_predef)[hidden])
}

#' @rdname print_colorDF
#' @export
print.colorDF <- function(x, ...) {
  print_colorDF(x, ...)
}

#' Print method for colorful data frames
#'
#' This is the core of the colorDF package – a print method for the colorful (and other) data frames. 
#'
#' print_colorDF is the exported function, print.colorDF is the S3 method.
#' Otherwise they are identical.
#'
#' `print_colorDF` is a function that can be applied to any data frame like
#' object. Using [colorDF()] to change the class of the object is only
#' necessary if modifications to the style are required, such as specifying
#' column type. However, `print_colorDF` applied to virtually any data frame,
#' tibble or data table will work. In such a case, the theme used to display the data frame 
#' will either be taken from [`getOption("colorDF_theme")`][base::getOption()] or a default theme will be used.
#'
#' @section Column types:
#' 
#' Column types are basically the column classes (numeric, factor etc.) plus a few specialized
#' types (such as p-value) which are displayed slightly differently. For
#' example, an identifier will usually be shown in bold, and significant
#' p-values will be red (details depend on the given theme and style; see
#' [col_type()] and [df_style()] for more information).
#'
#' @section Changing the default methods:
#' It is possible to assign `print_colorDF` to the default methods, thus
#' changing the way how tibbles, data frames or other data frame like objects
#' are displayed. This should be generally safe, but use it on your own peril
#' and preferably only in interactive sessions. I use the following code in my
#' [.Rprofile][base::Startup] file:
#'
#' ```
#' if(interactive()) {
#'   print.data.frame <- colorDF::print_colorDF
#'   print.tbl        <- colorDF::print_colorDF
#'   print.data.table <- colorDF::print_colorDF
#' }
#' ```
#'
#' @param n Number of rows to show (default=20, use Inf to show all; this
#'        value can be set with options("colorDF_n"))
#' @param x a colorful data frame (object with class colorDF), a
#'        data.frame, a tibble, a data.table or any other object which can be coerced
#'        to data frame with as.data.frame function.
#' @param width number of characters that the data frame should span on output
#' @param row.names if TRUE (default), row names will be shown on output
#' @param highlight a logical vector indicating which rows to highlight
#' @param tibble_style whether to print with tibble style (overrides style setting)
#' @param sep column separator string (overrides style setting)
#' @param bg set default background for the table
#' @param fg set default foreground for the table
#' @param ... further arguments are ignored
#' @import crayon
#' @importFrom purrr map map_int map_chr map2_chr map_lgl
#' @importFrom utils head
#' @seealso [df_style()] on how to modify colorful data frames styles;
#' [col_type()] on how to change the column types; [colorDF_themes_show()] to
#' demonstrate available themes; [highlight()] and [df_search()] functions on
#' how to use colorDF to highlight selected parts of a data frame.
#' @examples
#' colorDF(mtcars)
#' print_colorDF(mtcars, row.names=FALSE)
#'
#' if(require(dplyr)) {
#'     starwars %>% colorDF
#'     starwars %>% print_colorDF(highlight=.$homeworld == "Tatooine")
#'
#'     ## equivalently
#'     starwars %>% highlight(.$homeworld == "Tatooine")
#'
#'     ## with another style
#'     options(colorDF_theme="bw")
#'     starwars %>% print_colorDF(tibble_style=TRUE, sep=" |%%| ")
#' }
#' @export
print_colorDF <- function(x, 
  n=getOption("colorDF_n"), 
  width=getOption("width"), 
  row.names=TRUE, 
  tibble_style=getOption("colorDF_tibble_style"),
  highlight=NULL,
  sep=getOption("colorDF_sep"),
  bg=NULL,
  fg=NULL,
  ...) {

  name <- "Data frame (class data.frame)"
  if(inherits(x, "tmodReport")) {
    name <- "tmod report (class tmodReport)"
  } else if(inherits(x, "colorDF")) {
    name <- "Color data frame (class colorDF)"
  } else if(inherits(x, "tbl_df")) {
    name <- "Tibble (class tbl_df)"
  } else if(inherits(x, "data.table")) {
    name <- "Data table (class data.table)"
  } else {
    cl <- class(x)[1]
    err <- try(as.data.frame(x))
    if(inherits(err, "try-error")) {
      stop("This does not look like a data frame like object")
    }
    x <- err
    name <- sprintf("Data frame like object (class %s)", cl)
  }

  if(is.null(n)) n <- 20

  nc <- ncol(x) ; nr <- nrow(x)
  if(n > nr) n <- nr
  style <- .get_style(x)

  style$sep <- sep %OR% style$sep %OR% " "
  sep.length <- nchar(style$sep)

  tibble_style <- .is_tibble_style(tibble_style, style)

  comment_style <- list(fg="silver", decoration="italic")
  if(!is.null(bg)) { comment_style$bg <- bg }


  ret <- sprintf(.apply_style("# %s %d x %d:", comment_style), name, nc, nr)
  if(n < nr && nc > 0) { 
    ret <- ret %+% '\n' %+% sprintf(.apply_style("# (Showing rows 1 - %d out of %d)", comment_style), n, nr)
  }

  hidden <- .hidden_cols(x)
  if(!is.null(names(x))) {
    x <- x[ , ! names(x) %in% hidden, drop=FALSE ]
  }

  ## grouped_df is a class added when tibble groups the data
  if("grouped_df" %in% class(x) && !is.null(df_groups <- attr(x, "groups"))) {
    gn   <- names(df_groups)[ -ncol(df_groups) ]
    gnum <- nrow(df_groups)
    gn   <- paste(gn, collapse=", ")
    ret <- ret %+% '\n' %+% sprintf(.apply_style("# Groups: %s [%d]", comment_style), gn, gnum)
  }

  x <- head(x, n)
  if(!is.null(highlight)) highlight <- head(highlight, n)

  if(nc > 0) {
    ret <- ret %+% .print_df(x, style, highlight, n, row.names, width, tibble_style, comment_style) 
  } else {
    ret <- ret %+% "\n" %+% .apply_style("# No columns in the data frame", comment_style)
  }

  if(!is.null(names(x)) && length(hidden) > 0) {
    msg <- sprintf("# Hidden columns (%d): %s", length(hidden), paste(hidden, collapse=", "))
    msg <- .apply_style(msg, comment_style)
    ret <- ret %+% '\n' %+% msg
  }

  if(!is.null(bg)) {
    ret <- .apply_style(ret, list(bg=bg))
  }

  if(!is.null(fg)) {
    ret <- .apply_style(ret, list(fg=fg))
  }


  cat(ret %+% '\n')
  return(invisible(ret))
}


## actual printing of the data frame
.print_df <- function(x, style, highlight=NULL, n=20, row.names=TRUE, width=70, tibble_style=FALSE, comment_style=NULL) {

  c.names <- colnames(x) %OR% rep("", length(x))
  r.names <- .get_rownames(x, row.names)
  if(n > 0) {
    row.names.w <- max(nchar(r.names))
    width <- width - max(nchar(r.names))
  } else {
    row.names.w <- 0
  }

  nx <- length(x)

  ## column types
  classes <- map_chr(x, ~ class(.)[1])

  ## format columns
  col_styles <- .get_column_styles(x, style, c.names, classes)
  cols.w <- NULL

  x <- .format_cols(x, c.names, col_styles, df_style=style, classes, width=width - 1)

  cols.w <- attr(x, ".width") ## cols width without separator (prefix)
  cols.w.real <- map_int(x, ~  max(nchar(strip_style(.x))))

  ## disregard width if not sufficient to show row names + largest column
  if(max(cols.w.real) + nchar(style$sep) > width) { 
    old.width <- width
    width <- max(cols.w) + nchar(style$sep)
    warning(sprintf("Width too narrow, increasing from %d to %d", old.width + row.names.w, width + row.names.w))
  }

  ## split the data frame into chunks fitting the width
  slots <- .getslots(x, width)

  if(!is.null(style[["row.names"]])) {
    r.names <- format_col(r.names, style=style[["row.names"]], prefix="", format=FALSE)
  }

  ret <- .print_df_rows(x, n, slots, c.names, cols.w, row.names.w, r.names, style, classes, tibble_style, highlight) 

  ## any slots not shown in the tibble style?
  if(tibble_style && any(slots != 1)) { 
    msg <- .tibble_message(slots, c.names, classes, comment_style) 
    ret <- ret %+% "\n" %+% msg
  }


  return(ret)
}




.print_df_rows <- function(x, n, slots, c.names, cols.w, row.names.w, r.names, style, classes, tibble_style=FALSE, highlight=NULL) {
  ret <- ""

  nslots <- max(slots)

  if(tibble_style) { nslots <- 1 }


  for(sl in 1:nslots) {
    sel <- slots == sl

    ret <- ret %+% '\n' %+% .make_header(c.names[ sel ], row.names.w, cols.w[ sel ], style)

    if(tibble_style) {
      ret <- ret %+% '\n' %+% .make_header_tibble(classes[ sel ], row.names.w, cols.w[sel], style)
    }

    if(n > 0) {
      ret <- ret %+% '\n' %+% .print_single_row(n, x, slots==sl, style, highlight, r.names)
    }
  }
  return(ret)

}

.print_single_row <- function(n, x, slots_sel, style, highlight=NULL, r.names=NULL) {

    rows <- map_chr(1:n, ~ {
      i <- .x
      paste(map_chr(x[ slots_sel ], ~ .x[[i]]), collapse="")
    })

    rows <- paste0(r.names, rows)

    if(!is.null(highlight)) {
      nas <- is.na(highlight)
      highlight[nas] <- FALSE
      if(!is.null(style$highlight)) {
        rows[highlight] <- .apply_style(rows[highlight], style$highlight)
      } else {
        rows[ highlight ] <- underline(rows[ highlight ])
      }
    }

    if(n > 1 && !is.null(style[["interleave"]])) {
      sel <- seq(2, n, by=2)
      rows[ sel ] <- format_col(rows[sel], style=style[["interleave"]], format=FALSE, prefix="")
    }

    ## global data frame style
    rows <- format_col(rows, style=list(fg=style[["fg"]], bg=style[["bg"]], decoration=style[["decoration"]]), format=FALSE, prefix="")

    ret <- paste(rows, collapse="\n")
    #ret <- ret %+% "\n"
    return(ret)
}

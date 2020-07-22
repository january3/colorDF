.theme_env <- new.env(parent=emptyenv())

.theme_env[[".themes"]] <- list(

  light= list(
    description = "Suitable for black on white terminals",
    sep         = "\u2502",
    id         = "light",
    digits     = 2,
    fg_na      = "grey50",
    terminal   = "light",
    col.names  = list(bg="deepskyblue4", fg="white", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey94", grey=TRUE),
    highlight  = list(bg="#87FFFF"),
    col.types   = NULL,
    autoformat  = TRUE,
    type.styles = list(
      integer    = list(fg="cyan", fg_neg="blue", is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(fg="green", fg_neg="blue", is.numeric=TRUE, align="right"),
      logical    = list(fg_true="blue", fg_false="red", align="left"),
      factor     = list(fg="blue", is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      match      = list(fg="grey", fg_match="red"),
      pval       = list(fg_sign="red", fg="grey", sign.thr=0.05, is.pval=TRUE),
      default    = list(fg="#000000", align="left"))
  ),

  minimal= list(
    description = "Almost no style",
    sep         = " ",
    id          = "minimal",
    digits      = 2,
    interleave  = list(bg="grey94")
    ),

  universal = list(
    description = "Suitable for all terminals",
    sep         = "\u2502",
    id          = "universal",
    digits     = 2,
    fg_na      = "grey20",
    col.names  = list(bg="deepskyblue4", fg="white", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey75", grey=TRUE),
    highlight  = list(bg="grey50"),
    col.types   = NULL,
    autoformat  = TRUE,
    type.styles = list(
      integer    = list(fg="#009999", fg_neg="#000099", is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(fg="green", fg_neg="blue", is.numeric=TRUE, align="right"),
      logical    = list(fg_true="blue", fg_false="red", align="left"),
      factor     = list(fg="blue", is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      match      = list(fg_match="#CC0000"),
      pval       = list(fg_sign="#CC0000", sign.thr=0.05, is.pval=TRUE),
      default    = list(fg="#000000", align="left"))
  ),


  tibble=list(
    description  = "Very much like a tibble",
    sep          = " ",
    tibble_style = TRUE,
    id           = "tibble",
    digits       = 3,
    fg_na        = "red",
    row.names    = list(fg="grey50"),
    type.styles  = list(
             integer = list(fg_neg="red", is.numeric=TRUE),
             numeric = list(fg_neg="red", is.numeric=TRUE)
    )
    
    ),


  dark=list(
    description = "Suitable for white on black terminals",
    sep         = "\u2502",
    id         = "dark",
    digits     = 2,
    fg_na      = "grey50",
    terminal   = "dark",
    highlight  = list(bg="#005FFF"),
    col.names  = list(bg="#0000D7", fg="white", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey20", grey=TRUE),
    col.types   = NULL,
    autoformat  = TRUE,
    type.styles = list(
      integer    = list(fg="#00FFFF", fg_neg="#6666FF", is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(fg="#00FF00", fg_neg="#6666FF", is.numeric=TRUE, align="right"),
      logical    = list(fg_true="blue", fg_false="#FF0000", align="left"),
      factor     = list(fg="blue", is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      match      = list(fg="grey", fg_match="#FF0000"),
      pval       = list(fg_sign="#FF0000", fg="grey", sign.thr=0.05, is.pval=TRUE),
      default    = list(align="left"))
    ),

  bw=list(
    description = "Black and white only. Suitable for black on white terminals",
    terminal   = "light",
    sep         = "\u2502",
    id          = "bw",
    digits     = 2,
    fg_na      = "grey20",
    highlight  = list(bg="grey50"),
    col.names  = list(bg="black", fg="white", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey94", grey=TRUE),
    col.types   = NULL,
    autoformat  = TRUE,
    type.styles = list(
      integer    = list(is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(is.numeric=TRUE, align="right"),
      logical    = list(fg_true="#000000", fg_false="#333333", align="left"),
      factor     = list(is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      match      = list(fg_match="#000000", fg="grey"),
      pval       = list(fg_sign="#000000", fg="grey", sign.thr=0.05, is.pval=TRUE),
      default    = list(align="left"))
    ),

  wb=list(
    description = "Black and white only. Suitable for white on black terminals",
    terminal   = "dark",
    sep         = "\u2502",
    id          = "wb",
    digits     = 2,
    fg_na      = "grey80",
    col.names  = list(bg="white", fg="black", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey10", grey=TRUE),
    col.types   = NULL,
    autoformat  = TRUE,
    type.styles = list(
      integer    = list(is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(is.numeric=TRUE, align="right"),
      logical    = list(fg_true="#FFFFFF", fg_false="#999999", align="left"),
      factor     = list(is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      pval       = list(fg_sign="#FFFFFF", fg="grey50", sign.thr=0.05, is.pval=TRUE),
      default    = list(fg="#FFFFFF", align="left"))
    )
)


#' List all available themes for colorful data frames
#'
#' List all available themes for colorful data frames
#' @return A character vector with the names of the themes
#' @seealso [colorDF_themes_show()] for a demonstration of all themes.
#' @export
colorDF_themes <- function() {
  names(.theme_env[[".themes"]])
}





.example_colorDF <- data.frame(
  ID=c("ID1", "ID2", "ID3"),
  String=c("foo", "baz", "highlight"),
  Factor=c("foo", "baz", "boo"),
  Number=c(12.1, -pi, exp(1)),
  Integer=c(12L, -13L, 42),
  Logical=c(TRUE, FALSE, NA),
  Pvalue=c(0.001, 0.314159, NA))


#' Demonstrate all defined themes
#'
#' Demonstrate all defined themes
#' 
#' "Themes" are simply predefined styles for colorful data frames. Some are
#' suitable only for dark or light backgrounds, so this function is useful for
#' choosing what looks best on your terminal.
#'
#' When a colorful data frame is created with [colorDF()] or [as.colorDF()],
#' the default theme is assigned to it. The default theme is defined by the
#' option "colorDF_theme" set using [options()] (at startup, the default theme
#' is "light"). 
#'
#' You can also specify the theme to use when making a data frame colorful
#' with [colorDF()] by using the `theme=` parameter.
#' @param themes character vector with theme names to show
#' @param force_bg force background to "white" for light themes and "black" for dark themes
#' @examples
#' colorDF_themes_show()
#' colorDF_themes_show(themes=c("wb", "bw"))
#' @export
colorDF_themes_show <- function(themes=NULL, force_bg=FALSE) {
  .themes <- .theme_env[[".themes"]]
  themes <- .themes[ names(.themes) %in% themes ] %OR% .themes

  for(n in setdiff(names(themes), "default")) {
    .catf("Theme %s - %s:\n", n, themes[[n]]$description %OR% "no description")

    if(force_bg && !is.null(themes[[n]]$terminal)) {

      if(themes[[n]]$terminal == "dark") {
        print(colorDF(.example_colorDF, theme=n), highlight=c(FALSE, FALSE, TRUE), fg="#FFFFFF", bg="black")
      } else if(themes[[n]]$terminal == "light") {
        print(colorDF(.example_colorDF, theme=n), highlight=c(FALSE, FALSE, TRUE), fg="black", bg="#FFFFFF")
      } 
    } else {
      print(colorDF(.example_colorDF, theme=n), highlight=c(FALSE, FALSE, TRUE))
    }

    cat("\n")
  }

  .catf("Default theme: %s\n", getOption("colorDF_theme"))
  .catf('Change it with `options(colorDF_theme="<theme name>")`\n')
}




#' Return a style defined in a theme
#'
#' Return a style defined in a theme
#' @return A list with the definitions of style
#' @param theme name
#' @examples
#' get_colorDF_theme("bw")
#'
#' ## use it to change the style of a colorDF
#' foo <- colorDF(mtcars)
#' df_style(foo) <- get_colorDF_theme("wb")
#' ## Slightly modify the style
#' df_style(foo)$type.styles$pval$fg_sign <- "red"
#' @export
get_colorDF_theme <- function(theme) {
  ret <- .theme_env[[".themes"]][[theme]] 
  if(is.null(ret)) stop("No such theme")
  return(ret)
}

#' Add a new theme
#'
#' Add a new theme
#' @param theme a list containing style definitions
#' @param id an identifier (name) for the theme
#' @param description Description of th theme
#' @examples
#' newtheme <- get_colorDF_theme("bw")
#' ## Like "bw" theme, but significant p-values are red
#' newtheme$type.styles$pval$fg_sign <- "#FF0000"
#' add_colorDF_theme(newtheme, "new", "My new theme")
#' @return invisibly the new theme.
#' @export
add_colorDF_theme <- function(theme, id, description=NULL) {

  theme$id <- id
  theme$description <- description
  .theme_env[[".themes"]][[id]] <- theme
  return(invisible(theme))
}

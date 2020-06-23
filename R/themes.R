.default_style_256 <- list(
  #bg         = "black",
  #fg         = "white",
  #decoration = "italic",
  digits     = 2,
  fg_na      = "grey50",
  col.names  = list(bg="deepskyblue4", fg="white", decoration="bold", align="center"),
  row.names  = list(decoration="italic", fg="grey"),
  interleave = list(bg="grey98", grey=TRUE),
  col.types   = NULL,
  autoformat  = TRUE,
  data.styles = list(
    integer    = list(fg="cyan", fg_neg="blue", is.numeric=TRUE, align="right"),
    character  = list(fg="#000000",  decoration="italic", align="left"),
    numeric    = list(fg="green", fg_neg="blue", is.numeric=TRUE, align="right"),
    logical    = list(fg_true="blue", fg_false="red", align="left"),
    factor     = list(fg="blue", is.numeric=FALSE, align="left"),
    identifier = list(decoration="bold", align="right"),
    pval       = list(fg_sign="red", fg="grey", sign.thr=0.05, is.pval=TRUE),
    default    = list(fg="#000000", align="left"))
)

.default_style_8 <- .default_style_256




.themes <- list(

  universal = list(
    description = "Suitable for all terminals",
    id          = "universal",
    digits     = 2,
    fg_na      = "grey20",
    col.names  = list(bg="deepskyblue4", fg="white", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey75", grey=TRUE),
    col.types   = NULL,
    autoformat  = TRUE,
    data.styles = list(
      integer    = list(fg="#009999", fg_neg="#000099", is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(fg="green", fg_neg="blue", is.numeric=TRUE, align="right"),
      logical    = list(fg_true="blue", fg_false="red", align="left"),
      factor     = list(fg="blue", is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      pval       = list(fg_sign="#CC0000", sign.thr=0.05, is.pval=TRUE),
      default    = list(fg="#000000", align="left"))
  ),


  light= list(
    description = "Suitable for black on white terminals",
    id         = "light",
    digits     = 2,
    fg_na      = "grey50",
    col.names  = list(bg="deepskyblue4", fg="white", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey98", grey=TRUE),
    col.types   = NULL,
    autoformat  = TRUE,
    data.styles = list(
      integer    = list(fg="cyan", fg_neg="blue", is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(fg="green", fg_neg="blue", is.numeric=TRUE, align="right"),
      logical    = list(fg_true="blue", fg_false="red", align="left"),
      factor     = list(fg="blue", is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      pval       = list(fg_sign="red", fg="grey", sign.thr=0.05, is.pval=TRUE),
      default    = list(fg="#000000", align="left"))
  ),

  dark=list(
    description = "Suitable for white on black terminals",
    id         = "dark",
    digits     = 2,
    fg_na      = "grey20",
    col.names  = list(bg="deepskyblue4", fg="white", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey20", grey=TRUE),
    col.types   = NULL,
    autoformat  = TRUE,
    data.styles = list(
      integer    = list(fg="#00FFFF", fg_neg="#6666FF", is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(fg="#00FF00", fg_neg="#6666FF", is.numeric=TRUE, align="right"),
      logical    = list(fg_true="blue", fg_false="#FF0000", align="left"),
      factor     = list(fg="blue", is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      pval       = list(fg_sign="#FF0000", fg="grey", sign.thr=0.05, is.pval=TRUE),
      default    = list(align="left"))
    ),

  bw=list(
    description = "Black and white only. Suitable for black on white terminals",
    id          = "bw",
    digits     = 2,
    fg_na      = "grey20",
    col.names  = list(bg="black", fg="white", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey98", grey=TRUE),
    col.types   = NULL,
    autoformat  = TRUE,
    data.styles = list(
      integer    = list(is.numeric=TRUE, align="right"),
      character  = list(decoration="italic", align="left"),
      numeric    = list(is.numeric=TRUE, align="right"),
      logical    = list(fg_true="#000000", fg_false="#333333", align="left"),
      factor     = list(is.numeric=FALSE, align="left"),
      identifier = list(decoration="bold", align="right"),
      pval       = list(fg_sign="#000000", fg="grey", sign.thr=0.05, is.pval=TRUE),
      default    = list(align="left"))
    ),
  wb=list(
    description = "Black and white only. Suitable for white on black terminals",
    id          = "wb",
    digits     = 2,
    fg_na      = "grey80",
    col.names  = list(bg="white", fg="black", decoration="bold", align="center"),
    row.names  = list(decoration="italic", fg="grey"),
    interleave = list(bg="grey10", grey=TRUE),
    col.types   = NULL,
    autoformat  = TRUE,
    data.styles = list(
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

.themes$default <- .themes$light


#' List all available themes for colorful data frames
#'
#' List all available themes for colorful data frames
#' @return A character vector with the names of the themes
#' @seealso [colorDF_themes_show()] for a demonstration of all themes.
#' @export
colorDF_themes <- function() {
  names(.themes)
}


example_colorDF <- data.frame(
  ID=c("ID1", "ID2"),
  String=c("foo", "baz"),
  Number=c(12.1, -pi),
  Integer=c(12L, -13L),
  Logical=c(TRUE, FALSE),
  Pvalue=c(0.001, 0.314159))


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
#' @examples
#' colorDF_themes_show()
#' colorDF_themes_show(themes=c("wb", "bw"))
#' @export
colorDF_themes_show <- function(themes=NULL) {
  themes <- .themes[ names(.themes) %in% themes ] %OR% .themes

  for(n in setdiff(names(themes), "default")) {
    .catf("Theme %s - %s:\n", n, themes[[n]]$description %OR% "no description")
    print(colorDF(example_colorDF, theme=n))
    cat("\n")
  }

  .catf("Default theme: %s\n", getOption("colorDF_theme"))
  .catf('Change it with `options(colorDF_theme="<theme name>")`\n')
}




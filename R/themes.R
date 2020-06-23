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
  default=.default_style_256,
  light=.default_style_256,
  dark=list(
    digits     = 2,
    fg_na      = "grey20",
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
    ),
  bw=list(
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
      default    = list(fg="#000000", align="left"))
    )




)



#' List all available themes for colorful data frames
#'
#' List all available themes for colorful data frames
#' @export
colorDF_themes <- function() {
  names(themes)
}


example_colorDF <- data.frame(
  ID=c("ID1", "ID2"),
  String=c("foo", "baz"),
  Number=c(12, -42),
  Logical=c(TRUE, FALSE),
  Pvalue=c(0.001, 0.314159))


colorDF_themes_show <- function(themes=NULL) {
  themes <- .themes[ names(.themes) %in% themes ] %OR% .themes

  for(n in names(themes)) {
    .catf("Theme %s:\n", n)
    print(colorDF(example_colorDF, theme=n))
    cat("\n")
  }
}




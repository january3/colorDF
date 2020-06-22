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



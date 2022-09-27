.colorDF_chars <- list()
if(l10n_info()[["UTF-8"]]) {
  .colorDF_chars$ellipsis <- '\u2026'
  .colorDF_chars$dash <- "\u2500"
  .colorDF_chars$heavy_left <- "\u257E"
  .colorDF_chars$heavy_right <- "\u257C"
  .colorDF_chars$box_left <- "\u2524"
  .colorDF_chars$box_right <- "\u251C"
  .colorDF_chars$sep <- "\u2502"
  .colorDF_chars$pmin <- "\u00B1"
} else {
  .colorDF_chars$ellipsis <- '+'
  .colorDF_chars$dash <- "-"
  .colorDF_chars$heavy_left <- "<"
  .colorDF_chars$heavy_right <- ">"
  .colorDF_chars$box_left <- "["
  .colorDF_chars$box_right <- "]"
  .colorDF_chars$pmin <- ";"
  .colorDF_chars$sep <- "|"
}

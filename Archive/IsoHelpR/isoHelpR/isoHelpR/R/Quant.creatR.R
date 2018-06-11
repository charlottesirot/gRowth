Quant.creatR <- function() {
  path <- system.file("Quant.CreatR.Rmd", package = "isoHelpR")
  rmarkdown::run(path)
}

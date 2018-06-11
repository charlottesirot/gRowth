isoSignature.creatR <- function() {
  path <- system.file("isoSignature.CreatR.Rmd", package = "isoHelpR")
  rmarkdown::run(path)
}

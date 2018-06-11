CNEA.cleanR.Rmd <- function() {
  path <- system.file("CNEA.CleanR.Rmd", package = "isoHelpR")
  rmarkdown::run(path)
}

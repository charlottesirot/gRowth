TCD.cleanR <- function() {
  path <- system.file("TCD.CleanR.Rmd", package = "isoHelpR")
  rmarkdown::run(path)
}



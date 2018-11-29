## Overview

**gRowth** is an R package to facilitate the reading of otolith growth. It is still in construction. An old version is available on the V1.0 branch. However, this version has trouble to handle image with high resolution. our team is currently trying to fix this bug.

**gRowth** implements [R6 R classes](https://github.com/wch/R6/) and is based on [Shiny](http://shiny.rstudio.com/) for it's GUI.

#### Before installation

1. Check the version of R installed on your computer (`sessionInfo()`), must be ≥ 3.2.3, see [https://cran.r-project.org/](https://cran.r-project.org/) to update your version.

2. __For Mac users only__: gRowth package needs XQuartz. Please check, that XQuartz is installed and runs properly on your computer. If not, please visit [https://www.xquartz.org/](https://www.xquartz.org/).

## Installation

You can install the development version (not yet pushed on cran) from [GitHub](https://github.com/charlottesirot/elementR) :

```
#Install elementR's dependencies
pkgs <- c("devtools", "shiny","devtools", "shinyjs", "imager", "R6", "shinydashboard",
	"tiff", "rtiff", "jpeg", "tcltk", "tcltk2")

invisible(lapply(pkgs, function(pkgs){
	if(!require(pkgs)){
		install.packages(pkgs, dependencies=TRUE)
	}
}))

#Install elementR
devtools::install_github("charlottesirot/gRowth", ref = "V1.0", force = T, dependencies = T)

library(GRowth)
```





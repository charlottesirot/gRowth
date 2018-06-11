setwd("/home/cha/GitHub/gRowth")

library("R6")
library("shiny")
library("tcltk2")

projectPath = "/home/cha/GitHub/gRowth/src/"

source(paste(projectPath, "stateEnum.R", sep = ""))
source(paste(projectPath, "projectView.R", sep = ""))
source(paste(projectPath, "sessionState.R", sep = ""))
source(paste(projectPath, "gRowthProject.R", sep = ""))
source(paste(projectPath, "scale.R", sep = ""))
source(paste(projectPath, "fileHandler.R", sep = ""))
source(paste(projectPath, "drawOtolithView.R", sep = ""))

setwd("/home/cha/GitHub/gRowth")


runApp("src", launch.browser = TRUE)

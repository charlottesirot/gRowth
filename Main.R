setwd("/home/cha/Bureau")

projectPath = "/home/cha/Bureau/MarkCharlotteProject/"

source(paste(projectPath, "projectView.R", sep = ""))
source(paste(projectPath, "sessionState.R", sep = ""))
source(paste(projectPath, "gRowthProject.R", sep = ""))

runApp("MarkCharlotteProject", launch.browser = TRUE)

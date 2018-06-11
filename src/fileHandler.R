fileHandler <- R6Class("fileHandler", 
                       public = list(
                         listImages = function(path){
                           
                           fileList <- list.files(path, recursive = TRUE, include.dirs = FALSE, pattern = ".jpg")
                           
                           return(fileList)
                           
                         },
                         loadImage = function(path){
                           #todo
                         },
                         loadProject = function(path){
                           #todo
                         },
                         saveProject = function(project){
                           #todo
                         },
                         saveGrowthDistance = function(processedImage){
                           #todo
                         },
                         saveProcessedImage = function(processedImage){
                           #todo
                         }
                       ),
                       private = list()
                       )
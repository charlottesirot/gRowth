gRowthProject <- R6Class("gRowthProject",
                            public = list(
                              getName = function(){
                                #todo
                              },
                              initialize = function(projectPath){
                                
                                #imagePaths <- fileHandler$public_methods$listImages(projectPath)
                                #TODO load images
                              },
                              
                              ###
                              # Load scale from file
                              ###
                              loadScale = function(path){
                                private$scale = load(path)
                              }
                              
                              
                            ),
                            private = list(
                              name = NA,
                              scale = NA
                            )
)
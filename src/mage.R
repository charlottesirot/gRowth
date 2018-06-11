processedImage <- R6Class("processedImage",
                          public = list(
                            initialize = function(name, path, scale, image) {
                              self$name <- name
                              self$path <- path
                              self$scale <- scale 
                              self$image <- image
                            }, 
                            getImage = function(){
                              return(image)
                            },
                            getName = function(){
                              return(name)
                            },
                            getScale = function(){
                              return(scale)
                            }, 
                            getPath = function(){
                              return(path)
                            }, 
                            setMethodLocations = function(locations){
                              self$pointLocations <- locations
                            } 
                          ),
                          private = list(
                            name = NA,
                            path = NA, 
                            scale = NA, 
                            image = NA,
                            pointLocations = NA # location of core, growth increments and edge 
                            
                          )
)#
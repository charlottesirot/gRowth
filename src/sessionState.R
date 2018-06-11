sessionState <- R6Class("sessionState",
                        public = list(
                          value = NA, 
                          setValue = function(val){
                            self$value <- val
                          },
                          ### 
                          # Create a new project
                          ###
                          createProject = function(path){
                            private$projectPath <- path
                            private$currentState <- stateEnum()$CREATED
                            private$imageNames <- fileHandler$public_methods$listImages(private$projectPath)
                            
                            # TODO think about it
                            private$project <- gRowthProject$new(private$projectPath)
                            
                          },
                          
                          ### 
                          # Get the project path
                          ###
                          getProjectPath = function(){
                            return(private$projectPath)
                          },
                          
                          ### 
                          # Get project object
                          ###
                          getProject = function(){
                            return(private$project)
                          },
                          
                          ### 
                          # Get paths of all images for this project
                          ###
                          getImagePaths = function() {
                            return(private$imageNames)
                          },
                          
                          ### 
                          # Load scale from file
                          ###
                          uploadScale = function(path){
                            private$scalePath <- path
                            private$scaleUploaded <- TRUE
                            private$project$loadScale(path)
                          },
                          reInitializeScale = function(){
                            private$scaleUploaded <- FALSE
                            private$scalePath <- NA
                          }, 
                          isUploadScaleBoxTicked = function(){
                            return(private$uploadScaleBox)
                          },
                          isScaleUploaded = function(){
                            return(private$scaleUploaded)
                          },
                          setUploadScaleBox = function(val){
                            private$uploadScaleBox <- val
                          }, 
                          getCurrentState = function(){
                            return(private$currentState)
                          },
                          setCurrentState = function(newState){
                            private$currentState <- newState
                          }
                        ),
                        private = list(
                          projectPath = NA,
                          imageNames = NA,
                          scaleUploaded = FALSE,
                          uploadScaleBox = TRUE, # Menu button whether to upload one scale for all images
                          scalePath = NA,
                          project = NA,
                          currentState = stateEnum()$INIT
                          
                        )
                      )
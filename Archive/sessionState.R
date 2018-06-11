sessionState <- R6Class("sessionState",
                        public = list(
                          
                          ### 
                          # Flag whether project is set
                          ###
                          getProjectCreatedFlag = function(){
                            return(private$projectCreated)
                          },
                          
                          ### 
                          # Create a new project
                          ###
                          createProject = function(path){
                            private$projectPath <- path
                            private$projectCreated <- TRUE
                            
                            private$project <- gRowthProject$new(private$projectPath)
                            
                          },
                          
                          ### 
                          # Set project created flag
                          ###
                          setProjectCreatedFlag = function(flag){
                            private$projectCreated <- flag
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
                            return(list.files(private$projectPath, recursive = TRUE, include.dirs = FALSE))
                          },
                          
                          ### 
                          # Load scale from file
                          ###
                          uploadScale = function(path){
                            private$scalePath <- path
                            private$scaleUploaded <- TRUE
                            private$project$loadScale(path)
                          },
                          isUploadScaleBoxTicked = function(){
                            return(private$uploadScaleBox)
                          },
                          isScaleUploaded = function(){
                            return(private$scaleUploaded)
                          },
                          setUploadScaleBox = function(val){
                            private$uploadScaleBox <- val
                          }
                        ),
                        private = list(
                          projectPath = NA,
                          scaleUploaded = FALSE,
                          uploadScaleBox = TRUE, # Menu button whether to upload one scale for all images
                          scalePath = NA,
                          projectCreated = FALSE,
                          project = NA
                        )
                      )
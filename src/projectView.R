drawProjectVieW = function(){
  fluidRow(
    column(width = 6,
           wellPanel(
             uiOutput("drawCreateBoxStub")
           )
    ) ,
    column(width = 3,
           wellPanel(
             uiOutput("boxScale")
             
           )
    ),
    column(width = 3,
           wellPanel(
             uiOutput("boxImport")
           )
    )
  )
}

drawCreateBox = function(state){
  
  if(!is.null(state())){
    
    if(state()$getCurrentState() == stateEnum()$INIT){

      div(
        h2("Create your project"),
        actionButton("createSessionBtn", "Choose the data")
      )

    } else if(state()$getCurrentState() == stateEnum()$CREATED){
      div(
        h2("Create your project"),
        actionButton("createSessionBtn", "Change the data"),
        uiOutput("drawProjectInformationStub"),
        br(),
        uiOutput("drawScaleUploaderStub"),
        br(),
        uiOutput("finalizeLoadingProjectStub")
      )
    } else if(state()$getCurrentState() == stateEnum()$VALIDATED){
      div(
        h2("Create your project"),
        actionButton("createSessionBtn", "Change the data"),
        uiOutput("drawProjectInformationStub")
      )
    } else {}
    
  }
  
}

drawProjectInformation = function(state){
  
  imagePaths <- state()$getImagePaths()
  
  div(
    h3("Project uploaded"),
    lapply(1:length(imagePaths), function(x){
      p(imagePaths[x])
    })

  )
  
}

drawScaleUploader = function(state){
  
  div(
    checkboxInput("uploadScaleBox", "Upload a scale for the whole session?", value = state()$isUploadScaleBoxTicked()),
    actionButton("uploadScaleBtn", "Upload scale")
  )
  
}

finalizeLoadingProject = function(state){
  
  if((state()$isUploadScaleBoxTicked() & state()$isScaleUploaded()) | !state()$isUploadScaleBoxTicked()){
    div(
      actionButton("FinalizeProjectCreation", "Start analysis!")
    )
}
  
  
}


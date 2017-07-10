shinyServer(function(input, output) {
  
  ## creating sessionState ##
  
  state <- sessionState$new()
  
  ##### Project View #######
  output$ProjectView = renderUI({
      drawProjectVieW()
  })
  
 output$drawCreateBoxStub = renderUI({
   
  # Hack to force a redrawing of the view after the buttons below are clicked
  input$createSessionBtn
  drawCreateBox(state)
 })
 
 output$drawProjectInformationStub = renderUI({
   drawProjectInformation(state)
 })
 
 output$drawScaleUploaderStub = renderUI({
   drawScaleUploader(state)
 })
 
 # Observer for createSessionBtn
 observe({
   if(!is.null(input$createSessionBtn)){
     if(input$createSessionBtn > 0){
       projectPath <- tk_choose.dir()
       # todo : check the validity of the uploaded project
       state$createProject(projectPath)
     }
   }
 })
 
 # Observer for uploadScale
 observe({
   if(!is.null(input$uploadScaleBtn)){
     if(input$uploadScaleBtn > 0){
       scalePath <- tk_choose.files(default = "", caption = "Select scale", multi = FALSE, filters = matrix(c("R data", ".RData"), 1,2))
       #todo check the validity of the scale
       state$uploadScale(scalePath)
     }
   }
 })
 
 # Observer for uploading scale tick box
 observe({
   if(!is.null(input$uploadScaleBox)){
     ## this reactiveInput for defining the uploadScaleBox
     input$uploadScaleBox
     state$setUploadScaleBox(input$uploadScaleBox)
   }
 }) 
 
 output$finalizeLoadingProjectStub = renderUI({
   input$uploadScaleBox
   input$uploadScaleBtn
   finalizeLoadingProject(state)
 })
 
  ##### Scale Creation ########
  output$ScaleCreationView = renderUI({
    h2("Scale Creation")
  })
  
  #### Otolith Processing ######
  output$OtholitProcessingView = renderUI({
    h2("Otholit Processing")
  })
  
})
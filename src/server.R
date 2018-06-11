shinyServer(function(input, output, session) {
  
  state <- reactive({
    sessionState$new()
  })
  
  ##### Project View #######
  output$ProjectView = renderUI({
    drawProjectVieW()
  })

  output$drawCreateBoxStub = renderUI({
    updateCheckboxInput(session, "uploadScaleBox", value = input$uploadScaleBox)
    input$FinalizeProjectCreation
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
       state()$createProject(projectPath)
       state()$reInitializeScale()
     }
   }
 })
 
 # Observer for uploadScale
 observe({
   if(!is.null(input$uploadScaleBtn)){
     if(input$uploadScaleBtn > 0){
       scalePath <- tk_choose.files(default = "", caption = "Select scale", multi = FALSE, filters = matrix(c("R data", ".RData"), 1,2))
       #todo check the validity of the scale
       state()$uploadScale(scalePath)
     }
   }
 })

 output$finalizeLoadingProjectStub = renderUI({
   
   input$uploadScaleBtn
   
   if(!is.null(input$uploadScaleBox)){
     state()$setUploadScaleBox(input$uploadScaleBox)
   }
   
   finalizeLoadingProject(state)
   
 })
 
  # Change the currentState into VALIDATED
 observe({
   if(!is.null(input$FinalizeProjectCreation)){
     if(input$FinalizeProjectCreation > 0){
       state()$setCurrentState(stateEnum()$VALIDATED)
     } else {}
   } else {}
 })
 
  ##### Scale Creation ########
  output$ScaleCreationView = renderUI({
    h2("Scale Creation")
  })
  
  #### Otolith Processing ######
  output$OtholitProcessingView = renderUI({
    drawOtolithView()
  })
 
 output$Guide = renderUI({
   
 })
  
})
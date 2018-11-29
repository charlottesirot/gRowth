##############################################################
#
# gRowth - 12/10/2016
#
# charlott.sirot@gmail.comr
#
#####################################################################

calculDistance = function(x, y){

  distance <- sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2)

  return(distance)
}


growth <- function(){

  jscode <-
    '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
  '

  jscodeBis <-
    '$(document).on("shiny:connected", function(e) {
  var jsHeight = screen.height;
  Shiny.onInputChange("GetScreenHeight",jsHeight);
  });
  '

  runApp(shinyApp(

    ui =   navbarPage("gRowth", id="nav",
                      tabPanel("Project Settings",
                               fluidRow(
                                 column(width = 6,
                                        wellPanel(
                                          h2("Create your project"),
                                          uiOutput("boxCreation")
                                        )
                                 ) ,
                                 column(width = 3,
                                        wellPanel(
                                          h2("Create your scale"),
                                          uiOutput("boxScale")

                                        )
                                 ),
                                 column(width = 3,
                                        wellPanel(
                                          h2("Load your project"),
                                          uiOutput("boxImport")
                                        )
                                 )
                               )
                      ),
                      tabPanel("Scale creation",
                               tags$script(jscode),
                               tags$script(jscodeBis),
                               tags$style(HTML("
                                               #controlsScaling {
                                               /* Appearance */
                                               background-color: white;
                                               padding: 0 20px 20px 20px;
                                               cursor: move;
                                               /* Fade out while not hovering */
                                               opacity: 0.65;
                                               zoom: 0.9;
                                               transition: opacity 500ms 1s;}

                                                #controlsScaling:hover {
                                                /* Fade in while hovering */
                                                opacity: 0.95;
                                                transition-delay: 0;
                                                }

                                                #checkbox.shiny-bound-input{
                                                margin-top: 25px;
                                                }


                                                ")),
                               uiOutput("PLOTSCALING"),
                               uiOutput("PLOTSCALING2"),
                               uiOutput("boxScaling")
                      ),
                      tabPanel("Otolith analysis",
                               tags$script(jscode),
                               tags$script(jscodeBis),
                               tags$style(HTML("
                          #controls {
                            /* Appearance */
                            background-color: white;
                            padding: 0 20px 20px 20px;
                            cursor: move;
                            /* Fade out while not hovering */
                            opacity: 0.95;
                            #zoom: 0.9;
                            #transition: opacity 500ms 1s;
                          }
                          #controls:hover {
                            /* Fade in while hovering */
                            #opacity: 0.95;
                            #transition-delay: 0;
                          }

                          #checkbox.shiny-bound-input{
                            margin-top: 25px;
                          }


                    ")),

                               uiOutput("PLOT"),
                               uiOutput("PLOT2"),
                               uiOutput("box")


                      )

    ),

    server = function(input, output, session) {
      im <- reactiveValues(temp = NULL) # the image to analyze
      resWidth <- reactiveValues(temp = 0) # resolution of the screen
      resHeight <- reactiveValues(temp = 0) # resolution of the screen
      imWidth <- reactiveValues(temp = 0) # size of the image
      imHeight <- reactiveValues(temp = 0) # size of the image
      plotWidth <- reactiveValues(temp = 0) # resolution of the plot
      plotHeight <- reactiveValues(temp = 0) # resolution of the plot
      height <- reactiveValues(temp = 0) #height of the image
      ranges <- reactiveValues(x = NULL, y = NULL) # for zoom
      curvePoint <- reactiveValues(temp = list())
      projPath <- reactiveValues(temp = NULL)
      tempProj <- reactiveValues(temp = NULL) #project
      tempProjScal <- reactiveValues(temp = NULL) #project
      flagCreationProject <- reactiveValues(temp = 0)#0 for in progress, 1 when project finished to be created
      flagCreationProjectBis <- reactiveValues(temp = 0)
      flagCreationProjectTer <- reactiveValues(temp = 0)
      scale <- reactiveValues(temp = NULL) # nom du fichier correspondant à l'échelle

      observe({
        if(flagCreationProjectTer$temp == 1){
          input$createProjectButton
          input$creationScale
          isolate({
            flagCreationScale$temp <- 0
            flagImportProject$temp <- 0
            scaleCreation$temp <- NULL
          })
        }
      })

      observe({
        if(flagCreationScale$temp == 1){
          input$createProjectButton
          input$creationScale
          isolate({
            flagCreationProjectTer$temp <- 0
            flagImportProject$temp <- 0
            tempProj$temp <- NULL
          })
        }

      })

      observe({
        if(flagImportProject$temp == 1){
          isolate({
            flagCreationProjectTer$temp <- 0
            flagCreationScale$temp <- 0
            scaleCreation$temp <- NULL
          })
        }

      })


      ########################
      ### Project settings ###
      ########################
      {

        output$boxCreation <- renderUI({
          input$createProjectButton
          flagImportProject$temp
          flagCreationScale$temp

          if(flagCreationProjectTer$temp == 0){

            div(
              br(),
              uiOutput("createProject"),
              uiOutput("ChosenProject"),
              uiOutput("ScaleChoice"),
              uiOutput("ScaleUpload"),
              # uiOutput("SavePicture"),
              uiOutput("finalValid")
            )
          } else {

            projectName <- tempProj$temp$folderPath
            otolithName <-  paste(tempProj$temp$otolith, collapse = " ")
            scaleNAme <- tempProj$temp$nameScale

            div(
              br(),
              actionButton("changeCreationProject", "Change the project"),
              br(),
              br(),
              h4("The project that you are currently analyzing"),
              br(),
              p(icon("share"), paste0("Path of the project: ", projectName)),
              p(icon("share"), paste0("Name of the otoliths: ")),
              lapply(1:length(otolithName), function(x){div(otolithName[x])}),
              p(icon("share"), paste0("Name of the scale: ", scaleNAme))
            )

          }

        })

        output$createProject <- renderUI({
              actionButton("createProjectButton", "Upload your session")
        })

        output$ChosenProject <- renderUI({
          input$createProjectButton
          flagCreationProject$temp

          if(!is.null(tempProj$temp)){

            temp <-  list.files(projPath$temp, recursive = T)

            pattern <- ".tif|.TIF|.TIFF|.tiff|.png|.PNG|.bmp|.BMP|.jpg|.JPG|.JPEG|.jpeg"

            temp <- temp[grep(pattern, temp)]

            if(tempProj$temp$error !=  0){
              div(
                br(),
                br(),
                h3("Project uploaded:"),
                projPath$temp,
                br(),
                br(),
                h4("No file founded")
              )
            } else {

              flagCreationProject$temp <- 1

              div(
                br(),
                br(),
                h3("Project uploaded:"),
                projPath$temp,
                br(),
                br(),
                lapply(1:length(temp), function(x){div(temp[x])})
              )
            }


          } else {

          }
        })

        output$ScaleChoice <- renderUI({
          if(!is.null(tempProj$temp)){

            if(flagCreationProject$temp  >= 1 & tempProj$temp$error ==  0){
              div(
                br(),
                br(),
                checkboxInput("checkbox", label = h3("Upload a scale for the whole session?"), value = T)
              )

            } else {NULL}
          }

        })

        output$ScaleUpload <- renderUI({

          if(!is.null(input$checkbox)){
            if(input$checkbox == T & !is.null(tempProj$temp)){
              if(tempProj$temp$error ==  0){
                div(
                  column(2, actionButton("uploadScale", "Upload scale")),
                  column(10, uiOutput("scale")),
                  br(),
                  br()
                )
              } else { NULL}

            } else {
              NULL
            }
          } else {}
        })

        # output$SavePicture <- renderUI({
        #   if(!is.null(tempProj$temp)){
        #       if(tempProj$temp$error ==  0){
        #         div(
        #           br(),
        #           radioButtons("registerPicture", label = "Include the pictures in the saved object",
        #                        choices = list("yes", "no"),
        #                        selected = "no"),
        #           div("Keep in mind that include the pictures in the saved object is simplest to re-open the project later but could also slow the export of the data (especially in tif format)", style = "padding: 10px; background-color: #e7e7e7"),
        #           br()
        #         )
        #       } else { NULL}
        #   } else {}
        # })

        output$finalValid <- renderUI({
          if(!is.null(flagCreationProjectBis$temp)){
            if(flagCreationProjectBis$temp == 2){
              isolate({
                div(
                  br(),
                  br(),
                  actionButton("finalValidation", "Go analyzing!")
                )
              })

            } else {
              NULL
            }
          } else {}

        })

        # upload the files
        observe({
          if(!is.null(input$createProjectButton)){

            if(input$createProjectButton > 0){

              isolate({

                if(Sys.info()[1] == "Windows"){

                  projPath$temp <- choose.dir()

                } else  {

                  projPath$temp <- tk_choose.dir()

                }

                if(!is.na(projPath$temp)){
                  tempProj$temp <- gRowth_project$new(projPath$temp)
                } else {}

              })


            }
          } else {}


        })

        observe({
          input$uploadScale
          if(!is.null(input$checkbox)){
            if(input$checkbox == F){

              if(!is.null(tempProj$temp)){

              tempProj$temp$scale <- "custom"

                lapply(1:length(tempProj$temp$otolith), function(x){
                  tempProj$temp$data[[x]]$flag <- -1
                  tempProj$temp$data[[x]]$scale <- "custom"
                })
              } else {}
            } else {

              if(!is.null(tempProj$temp)){
                if(tempProj$temp$error != 1){

              tempProj$temp$scale <- ""

              lapply(1:length(tempProj$temp$otolith), function(x){
                tempProj$temp$data[[x]]$flag <- 0
                tempProj$temp$data[[x]]$scale <- ""
              })
                } else {}
              } else {}
            }
          }
        })

        # upload the scale
        observe({
          if(!is.null(input$uploadScale)){
            if(input$uploadScale > 0){
              if(!is.null(tempProj$temp)){

                if(Sys.info()[1] == "Windows"){

                  scale$temp <- choose.files(default = tempProj$temp$folderPath)

                } else  {

                  scale$temp <- tk_choose.files(default = tempProj$temp$folderPath, caption = "Select files", multi = FALSE, filters = matrix(c("R data", ".RData"), 1,2), index = 1)

                }

                if(!is.null(scale$temp)){
                  if(length(scale$temp) !=0 ){
                    if(str_detect(scale$temp, ".RData")){

                      load(scale$temp)

                      if(exists("myProject")){

                        tempProj$temp$setScale(myProject$name, pix = myProject$scalePixel, dist = myProject$scaleValue, unit = myProject$unit)

                        output$scale <- renderUI({
                          paste0("Scale Uploaded: ", tempProj$temp$nameScale)
                        })

                      } else {

                        output$scale <- renderUI({
                          "Scale not suitable"
                        })

                      }

                    } else if(!is.null(scale$temp) & !identical(scale$temp, "")){

                      tempProj$temp$setScale(NULL, pix = NULL, dist = NULL, unit = NULL)

                      output$scale <- renderUI({
                        "Scale not suitable"
                      })

                    } else {
                      output$scale <- renderUI({
                        ""
                      })
                    }
                  }

                }

            } else {}
            } else {}
          } else {}

        })

        # set the flag check box
        observe({
          input$uploadScale
          if(!is.null(tempProj$temp)){
            if(!is.null(input$checkbox)){
              if(input$checkbox == F){
                isolate(flagCreationProjectBis$temp <- 2)
              } else if(!is.null(scale$temp) & !is.null(tempProj$temp$nameScale)){
                isolate(flagCreationProjectBis$temp <- 2)
              } else {isolate(flagCreationProjectBis$temp <- 0)}
            } else {}
          } else {}
        })

        # set the flag
        observe({
          if(!is.null(tempProj$temp)){
            if(!is.null(input$finalValidation)){
            if(input$finalValidation > 0){
              flagCreationProjectTer$temp <- 1

              # if(input$registerPicture == "no"){
              #
              #   lapply(1:length(tempProj$temp$otolith), function(x){
              #     tempProj$temp$data[[x]]$data <- NA
              #   })
              # } else {}


            }
          }
          } else {}
        })

        # set the flag
        observe({
          if(!is.null(input$changeCreationProject)){
            if(input$changeCreationProject > 0){
              isolate({
                flagCreationProjectTer$temp <- 0
                flagCreationProjectBis$temp <- 0
                flagCreationProject$temp <- 0
                tempProj$temp <- NULL
                projPath$temp <- NULL
              })
          } else {}
          } else {}
        })

      }

      ######################################
      ### Settings for scale creation ######
      ######################################
      {
        flagCreationScale <- reactiveValues(temp = 0)
        scaleCreation <- reactiveValues(temp = NULL)

        output$boxScale <- renderUI({
          flagCreationProjectTer$temp
          flagImportProject$temp

          div(
            uiOutput("creationScale"),
            uiOutput("ChosenImage")
          )
        })

        output$creationScale <- renderUI({

          input$finalValidation
          flagCreationProjectTer$temp
          flagImportProject$temp

          if(flagCreationScale$temp == 1){
            div(
              br(),
              actionButton("changeCreationProject", "Change the project")
            )

          } else {
            div(
              br(),
              actionButton("creationScale", "Import image for scale")
            )

          }
        })

        output$ChosenImage <- renderUI({

          input$finalValidation
          flagCreationProjectTer$temp
          flagImportProject$temp

          if(!is.null(scaleCreation$temp) & flagCreationScale$temp == 0){
            if(!identical(scaleCreation$temp, "")){
              div(
                br(),
                br(),
                h3("Image uploaded:"),
                scaleCreation$temp,
                br(),
                br(),
                actionButton("validScaleCreation", "Go scaling!")
              )
            } else {NULL}


          } else {
            NULL
          }
        })

        # upload the image
        observe({
          if(!is.null(input$creationScale)){

            if(input$creationScale > 0){

              isolate({

                if(Sys.info()[1] == "Windows"){

                  scaleCreation$temp <- choose.files(default = getwd())

                } else  {

                  Filters <- matrix(c("image", "image", "image", "image","image","image","image", "image", "image", "image", "image","image",
                                      ".jpg", ".JPG", ".jpeg", ".JPEG", ".tif" , ".TIF", ".tiff", ".TIFF", ".bmp", ".BMP", ".png", ".PNG"),
                                    12, 2)

                  scaleCreation$temp <- tk_choose.files(default = getwd(), caption = "Select files", multi = FALSE, index = 1, filter = Filters)

                }

                })

            } else {}

          } else {}

        })

        #set flag
        observe({
          if(!is.null(input$validScaleCreation)){
            if(input$validScaleCreation > 0){
              isolate({
                flagCreationScale$temp <- 1
                tempProjScal$temp <- gRowth_scale$new(scaleCreation$temp)
              })

            }
          }
        })

        # create the currentProject element
        currentProject <- reactive({
          tempProjScal$temp
        })

        #set flag
        observe({
          if(!is.null(input$changeCreationProject)){
            if(input$changeCreationProject > 0){
              flagCreationScale$temp <- 0
              scaleCreation$temp <- NULL
            }
          }
        })
      }

      ##################################
      ### Import existing project ######
      ##################################
      {
        flagImportProject <- reactiveValues(temp = 0)

        output$boxImport <- renderUI({
          if(flagImportProject$temp == 0){
            div(
              br(),
              actionButton("ImportProject", "Import project")
            )
          } else {
            div(
              br(),
              actionButton("changeImport", "Change the project")
            )

          }

        })
      }

      ########################
      ### Image processing ###
      ########################
      {
        incrementPoint <- reactiveValues(temp = list())
        idCreation <- reactiveValues(temp = 0)
        table <- reactiveValues(temp = NULL)
        correction <- reactiveValues(temp = c(0,0,0))
        toCorrect <- reactiveValues(temp = NULL)
        checkScale <- reactiveValues(temp = NULL)
        scaleValueCreation <- reactiveValues(temp = NULL)

        zoom <- reactiveValues(temp = 0)
        zoom2 <- reactiveValues(temp = 0)
        zoom3 <- reactiveValues(temp = 0)
        previous <- reactiveValues(temp = 0)


        facMult <- reactiveValues(temp = NULL)

        observe({
          if(!is.null(tempProj$temp)){

            checkScale$temp <- vector("list", length(tempProj$temp$otolith))
            facMult$temp <- vector(length = length(tempProj$temp$otolith))

          } else {}
        })

        observe({

          if(!is.null(tempProj$temp)){
            if(!is.null(input$selectOtolith)){
            if(!is.null(input$selectOtolith) & !identical(input$selectOtolith, "") & flagCreationProject$temp == 1){

              file <- paste(projPath$temp, input$selectOtolith, sep = "/")

              im$temp <- tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$data

              imWidth$temp <- dim(im$temp)[1]
              imHeight$temp <- dim(im$temp)[2]

              ranges$x <- c(0, imWidth$temp)
              ranges$y <- c(imHeight$temp, 0)


              resWidth$temp <- input$GetScreenWidth
              resHeight$temp <- input$GetScreenHeight - 100

              isolate({
                ratio <- imWidth$temp/imHeight$temp

                if(imWidth$temp < imHeight$temp){
                  plotWidth$temp <- resWidth$temp
                  plotHeight$temp <- plotWidth$temp / ratio

                  facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)] <- resWidth$temp/imWidth$temp

                } else{
                  plotHeight$temp <- resHeight$temp
                  plotWidth$temp <- plotHeight$temp * ratio

                  facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)] <- resHeight$temp/imHeight$temp

                }
              })

            } else {}
          } else {}
          } else {}
        }) # define image and size of the picture

        observe({
          input$changeCreationProject
          if(!is.null(tempProj$temp)){
            if(!is.null(ranges$x) & !identical(resWidth$temp, numeric(0))){


                output$plot1 <- renderPlot({

                  par(mar = c(0,0,0,0), bty = "n")

                  div(plot(im$temp, xlim = ranges$x, ylim = ranges$y, axes = T))

                }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")




            } else {}
          } else {
            output$plot1 <- renderPlot({NULL

            }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")
          }
        }) # define plot1

        observe({
          input$changeCreationProject
          if(!is.null(tempProj$temp)){
            if(!is.null(input$selectOtolith)){

            if(!is.null(ranges$x)){

              input$changeCreationProject
              input$selectOtolith
              input$next1Creation
              input$CheckScale
              input$next0Creation
              input$drawAxisCreation
              input$endAxisCreation
              input$correctAxisCreation
              input$next2Creation
              input$next3Creation
              input$setPoints
              input$endSetPoints
              input$add
              input$remove
              input$move
              input$finishcorrect
              input$next4Creation
              input$correctPoints
              input$next5Creation
              input$next6Creation
              input$SaveAnalysis
              input$next7Creation
              input$next8Creation
              checkScale$temp
              input$correctAxisCheckCreation
              input$drawAxisCheckCreation
              input$correctAxisCreation
              input$next1CheckCreation
              input$next2CheckCreation
              input$textCreation
              input$textCreation2
              input$correctAxisCheckCreation
              input$selectScaleCreation
              input$next3CheckCreation
              input$next4CheckCreation
              input$next5CheckCreation
              input$ValidCheckCreation
              scaleValueCreation$temp
              previous$temp
              input$deleteAnalysis

              if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.8 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.7 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.6 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.5 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.1){

                if((which(tempProj$temp$otolith == input$selectOtolith)) <= length(checkScale$temp)){

                  if(length(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]]) == 0){

                  output$plot2 <- renderPlot({

                    par(mar = c(0,0,0,0), bty = "n")

                    plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                  }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

                } else if(length(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]]) == 1){

                  output$plot2 <- renderPlot({

                    par(mar = c(0,0,0,0), bty = "n")

                    plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                    points(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[1]][1], checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[1]][2], cex = 2, col= "red", pch = 16)

                  }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

                } else if(length(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]]) == 2){

                  output$plot2 <- renderPlot({

                    par(mar = c(0,0,0,0), bty = "n")

                    plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                    if(!identical(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]], list())){

                      points(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[1]][1], checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[1]][2], cex = 2, col= "red", pch = 16)
                      points(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[2]][1], checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[2]][2], cex = 2, col= "red", pch = 16)
                      segments(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[1]][1],checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[1]][2], checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[2]][1], checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[2]][2], col = "red")

                    } else {

                    }



                  }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

                } else {}

                } else {
                  output$plot2 <- renderPlot({

                    par(mar = c(0,0,0,0), bty = "n")

                    plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                  }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")
                }

              } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 0 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 1){

                output$plot2 <- renderPlot({

                  par(mar = c(0,0,0,0), bty = "n")

                  plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

              } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 2 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 3){

                if(length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord) == 0){

                  output$plot2 <- renderPlot({

                    par(mar = c(0,0,0,0), bty = "n")

                    plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                  }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

                } else if(length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord) == 1){


                  output$plot2 <- renderPlot({

                    par(mar = c(0,0,0,0), bty = "n")

                    plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                    lapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord), function(x){
                      if(x == 1){
                        points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][2], cex = 2, col= "yellow", pch = 16)
                      } else if(x == length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)){
                        points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][2], cex = 2, col= "blue", pch = 16)
                      } else {
                        points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][2], cex = 2, col= "red", pch = 16)
                      }

                    })

                    legend("topleft", legend = c("Core"), pch = 16, col = c("yellow"), bty = "o", bg = "white")

                  }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

                } else if(length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord) >= 2){

                  output$plot2 <- renderPlot({

                    par(mar = c(0,0,0,0), bty = "n")

                    plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n", bty = "n", bg = "white")

                    if(!is.null(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)){

                      lapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord), function(x){
                        if(x == 1){
                          points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][2], cex = 2, col= "yellow", pch = 16)
                        } else if(x == length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)){
                          points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][2], cex = 2, col= "blue", pch = 16)
                        } else {
                          points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][2], cex = 2, col= "red", pch = 16)
                        }

                      })

                      lapply(2:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord), function(x){
                        segments(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][2], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x-1]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x-1]][2], cex = 1.5, col = "red")
                      })
                    }

                    legend("topleft", legend = c("Core", "Edge"), pch = 16, col = c("yellow", "blue"), bty = "o", bg = "white")

                  }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

                } else {  }



              } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 4 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 5 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 5.5 | tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag >= 6){

                output$plot2 <- renderPlot({

                  par(mar = c(0,0,0,0), bty = "n")

                  plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                  points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[1]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[1]][2], cex = 2, col= "yellow", pch = 16)

                  t <- length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)
                  points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[t]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[t]][2], cex = 2, col= "blue", pch = 16)

                  if(!is.null(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)){
                    lapply(2:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord), function(x){
                      segments(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]][2], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x-1]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x-1]][2], cex = 1.5, col = "red")
                    })
                  }


                  input$plot_click
                  incrementPoint$temp

                  if(!is.null(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis)){
                    if(length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis) != 0){

                      lapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis), function(x){
                        points(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis[[x]][1], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis[[x]][2], cex = 2, col= "red", pch = 16)
                      })

                    }
                  }

                  legend("topleft", legend = c("Core", "Increment", "Edge"), pch = 16, col = c("yellow", "red", "blue"), bty = "o", bg = "white")


                }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

              } else {
                output$plot2 <- renderPlot({

                  par(mar = c(0,0,0,0), bty = "n")

                  plot(200, 200, xlim = ranges$x, ylim = ranges$y, axes = T, pch = 16, col = "white", type = "n")

                }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")
              }

            } else {}
            } else {}
          } else {

            output$plot2 <- renderPlot({NULL

            }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

          }
        }) # define plot2

        output$PLOT <- renderUI({
          div(plotOutput("plot1", height = "100%"))
        })

        output$PLOT2 <- renderUI({

          zoom$temp
          zoom2$temp
          zoom3$temp
          if(!is.null(tempProj$temp)){
            if(!is.null(input$selectOtolith)){
              if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <= 0){

                if(zoom$temp %% 2 == 0){

                  div(plotOutput("plot2",
                                 click = "plot_click",
                                 dblclick = "plot_dblclick",
                                 hover = "plot_hover",
                                 brush = brushOpts(
                                   id = "plot_brush",
                                   resetOnNew = TRUE
                                 ), height = "100%"), style = paste0("margin-top: -", plotHeight$temp ,"px"))
                } else {
                  div(plotOutput("plot2",
                                 dblclick = "plot_dblclick",
                                 hover = "plot_hover",
                                 brush = brushOpts(
                                   id = "plot_brush",
                                   resetOnNew = TRUE
                                 ), height = "100%"), style = paste0("margin-top: -", plotHeight$temp ,"px"))
                }
              } else {

                if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag < 5){
                  if(zoom2$temp %% 2 == 0){

                    div(plotOutput("plot2",
                                   click = "plot_click",
                                   dblclick = "plot_dblclick",
                                   hover = "plot_hover",
                                   brush = brushOpts(
                                     id = "plot_brush",
                                     resetOnNew = TRUE
                                   ), height = "100%"), style = paste0("margin-top: -", plotHeight$temp ,"px"))
                  } else {
                    div(plotOutput("plot2",
                                   dblclick = "plot_dblclick",
                                   hover = "plot_hover",
                                   brush = brushOpts(
                                     id = "plot_brush",
                                     resetOnNew = TRUE
                                   ), height = "100%"), style = paste0("margin-top: -", plotHeight$temp ,"px"))
                  }
                } else {
                  if(zoom3$temp %% 2 == 0){

                    div(plotOutput("plot2",
                                   click = "plot_click",
                                   dblclick = "plot_dblclick",
                                   hover = "plot_hover",
                                   brush = brushOpts(
                                     id = "plot_brush",
                                     resetOnNew = TRUE
                                   ), height = "100%"), style = paste0("margin-top: -", plotHeight$temp ,"px"))
                  } else {
                    div(plotOutput("plot2",
                                   dblclick = "plot_dblclick",
                                   hover = "plot_hover",
                                   brush = brushOpts(
                                     id = "plot_brush",
                                     resetOnNew = TRUE
                                   ), height = "100%"), style = paste0("margin-top: -", plotHeight$temp ,"px"))
                  }
                }



              }
            }
          } else {}

        })

        observeEvent(input$plot_dblclick, {

          brush <- input$plot_brush

          if (!is.null(brush)) {
            if(brush$xmin != brush$xmax & brush$ymax != brush$ymin){

                ranges$x <- c(brush$xmin, brush$xmax)
                ranges$y <- c(brush$ymax,brush$ymin)

              isolate({

                h <- brush$ymax - brush$ymin
                w <- brush$xmax - brush$xmin

                W <- resWidth$temp - 200
                H <- resHeight$temp - 150

                height <- H/h
                width <- W/w

                min <- min(height,width)

                plotWidth$temp <- min * w
                plotHeight$temp <- min * h
              })
            }


          } else {

              ranges$x <- c(0, imWidth$temp)
              ranges$y <- c(imHeight$temp, 0)

            isolate({
              ratio <- imWidth$temp/imHeight$temp

              if(imWidth$temp < imHeight$temp){
                plotWidth$temp <- resWidth$temp
                plotHeight$temp <- plotWidth$temp / ratio
              } else{
                plotHeight$temp <- resHeight$temp
                plotWidth$temp <- plotHeight$temp * ratio
              }
            })
          }
        }) #zoom

        observeEvent(input$plot_click, {
          if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 2){
            tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)+1]] <- c(input$plot_click$x, input$plot_click$y)
            previous$temp <- tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord
          } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.8){
              checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[length(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]]) + 1]] <- c(input$plot_click$x, input$plot_click$y)
          } else {}

        }) #double clik

        observe({
          if(!is.null(input$selectOtolith)){
            input$selectOtolith
            incrementPoint$temp <- tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis
          }
        })

        observe({
          input$plot_click
          input$selectOtolith

          if(!is.null(tempProj$temp)){
            if(!is.null( input$selectOtolith)){
              isolate({
                if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 5){
                  incrementPoint$temp[[length(incrementPoint$temp)+1]] <- c(input$plot_click$x, input$plot_click$y)
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setIncrementPoint(incrementPoint$temp)
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$putIncrementAxis(incrementPoint$temp)
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setIncrementFinalPixel()
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistanceFinalPixel()
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistanceFinalM( facMult = facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)])
                  table$temp <-  rbind(cbind(as.character(1:nrow(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal)), round(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal[,2],2)), c("total.length", round(sum(sapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal), function(x){tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal[[x]][[4]]}))*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)],2)))
                  colnames(table$temp) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))
                }
                if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 5.5){
                  if(identical(correction$temp,c(1,0,0))){
                    incrementPoint$temp[[length(incrementPoint$temp)+1]] <- c(input$plot_click$x, input$plot_click$y)
                    tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setIncrementPoint(incrementPoint$temp)
                    tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$putIncrementAxis(incrementPoint$temp)
                    tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setIncrementFinalPixel()
                    tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistanceFinalPixel()
                    tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistanceFinalM( facMult = facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)])
                    table$temp <-  rbind(cbind(as.character(1:nrow(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal)), round(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal[,2],2)), c("total.length", round(sum(sapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal), function(x){tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal[[x]][[4]]}))*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)],2)))
                    colnames(table$temp) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))
                    isolate(correction$temp <- c(0,0,0))
                  }
                  if(identical(correction$temp,c(0,1,0))){
                    toCorrect$temp <- c(input$plot_click$x, input$plot_click$y)
                    distance <- sapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis), function(x){calculDistance(toCorrect$temp, tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis[[x]])})
                    toDelete <- which(distance == min(distance))
                    tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$correctTable(toDelete, facMult = facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)])
                    if(is.null(unlist(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis))){

                      distance <- sum(sapply(1:(length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)-1), function(x){
                        tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistance(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x+1]])
                      }))
                      mat <- matrix(c("total.length", round(distance*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)])), ncol = 2)

                      colnames(mat) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))

                      table$temp <- mat

                    } else {

                      table$temp <-  rbind(cbind(as.character(1:nrow(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal)), round(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal[,2],2)), c("total.length", round(sum(sapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal), function(x){tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal[[x]][[4]]}))*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)],2)))
                      colnames(table$temp) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))
                    }
                    isolate(correction$temp <- c(0,0,0))
                  }
                  if(identical(correction$temp,c(0,0,1))){

                    if(idCreation$temp == 0){
                      toCorrect$temp <- c(input$plot_click$x, input$plot_click$y)
                      distance <- sapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis), function(x){calculDistance(toCorrect$temp, tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis[[x]])})
                      toDelete <- which(distance == min(distance))
                      tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$correctTable(toDelete, facMult = facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)])

                      if(is.null(unlist(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis))){

                        distance <- sum(sapply(1:(length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)-1), function(x){
                          tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistance(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x+1]])
                        }))

                        mat <- matrix(c("total.length", round(distance*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)],2)), ncol = 2)

                        colnames(mat) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))

                        table$temp <- mat

                      } else {

                        table$temp <-  rbind(cbind(as.character(1:nrow(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal)), round(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal[,2],2)), c("total.length", round(sum(sapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal), function(x){tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal[[x]][[4]]}))*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)],2)))
                        colnames(table$temp) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))
                      }

                      isolate({idCreation$temp <- 1})

                    } else {
                      if(idCreation$temp == 1){
                        incrementPoint$temp[[length(incrementPoint$temp)+1]] <- c(input$plot_click$x, input$plot_click$y)
                        tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setIncrementPoint(incrementPoint$temp)
                        tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$putIncrementAxis(incrementPoint$temp)
                        tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setIncrementFinalPixel()
                        tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistanceFinalPixel()
                        tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistanceFinalM( facMult = facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)])

                        if(is.null(unlist(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis))){

                          distance <- sum(sapply(1:(length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)-1), function(x){
                            tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistance(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x+1]])
                          }))

                          mat <- matrix(c("total.length", round(distance*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)],2)), ncol = 2)

                          colnames(mat) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))

                          table$temp <- mat

                        } else {

                          table$temp <-  rbind(cbind(as.character(1:nrow(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal)), round(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal[,2],2)), c("total.length", round(sum(sapply(1:length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal), function(x){tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal[[x]][[4]]}))*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)],2)))
                          colnames(table$temp) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))
                        }

                        isolate({idCreation$temp <- 0})

                        isolate(correction$temp <- c(0,0,0))

                      }
                    }




                  }

                  isolate({isolate(incrementPoint$temp <- tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis)})

                }

              })
            }

          }


        }) #double clik

        output$selectSample <- renderUI({
          if(flagCreationProject$temp == 1){

            div(
              selectInput("selectOtolith", label = "Select otolith",
                          choices = tempProj$temp$otolith,
                          selected = 1),
              br(),
              uiOutput("way")

            )
          }
        })

        output$way <- renderUI({

          input$selectOtolith
          input$next1Creation
          input$CheckScale
          input$next0Creation
          input$drawAxisCreation
          input$endAxisCreation
          input$correctAxisCreation
          input$next2Creation
          input$next3Creation
          input$setPoints
          input$endSetPoints
          input$add
          input$remove
          input$move
          input$textCreation
          input$textCreation2
          input$finishcorrect
          input$next4Creation
          input$correctPoints
          input$next5Creation
          input$next6Creation
          input$SaveAnalysis
          input$next7Creation
          input$next8Creation
          input$drawAxisCheckCreation
          input$correctAxisCreation
          input$next1CheckCreation
          input$next2CheckCreation
          input$correctAxisCheckCreation
          input$selectScaleCreation
          input$next3CheckCreation
          input$next4CheckCreation
          input$next5CheckCreation
          input$ValidCheckCreation
          scaleValueCreation$temp
          input$zoom
          input$zoom2
          input$return
          previous$temp
          checkScale$temp
          input$deleteAnalysis
          input$SaveAnalysis

          if(flagCreationProjectTer$temp == 1){

            if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -1){
              div(
                h4("Preliminary step: Define scale"),
                br(),
                column(2),
                column(8, actionButton("drawAxisCheckCreation", "Draw axis")),
                column(2),
                br(),
                br(),
                br(),
                div("Point the edges of the scale", style = "padding: 10px; background-color: #e7e7e7")
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.8){

              if(zoom$temp %% 2 == 0){
                div(
                  h4("Preliminary step: Define scale"),
                  br(),
                  column(2),
                  column(8, actionButton("zoom", "Zoom")),
                  column(2),
                  br(),
                  br(),
                  br(),
                  div("Point the edges of the scale", style = "padding: 10px; background-color: #e7e7e7")
                )
              } else {
                div(
                  h4("Preliminary step: Define scale"),
                  br(),
                  column(2),
                  column(8, actionButton("return", "Return to analyse")),
                  column(2),
                  br(),
                  br(),
                  br(),
                  div("Point the edges of the scale", style = "padding: 10px; background-color: #e7e7e7")
                )
              }


            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.7){
              div(
                h4("Preliminary step: Define scale"),
                br(),
                column(2),
                column(8, actionButton("correctAxisCheckCreation", "Correct Scale")),
                column(2, actionButton("next1CheckCreation", p(icon("arrow-circle-right"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                br(),
                br(),
                br(),
                div("Click on the Correct button to delete this axis and draw a new one", style = "padding: 10px; background-color: #e7e7e7")
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.6){

              if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$scale == "custom"){
                div(
                  h4("Preliminary step: Define scale"),
                  br(),
                  column(2, actionButton("next2CheckCreation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                  column(4, textInput("textCreation", label = "Scale value", value = "")),
                  column(4, selectInput("selectScaleCreation", label = "unit",
                                        choices = c("µm", "mm", "cm", "m"),
                                        selected = "µm")),
                  column(2),
                  br()
                )
              } else{

                temp <- calculDistance(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[1]], checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[2]]) * tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)]

                div(
                  h4("Preliminary step: Define scale"),
                  br(),
                  column(2, actionButton("next2CheckCreation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                  column(4, textInput("textCreation", label = "Scale value", value = round(temp,2))),
                  column(4, selectInput("selectScaleCreation", label = "unit",
                                        choices = c("µm", "mm", "cm", "m"),
                                        selected = tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale)),
                  column(2),
                  br()
                )
              }


            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.5){
              div(
                h4("Preliminary step: Define scale"),
                br(),
                column(2, actionButton("next2CheckCreation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(4, textInput("textCreation", label = "Scale value", value = scaleValueCreation$temp)),
                column(4, selectInput("selectScaleCreation", label = "unit",
                                      choices = c("µm", "mm", "cm", "m"),
                                      selected = "µm")),
                column(2, actionButton("next4CheckCreation", p(icon("arrow-circle-right"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                br()
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == -0.1){
              div(
                h4("Preliminary step: Define scale"),
                br(),
                column(2, actionButton("next5CheckCreation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(10, actionButton("ValidCheckCreation", "Valid Scale")),
                br()
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 0){
              div(
                h4("Optional Step: Check scale"),
                br(),
                column(3),
                column(6, actionButton("CheckScale", "Check")),
                column(3, actionButton("next1Creation", p(icon("arrow-circle-right"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                br(),
                br(),
                br(),
                div("Press the Check Button to check the scale", style = "padding: 10px; background-color: #e7e7e7")
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 1){
              div(
                h4("Step 1: Draw growth axis"),
                br(),
                column(3, actionButton("next0Creation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(6, actionButton("drawAxisCreation", "Draw")),
                column(3),
                br(),
                br(),
                br(),
                div("Draw the growth axis  by clicking on the image for set the point of the axis; Be careful to draw the growth axis from the core to the edge of the otolith", style = "padding: 10px; background-color: #e7e7e7")
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 2 & length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord) >= 2){

              if(zoom2$temp %% 2 == 0){
                div(
                  h4("Step 1: Draw growth axis"),
                  br(),
                  column(2),
                  column(8,
                         actionButton("zoom2", "Zoom"),
                         actionButton("endAxisCreation", "Finish!")
                  ),
                  column(2),
                  br(),
                  br(),
                  br(),
                  div("Draw the growth axis  by clicking on the image for set the point of the axis; Be careful to draw the growth axis from the core to the edge of the otolith", style = "padding: 10px; background-color: #e7e7e7")
                )
              } else {
                  div(
                    h4("Step 1: Draw growth axis"),
                    br(),
                    column(2),
                    column(8,
                           actionButton("return2", "Return to analyis"),
                           actionButton("endAxisCreation", "Finish!")
                    ),
                    column(2),
                    br(),
                    br(),
                    br(),
                    div("Draw the growth axis  by clicking on the image for set the point of the axis; Be careful to draw the growth axis from the core to the edge of the otolith", style = "padding: 10px; background-color: #e7e7e7")
                  )
              }
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 2 & length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord) < 2){

              if(zoom2$temp %% 2 == 0){
                div(
                  h4("Step 1: Draw growth axis"),
                  br(),
                  column(2),
                  column(8,
                         actionButton("zoom2", "Zoom")
                  ),
                  column(2),
                  br(),
                  br(),
                  br(),
                  div("Draw the growth axis  by clicking on the image for set the point of the axis; Be careful to draw the growth axis from the core to the edge of the otolith", style = "padding: 10px; background-color: #e7e7e7")
                )
              } else {
                div(
                  h4("Step 1: Draw growth axis"),
                  br(),
                  column(2),
                  column(8,
                         actionButton("return2", "Return to analyis")
                  ),
                  column(2),
                  br(),
                  br(),
                  br(),
                  div("Draw the growth axis  by clicking on the image for set the point of the axis; Be careful to draw the growth axis from the core to the edge of the otolith", style = "padding: 10px; background-color: #e7e7e7")
                )
              }

            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 3){
              div(
                h4("Step 1: Draw growth axis"),
                br(),
                column(3),
                column(6, actionButton("correctAxisCreation", "Correct Axis")),
                column(3, actionButton("next2Creation", p(icon("arrow-circle-right"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                br(),
                br(),
                br(),
                div("Click on the Correct button to delete this axis and draw a new one", style = "padding: 10px; background-color: #e7e7e7")
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 4){
              div(
                h4("Step 2: Set growth increments"),
                br(),
                column(3, actionButton("next3Creation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(6, actionButton("setPoints", "Set Increments")),
                column(3),
                br(),
                br(),
                br(),
                div("Set the growth increments by clicking on the image", style = "padding: 10px; background-color: #e7e7e7")
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 5){

              if(zoom3$temp %% 2 == 0){
                div(
                  h4("Step 2: Set growth increments"),
                  br(),
                  column(6,  actionButton("zoom3", "Zoom")),
                  column(6, actionButton(inputId = "endSetPoints", "Finish!")),
                  column(3),
                  br(),
                  br(),
                  br(),
                  div("Set the growth increments by clicking on the image", style = "padding: 10px; background-color: #e7e7e7"),
                  br(),
                  tableOutput("tableFinal")
                )
              } else {
                div(
                  h4("Step 2: Set growth increments"),
                  br(),
                  column(6,  actionButton("return3", "Return to analysis")),
                  column(6, actionButton(inputId = "endSetPoints", "Finish!")),
                  column(3),
                  br(),
                  br(),
                  br(),
                  div("Set the growth increments by clicking on the image", style = "padding: 10px; background-color: #e7e7e7"),
                  br(),
                  tableOutput("tableFinal")
                )
              }


            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 5.5){

              if(zoom3$temp %% 2 == 0){
                div(
                  h4("Step 2: Set growth increments"),
                  br(),
                  column(2, actionButton("add", "A")),
                  column(2, actionButton("remove", "R")),
                  column(2, actionButton("move", "M")),
                  column(3, actionButton("zoom3", "Zoom")),
                  column(2, actionButton("finishcorrect", "Valid")),
                  br(),
                  br(),
                  br(),
                  div("Set the growth increments by clicking on the image; A for adding a new point; R for removing a point; M for moving a point; return to get back to the point settings", style = "padding: 10px; background-color: #e7e7e7"),
                  br(),
                  tableOutput("tableFinal")
                )
              } else {
                div(
                  h4("Step 2: Set growth increments"),
                  br(),
                  column(2, actionButton("add", "A")),
                  column(2, actionButton("remove", "R")),
                  column(2, actionButton("move", "M")),
                  column(3, actionButton("return3", "Return")),
                  column(2, actionButton("finishcorrect", "Valid")),
                  br(),
                  br(),
                  br(),
                  div("Set the growth increments by clicking on the image; A for adding a new point; R for removing a point; M for moving a point; return to get back to the point settings", style = "padding: 10px; background-color: #e7e7e7"),
                  br(),
                  tableOutput("tableFinal")
                )
              }


            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 6){
              div(
                h4("Step 2: Set growth increments"),
                br(),
                column(3, actionButton("next4Creation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(6, actionButton("correctPoints", "Correct Increments")),
                column(3, actionButton("next5Creation", p(icon("arrow-circle-right"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                br(),
                br(),
                br(),
                div("Set the growth increments by clicking on the image", style = "padding: 10px; background-color: #e7e7e7"),
                br(),
                tableOutput("tableFinal")
              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 6.5){
              div(
                h4("Step 3: Edge status"),
                br(),
                column(3, actionButton("next6Creation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(6, div(radioButtons("edge", "", choices = c("Translucent", "Opaque"), selected = "Opaque"), style="margin-top:-20px") ),
                column(3, actionButton("next7Creation", p(icon("arrow-circle-right"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                br(),
                br(),
                br(),
                div("Indicate if there is an increment on the edge", style = "padding: 10px; background-color: #e7e7e7"),
                br(),
                tableOutput("tableFinal")

              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 7){
              div(
                h4("Step 4: Save analysis"),
                br(),
                column(2, actionButton("next8Creation", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(6, actionButton("SaveAnalysis", "Save")),
                br(),
                br(),
                br(),
                div("Save this analysis; Saving this analysis does not mean that you export these data: be careful to export the project", style = "padding: 10px; background-color: #e7e7e7"),
                br(),
                tableOutput("tableFinal")

              )
            } else if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag == 8){
              div(
                h4("Otolith analyzed/Analysis saved"),
                br(),
                column(6, actionButton("deleteAnalysis", "Delete this analysis")),
                br(),
                br(),
                br(),
                div("Save this analysis; Saving this analysis does not mean that you export these data: be careful to export the project", style = "padding: 10px; background-color: #e7e7e7"),
                br(),
                tableOutput("tableFinal")
              )
            } else {}

          } else {
            NULL
          }
        }) #arrows

        output$box <- renderUI({
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                        width = 400, height = "auto",
                        column(8, h2("Otolith analysis")),
                        column(4, br(),uiOutput("ex")),
                        br(),
                        br(),
                        br(),
                        br(),
                        uiOutput("selectSample")
          )
        }) #absolutePanel

        output$tableFinal <- renderTable({

          input$plot_click
          input$next4Creation
          input$selectOtolith

          if(is.null(unlist(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis))){

            if(!is.null(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)){
              distance <- sum(sapply(1:(length(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)-1), function(x){
                tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$calculDistance(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x]], tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord[[x+1]])
              }))
              mat <- matrix(c("total.length", round(distance*tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$DistScale/tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$NbPixelScale*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)],2)), ncol = 2)

              colnames(mat) <- c("#Increment", paste0("distanceFromNucleus_", tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$unitScale))

              table$temp <- mat
              return(table$temp)
            } else { NULL}




          } else {
            table$temp
          }

        },include.rownames=FALSE)

        observe({
          if(!is.null(input$zoom)){
            if(input$zoom > 0){
              isolate({
                zoom$temp <- zoom$temp + 1
              })

            }
          }
        })

        observe({
          if(!is.null(input$return)){
            if(input$return > 0){
              isolate({
                zoom$temp <- zoom$temp + 1
              })

            }
          }
        })

        observe({
          if(!is.null(input$zoom2)){
            if(input$zoom2 > 0){
              isolate({
                zoom2$temp <- zoom2$temp + 1
              })

            }
          }
        })

        observe({
          if(!is.null(input$return2)){
            if(input$return2 > 0){
              isolate({
                zoom2$temp <- zoom2$temp + 1
              })

            }
          }
        })

        observe({
          if(!is.null(input$zoom3)){
            if(input$zoom3 > 0){
              isolate({
                zoom3$temp <- zoom3$temp + 1
              })

            }
          }
        })

        observe({
          if(!is.null(input$return3)){
            if(input$return3 > 0){
              isolate({
                zoom3$temp <- zoom3$temp + 1
              })

            }
          }
        })

        #### nextCreation Image processig ####
        {
          # next0Creation
          observe({
            if(!is.null(input$next0Creation)){
              if(input$next0Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 0
                })
              }
            }
          })

          # next1Creation
          observe({
            if(!is.null(input$next1Creation)){
              if(input$next1Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 1
                })
              }
            }
          })

          # next2Creation
          observe({
            if(!is.null(input$next2Creation)){
              if(input$next2Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 4
                })
              }
            }
          })

          # next3Creation
          observe({
            if(!is.null(input$next3Creation)){
              if(input$next3Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 3
                })
              }
            }
          })

          # next4Creation
          observe({
            if(!is.null(input$next4Creation)){
              if(input$next4Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 4
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis <- NULL
                  incrementPoint$temp <- NULL
                })
              }
            }
          })

          # next5Creation
          observe({
            if(!is.null(input$next5Creation)){
              if(input$next5Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 6.5
                })
              }
            }
          })

          # next6Creation
          observe({
            if(!is.null(input$next6Creation)){
              if(input$next6Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 6
                })
              }
            }
          })

          # next7Creation
          observe({
            if(!is.null(input$next7Creation)){
              if(input$next7Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 7
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$edge <- input$edge
                })
              }
            }
          })

          # next8Creation
          observe({
            if(!is.null(input$next8Creation)){
              if(input$next8Creation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 6
                })
              }
            }
          })

        }

        #### nextCreation check/define scale ####
        {
          # next1CheckCreation
          observe({
            if(!is.null(input$next1CheckCreation)){
              if(input$next1CheckCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.6
                })
              }
            }
          })

          # next2CheckCreation
          observe({
            if(!is.null(input$next2CheckCreation)){
              if(input$next2CheckCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.7
                })
              }
            }
          })

          # next3CheckCreation
          observe({
            if(!is.null(input$next3CheckCreation)){
              if(input$next3CheckCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.2
                })
              }
            }
          })

          # next4CheckCreation
          observe({
            if(!is.null(input$next4CheckCreation)){
              if(input$next4CheckCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.1
                })
              }
            }
          })

          # next5CheckCreation
          observe({
            if(!is.null(input$next5CheckCreation)){
              if(input$next5CheckCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.5
                })
              }
            }
          })

        }

        ###### creation analysis ######
        {
          # drawAxisCreation
          observe({
            if(!is.null(input$drawAxisCreation)){
              if(input$drawAxisCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 2
                })
              }
            }
          })

          # endAxisCreation
          observe({
            if(!is.null(input$endAxisCreation)){
              if(input$endAxisCreation > 0) {
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 3
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setAxisCoord(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setAxisFunction(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord)
                })
              }
            }
          })

          # correctAxisCreation
          observe({
            if(!is.null(input$correctAxisCreation)){
              if(input$correctAxisCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 1
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setAxisCoord(NULL)
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord <- NULL
                })
              }
            }
          })

          #setPoints
          observe({
            if(!is.null(input$setPoints)){
              if(input$setPoints > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 5
                })
              }
            }
          })

          # endSetPoints
          observe({
            if(!is.null(input$endSetPoints)){
              if(input$endSetPoints > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 6
                })
              }
            }
          })

          #correctPoint
          observe({
            if(!is.null(input$correctPoints)){
              if(input$correctPoints > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 5.5
                })
              }
            }
          })

          #finish correct
          observe({
            if(!is.null(input$finishcorrect)){
              if(input$finishcorrect > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 6
                })
              }
            }
          })

          #add
          observe({
            if(!is.null(input$add)){
              if(input$add > 0){
                isolate({
                  correction$temp <- c(1,0,0)
                })
              }
            }
          })

          #remove
          observe({
            if(!is.null(input$remove)){
              if(input$remove > 0){
                isolate({
                  correction$temp <- c(0,1,0)
                })
              }
            }
          })

          # move
          observe({
            if(!is.null(input$move)){
              if(input$move > 0){
                isolate({
                  correction$temp <- c(0,0,1)
                })
              }
            }
          })

          #checkScale
          observe({
            if(!is.null(input$CheckScale)){
              if(input$CheckScale > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -1
                })
              }
            }
          })

          #SaveAnalysis
          observe({
            if(!is.null(input$SaveAnalysis)){
              if(input$SaveAnalysis > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 8
                  tempProj$temp$flagDone[which(tempProj$temp$otolith == input$selectOtolith)] <- 1
                })
              }
            }
          })

          #deleteAnalysis
          observe({
            if(!is.null(input$deleteAnalysis)){
              if(input$deleteAnalysis > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisCoord <- NULL
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$axisFunction <- NULL
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPoint <- NULL
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementPointAxis <- NULL
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$IncrementFinal <- NULL
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinalPixel <- NULL
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$distanceFinal <- NULL


                  tempProj$temp$flagDone[which(tempProj$temp$otolith == input$selectOtolith)] <- 0

                  incrementPoint$temp <- list()
                  idCreation$temp  <- 0
                  table$temp <- NULL
                  correction$temp <- c(0,0,0)
                  toCorrect$temp <- NULL
                  checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]] <- list()
                  scaleValueCreation$temp <- NULL

                  zoom$temp <- 0
                  zoom2$temp  <- 0
                  zoom3$temp  <- 0
                  previous$temp <-  0

                  if(tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$scale == "custom"){
                    tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -1
                  } else {
                    tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 0
                  }

                })
              }
            }
          })


        }

        ###### check scale ######
        {
          # drawAxisCheckCreation
          observe({
            if(!is.null(input$drawAxisCheckCreation)){
              if(input$drawAxisCheckCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.8
                })
              }
            }
          })

          observe({
            input$plot_click
            if(!is.null(tempProj$temp$otolith)){
              if(!is.null(input$selectOtolith)){
                if((which(tempProj$temp$otolith == input$selectOtolith)) <= length(checkScale$temp)){
                  if(!is.null(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]])){
                    if(length(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]]) == 2){
                      isolate({
                        tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.7
                      })
                    }
                  }
                }

              }
            } else {}
          })

          # correctAxisCheckCreation
          observe({
            if(!is.null(input$correctAxisCheckCreation)){
              if(input$correctAxisCheckCreation > 0){
                isolate({
                  checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]] <- list()
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -1
                })
              }
            }
          })


          observe({
            if(!is.null(input$textCreation)){
              if(is.numeric(input$textCreation) | !is.na(as.numeric(input$textCreation))){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.5
                  scaleValueCreation$temp <- input$textCreation
                  updateTextInput(session, "textCreation", value = scaleValueCreation$temp)

                })

              } else {
                tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- -0.6
                  updateTextInput(session, "textCreation", value = "")

              }
            }

          })

          #ValidCheckCreation
          observe({
            if(!is.null(input$ValidCheckCreation)){
              if(input$ValidCheckCreation > 0){
                isolate({
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$flag <- 1
                  toSave <- calculDistance(checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[1]], checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]][[2]])
                  checkScale$temp[[which(tempProj$temp$otolith == input$selectOtolith)]] <- list()
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$scale <- "custom"
                  tempProj$temp$data[[which(tempProj$temp$otolith == input$selectOtolith)]]$setScale(name = "custom", pix = toSave*facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)], dist = as.numeric(as.character(scaleValueCreation$temp)), unit=input$selectScaleCreation)
                  # facMult$temp[which(tempProj$temp$otolith == input$selectOtolith)] <- 1
                })
              }
            }
          })

        }

        }

      ########################
      ### Scale creation   ###
      ########################
      {
        flagScaling <- reactiveValues(temp = 0)
        scalePoint <- reactiveValues(temp = list(c(NULL, NULL), c(NULL, NULL)))
        id <- reactiveValues(temp = 1)
        scaleValue <- reactiveValues(temp = 0)

        zoomScale <- reactiveValues(temp = 0)

        facMultScale <- reactiveValues(temp = NULL)

        observe({
          input$changeCreationProject
          if(!is.null(currentProject())){

            if(flagCreationScale$temp == 1){

              im$temp <- currentProject()$data

              imWidth$temp <- dim(im$temp)[1]
              imHeight$temp <- dim(im$temp)[2]

              ranges$x <- c(0, imWidth$temp)
              ranges$y <- c(imHeight$temp,0)

              resWidth$temp <- input$GetScreenWidth
              resHeight$temp <- input$GetScreenHeight - 100

              isolate({
                ratio <- imWidth$temp/imHeight$temp

                if(imWidth$temp < imHeight$temp){
                  plotWidth$temp <- resWidth$temp
                  plotHeight$temp <- plotWidth$temp / ratio

                  facMultScale$temp <- resWidth$temp/imWidth$temp

                } else{
                  plotHeight$temp <- resHeight$temp
                  plotWidth$temp <- plotHeight$temp * ratio

                  facMultScale$temp <- resHeight$temp/imHeight$temp
                }
              })
            } else {}
          }



        }) # define image and size of the picture

        observe({
          input$changeCreationProject
          if(!is.null(currentProject())){

            if(flagCreationScale$temp == 1){

            if(!is.null(ranges$x)){


              output$plotScale <- renderPlot({

                par(mar = c(0,0,0,0), bty = "n")

                plot(im$temp, xlim = ranges$x, ylim = ranges$y, axes = T)


              }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

            } else {}

            } else {}
          } else  {}
        }) # define plotScale

        observe({
          input$changeCreationProject
          if(!is.null(currentProject())){

            if(flagCreationScale$temp == 1){

            if(!is.null(ranges$x)){

              if(is.null(scalePoint$temp[[1]][1])){

                output$plotScale2 <- renderPlot({

                  par(mar = c(0,0,0,0), bty = "n")

                  plot(0,0, xlim = ranges$x, ylim = ranges$y, axes = T)

                }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

              } else if(is.null(scalePoint$temp[[2]][1])){

                output$plotScale2 <- renderPlot({

                  par(mar = c(0,0,0,0), bty = "n")

                  plot(0,0, xlim = ranges$x, ylim = ranges$y, axes = T)

                  lapply(1:length(scalePoint$temp), function(x){
                    points(scalePoint$temp[[x]][1], scalePoint$temp[[x]][2], cex = 2, col= "red", pch = 16)
                  })


                }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")

              } else {

                output$plotScale2 <- renderPlot({

                  par(mar = c(0,0,0,0), bty = "n")

                  plot(0,0, xlim = ranges$x, ylim = ranges$y, axes = T)

                  lapply(1:length(scalePoint$temp), function(x){
                    points(scalePoint$temp[[x]][1], scalePoint$temp[[x]][2], cex = 2, col= "red", pch = 16)
                  })

                  segments(scalePoint$temp[[1]][1], scalePoint$temp[[1]][2], scalePoint$temp[[2]][1], scalePoint$temp[[2]][2], cex = 1.5, col = "red")


                }, width = plotWidth$temp, height = plotHeight$temp, bg = "transparent")
              }


            } else {}

            } else {
              output$plotScale2 <- renderPlot({NULL})
            }

          }else {}
        }) # define plotScale2

        output$PLOTSCALING <- renderUI({

          plotOutput("plotScale", height = "100%")

        })

        output$PLOTSCALING2 <- renderUI({

          zoomScale$temp

          if(zoomScale$temp %% 2 == 0){
            div(plotOutput("plotScale2",
                           click = "plotScale_click",
                           dblclick = "plotScale_dblclick",
                           hover = "plotScale_hover",
                           brush = brushOpts(
                             id = "plotScale_brush",
                             resetOnNew = TRUE
                           ), height = "100%"), style = paste0("margin-top: -", plotHeight$temp ,"px"))

          } else {
            div(plotOutput("plotScale2",
                           dblclick = "plotScale_dblclick",
                           hover = "plotScale_hover",
                           brush = brushOpts(
                             id = "plotScale_brush",
                             resetOnNew = TRUE
                           ), height = "100%"), style = paste0("margin-top: -", plotHeight$temp ,"px"))
          }


        })

        observeEvent(input$plotScale_dblclick, {
          brush <- input$plotScale_brush
          if (!is.null(brush)) {
            if(brush$xmin != brush$xmax & brush$ymax != brush$ymin){
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymax, brush$ymin)

            isolate({

              h <- brush$ymax - brush$ymin
              w <- brush$xmax - brush$xmin

              W <- resWidth$temp - 200
              H <- resHeight$temp - 150

              height <- H/h
              width <- W/w

              min <- min(height,width)

              plotWidth$temp <- min * w
              plotHeight$temp <- min * h
            })
            }

          } else {
            ranges$x <- c(0, dim(im$temp)[1])
            ranges$y <- c(dim(im$temp)[2], 0)

            isolate({
              ratio <- imWidth$temp/imHeight$temp

              if(imWidth$temp < imHeight$temp){
                plotWidth$temp <- resWidth$temp
                plotHeight$temp <- plotWidth$temp / ratio
              } else{
                plotHeight$temp <- resHeight$temp
                plotWidth$temp <- plotHeight$temp * ratio
              }
            })

          }
        }) #zoom

        observeEvent(input$plotScale_click, {
          if(flagScaling$temp == 1){
            scalePoint$temp[[id$temp]] <- c(input$plotScale_click$x, input$plotScale_click$y)
            id$temp <- id$temp + 1
          }
        }) #double clik

        output$boxScaling <- renderUI({
          absolutePanel(id = "controlsScaling", class = "panel panel-default", fixed = TRUE,
                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                        width = 370, height = "auto",

                        h2("Scale creation"),
                        br(),
                        uiOutput("wayScaling")
          )
        }) #absolutePanel

        output$wayScaling <- renderUI({

          input$zoomScale
          input$changeCreationProject

          if(flagCreationScale$temp == 1){
            if(flagScaling$temp == 0){
              div(
                h4("Define scale"),
                br(),
                column(2),
                column(8, actionButton("drawAxis", "Draw axis")),
                column(2),
                br(),
                br(),
                br(),
                div("Point the edges of the scale", style = "padding: 10px; background-color: #e7e7e7")
              )
              } else if(flagScaling$temp == 1){

                if(zoomScale$temp %% 2 == 0){
                  div(
                    h4("Define scale"),
                    br(),
                    column(2),
                    column(8, actionButton("zoomScale", "Zoom")),
                    column(2),
                    br(),
                    br(),
                    br(),
                    div("Point the edges of the scale", style = "padding: 10px; background-color: #e7e7e7")
                  )
                } else {
                  div(
                    h4("Define scale"),
                    br(),
                    column(2),
                    column(8, actionButton("returnScale", "Return to analysis")),
                    column(2),
                    br(),
                    br(),
                    br(),
                    div("Point the edges of the scale", style = "padding: 10px; background-color: #e7e7e7")
                  )
                }


            } else if(flagScaling$temp == 2){
              div(
                h4("Define scale"),
                br(),
                column(2),
                column(8, actionButton("correctAxis", "Correct Scale")),
                column(2, actionButton("next1", p(icon("arrow-circle-right"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                br()
              )
            } else if(flagScaling$temp == 3){
              div(
                h4("Define the value of the scale"),
                br(),
                column(2, actionButton("next2", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(4, textInput("text", label = "Scale value", value = "")),
                column(4, selectInput("selectScale", label = "unit",
                                      choices = c("µm", "mm", "cm", "m"),
                                      selected = "µm")),
                column(2)
              )
            } else if(flagScaling$temp == 4){
              div(
                h4("Define the value of the scale"),
                br(),
                column(2, actionButton("next3", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(4, textInput("text2", label = "Scale value", value = as.numeric(scaleValue$temp))),
                column(4, selectInput("selectScale", label = "unit",
                                      choices = c("µm", "mm", "cm", "m"),
                                      selected = "µm")),
                column(2, actionButton("next4", p(icon("arrow-circle-right"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px"))
              )
            } else if(flagScaling$temp == 5){
              div(
                column(2, actionButton("next5", p(icon("arrow-circle-left"), "", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 1px")),
                column(10,
                       column(6, textInput("name", label = "", value = "Name ..."), style = "margin-top: -20px"),
                       column(6, actionButton("exportScale", "Export Scale"))
                )
              )
            } else if(flagScaling$temp == 6){
              div(
                h4("Scale analyzed/Analysis exported"),
                br(),
                column(6, actionButton("deleteScale", "Delete this Scale"))
              )
            } else {}

          } else {
            NULL
          }



        }) #arrows

        observe({
          if(!is.null(input$zoomScale)){
            if(input$zoomScale > 0){
              isolate({
                zoomScale$temp <- zoomScale$temp + 1
              })

            }
          }
        })

        observe({
          if(!is.null(input$exportScale)){
            if(input$exportScale > 0){
              isolate({
                flagScaling$temp <- 6
              })

            }
          }
        })

        #deleteScale
        observe({
          if(!is.null(input$deleteScale)){
            if(input$deleteScale > 0){
              isolate({
                flagScaling$temp <- 0

                tem <- str_split(currentProject()$fPath,  pattern = "/")

                chemin <- paste0(paste0(tem[[1]][1:(length(tem[[1]])-1)], collapse = "/"), "/")

                file.remove(file = paste0(chemin, currentProject()$name, ".RData"))
              })

            }
          }
        })

        observe({
          if(!is.null(input$returnScale)){
            if(input$returnScale > 0){
              isolate({
                zoomScale$temp <- zoomScale$temp + 1
              })

            }
          }
        })

        #drawAxis
        observe({
          if(!is.null(input$drawAxis)){
            if(input$drawAxis > 0){
              isolate({
                flagScaling$temp <- 1
              })

            }
          }
        })

        observe({
          if(length(unlist(scalePoint$temp)) == 4){
            isolate({
              flagScaling$temp <- 2
            })
          }
        })

        observe({
          if(!is.null(input$next1)){
            if(input$next1 > 0){
              isolate({
                flagScaling$temp <- 3
              })
            }
          }
        })

        observe({
          if(!is.null(input$next2)){
            if(input$next2 > 0){
              isolate({
                flagScaling$temp <- 2
              })
            }
          }
        })

        observe({
          if(!is.null(input$correctAxis)){
            if(input$correctAxis > 0){
              isolate({
                flagScaling$temp <- 1
                scalePoint$temp <- list(c(NULL, NULL), c(NULL, NULL))
                id$temp <- 1
              })
            }
          }
        })

        observe({
          if(!is.null(input$text)){
            if(is.numeric(input$text) | !is.na(as.numeric(input$text))){
              isolate({
                flagScaling$temp <- 4
                scaleValue$temp <- as.numeric(input$text)
              })
            } else {
              isolate({
                flagScaling$temp <- 3
              })
            }
          }
        })

        observe({
          if(!is.null(input$text2)){
            if(is.numeric(input$text2) | !is.na(as.numeric(input$text2))){
              isolate({
                flagScaling$temp <- 4
                scaleValue$temp <- as.numeric(input$text2)
              })
            } else {
              isolate({
                flagScaling$temp <- 3
              })
            }
          }
        })

        observe({
          if(!is.null(input$next3)){
            if(input$next3 > 0){
              isolate({
                flagScaling$temp <- 2
              })
            }
          }
        })

        observe({
          if(!is.null(input$next4)){
            if(input$next4 > 0){
              isolate({
                flagScaling$temp <- 5
              })
            }
          }
        })

        observe({
          if(!is.null(input$next5)){
            if(input$next5 > 0){
              isolate({
                flagScaling$temp <- 4
              })
            }
          }
        })

        #exportScale
        observe({
          if(!is.null(input$exportScale) & !is.null(input$name)){
            if(input$exportScale > 0 & input$name != "Name ..."){
              isolate({
                distance <- calculDistance(scalePoint$temp[[1]], scalePoint$temp[[2]])
                currentProject()$setScalePixel(distance*facMultScale$temp)
                currentProject()$setScaleValue(scaleValue$temp)
                currentProject()$setName(input$name)
                currentProject()$setUnit(input$selectScale)
                myProject <- currentProject()


                if(Sys.info()[1] == "Windows"){

                  tem <- str_split(currentProject()$fPath,  pattern = "\\\\")

                  chemin <- paste0(paste0(tem[[1]][1:(length(tem[[1]])-1)], collapse = "\\"), "\\")

                  save(myProject, file = paste0(chemin, input$name, ".RData"))

                } else  {

                  tem <- str_split(currentProject()$fPath,  pattern = "/")

                  chemin <- paste0(paste0(tem[[1]][1:(length(tem[[1]])-2)], collapse = "/"), "/")
                  
                  save(myProject, file = paste0(chemin, input$name, ".RData"))

                }


              })
            }
          }
        })

      }

      ########################
      ### Project Export   ###
      ########################
      {

        output$ex <- renderUI({

          input$createProjectButton

          if(!is.null(tempProj$temp)){

            actionButton("exportProject", "Export Project")

          } else {

          }

        })

        observe({
          if(!is.null(input$exportProject)){
            if(input$exportProject > 0){
              isolate({

                pb <- tkProgressBar("Progress bar", "Some information in %",
                                    0, 100, 20)

                info <- sprintf("%d%% done", round(10))
                setTkProgressBar(pb, 10, sprintf("Data export (%s)", info), info)


                wd <- projPath$temp

                setwd(wd)

                temp <- tempProj$temp

                # For Christian
                # save(temp, file = "Result.RData")

                info <- sprintf("%d%% done", round(50))
                setTkProgressBar(pb, 50, sprintf("Data export (%s)", info), info)

                print(facMult$temp)
                
                tempProj$temp$setFinalTable(facMult = facMult$temp)

                write.csv(tempProj$temp$tabFinalDistance,file = paste0("SummarySessionDistance.csv"))
                write.csv(tempProj$temp$tabFinalWidth,file = paste0("SummarySessionWidth.csv"))

                info <- sprintf("%d%% done", round(70))
                setTkProgressBar(pb, 70, sprintf("Data loading (%s)", info), info)

                lapply(1:length(tempProj$temp$otolith), function(x){

                  if(tempProj$temp$flagDone[x] == 1){

                    setwd(tempProj$temp$data[[x]]$fPath)

                    a <- str_split(tempProj$temp$data[[x]]$name, pattern = "/")[[1]][length(str_split(tempProj$temp$data[[x]]$name, pattern = "/")[[1]])]
                    b <- str_split(a, pattern = tempProj$temp$data[[x]]$format)[[1]][1]

                    write.csv(tempProj$temp$data[[x]]$distanceFinal,file = paste0("finalDistance",b,".csv"))
                    write.csv(tempProj$temp$data[[x]]$widthIncrement,file = paste0("finalWidth",b,".csv"))

                    jpeg(filename = paste0("finalDistance",b,".jpeg"), width = imWidth$temp, height = imHeight$temp)

                    image <- tempProj$temp$data[[x]]$data

                      xLim <- c(0, dim(image)[1])
                      yLim <- c(dim(image)[2],0)

                      plot(image, xlim = xLim, ylim = yLim, axes = T)

                      par(new = T)

                      plot(200, 200, xlim = xLim, ylim = yLim, axes = T, pch = 16, col = "white", type = "n")

                    points(tempProj$temp$data[[x]]$axisCoord[[1]][1], tempProj$temp$data[[x]]$axisCoord[[1]][2], cex = 1, col= "yellow", pch = 16)

                    t <- length(tempProj$temp$data[[x]]$axisCoord)
                    points(tempProj$temp$data[[x]]$axisCoord[[t]][1], tempProj$temp$data[[x]]$axisCoord[[t]][2], cex = 1, col= "blue", pch = 16)

                    lapply(2:length(tempProj$temp$data[[x]]$axisCoord), function(i){
                      segments(tempProj$temp$data[[x]]$axisCoord[[i]][1], tempProj$temp$data[[x]]$axisCoord[[i]][2], tempProj$temp$data[[x]]$axisCoord[[i-1]][1], tempProj$temp$data[[x]]$axisCoord[[i-1]][2], cex = 1.5, col = "red")
                    })

                    if(!is.null(tempProj$temp$data[[x]]$IncrementPointAxis)){
                      if(length(tempProj$temp$data[[x]]$IncrementPointAxis) != 0){

                        lapply(1:length(tempProj$temp$data[[x]]$IncrementPointAxis), function(i){
                          points(tempProj$temp$data[[x]]$IncrementPointAxis[[i]][1], tempProj$temp$data[[x]]$IncrementPointAxis[[i]][2], cex = 1, col= "red", pch = 16)
                        })

                      }
                    }

                    legend("topleft", legend = c("Core", "Increment", "Edge"), pch = 16, col = c("yellow", "red", "blue"), bty = "o", bg = "white")

                    dev.off()

                  } else {

                    setwd(tempProj$temp$data[[x]]$fPath)

                    temp <- list.files(pattern = "finalDistance")

                    lapply(1:length(temp), function(i){
                      file.remove(temp[i])
                    })
                  }


                })


                info <- sprintf("%d%% done", round(100))
                setTkProgressBar(pb, 100, sprintf("Data loading (%s)", info), info)

                close(pb)

              })
            }
          }
        })

      }

    }
  ), launch.browser = TRUE)



}

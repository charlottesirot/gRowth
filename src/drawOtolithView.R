drawOtolithView = function(){
  fluidRow(
    uiOutput("MainPlot"),
    uiOutput("ProcessedPlot"),
    uiOutput("Guide"))
}


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
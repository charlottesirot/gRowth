navbarPage("gRowth", id = "nav", 
           tabPanel("Project Settings",
                    uiOutput("ProjectView")),
           tabPanel("Otolith Processing",
                    uiOutput("OtholitProcessingView")),
           tabPanel("Scale Creation",
                    uiOutput("ScaleCreationView"))
)


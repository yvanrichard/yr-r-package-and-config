library(shiny)

shinyUI(
    fluidPage(
        sidebarLayout(
            sidebarPanel(width = 3,
                         uiOutput("xvar"),
                         uiOutput("xtrans"),
                         hr(),
                         uiOutput("yvar"),
                         uiOutput("ytrans"),
                         hr(),
                         uiOutput("groupvar"),
                         uiOutput("panelvar")),
            mainPanel(width = 9,
                      plotOutput("theplot", height = '800px'))
        )
    )
)

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(flexdashboard)
library(shinythemes)
require(pracma)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"
    )),
  fluidRow(
    h3("AEB: CAPACIDAD DIAGNÓSTICA Y VALOR PREDICTIVO", style="text-align:center"),
    sidebarLayout(
      sidebarPanel(width=3,
        wellPanel(style= "background: #CDDCFE",
         sliderInput(
           "h.m",
           "Media sanos",
           min = 10,
           max = 100,
           value = 30,
           ticks=F
           ),
         sliderInput(
           "h.sd",
           "Desv. std. sanos",
           min = 1,
           max = 20,
           value = 8,
           ticks=F
          )
        ),
        wellPanel(style= "background: #FEDCCD",
         sliderInput(
           "d.m",
           "Media enfermos",
           min = 10,
           max = 100,
           value = 50,
           ticks=F
         ),
         sliderInput(
           "d.sd",
           "Desv. std Enfermos",
           min = 1,
           max = 20,
           value = 8,
           ticks=F
         )
        ),
       wellPanel(
         sliderInput("p.c",
           "Punto de corte",
           min = 15,
           max = 75,
           value = 40,
           step=0.1,
           ticks=F
         ),
         sliderInput("prev",
           "Prevalencia(%)",
           min = 0.1,
           max = 100,
           value = 50,
           step=0.1,
           ticks=F
          )
         )
      ),
    
    mainPanel(width=9,style="padding:10px;",
      # Gauges
      fluidRow(class='grow',
         column(4,title="Sensibilidad VP/(VP+FN)",
           gaugeOutput('Sen.g',height = "75%",width="75%"),
           p("Sensibilidad")
          ),
         column(4, title="Especificidad VN/(VN+FP)",
           gaugeOutput('Esp.g',height = "75%",width="75%"),
           p("Especificidad")
         ),
         column(4,title="Eficiencia (VP+VN)/Total",
           gaugeOutput('Efi.g',height = "75%",width="75%"),
           p("Eficiencia")
         )
      ),
      br(),
      fluidRow(class="grow",
         column(4,offset=0,title="Valor predictivo positivo",
           gaugeOutput('ppv.g',height = "75%",width="75%"),
           p("Valor predictivo pos.")
         ),
         column(4,title="Valor predictivo negativo",
           gaugeOutput('npv.g',height = "75%",width="75%"),
           p("Valor predictivo neg.")
         ),
         column(4,style="height:100%;",align='center',
           tableOutput('mytab')
        )
    ),
      #Plots
    tabsetPanel(id='tab',
      tabPanel('Parametric',
       fluidRow(
         column(6,
          plotlyOutput("distPlot")
         ),
         column(6,
          plotlyOutput("senPlot"))
        )
      ),
      tabPanel('NonParametric',
       fluidRow(
         column(6,
          plotlyOutput("distPlot2")
         ),
         column(6,
          plotlyOutput("senPlot2")
          )
         )
        )
       )
      )
     )
    )
   )




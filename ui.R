library(shiny)
library(leaflet)
#library(shinythemes)
library(shinydashboard)
library(dplyr)


header <- dashboardHeader(
  title = "Woodstock project"
)

body <- dashboardBody(
  fluidRow(
    column(width = 5,
           box(width = 300, solidHeader = TRUE,
               leafletOutput("mymap", height = 500)
           )
    ),
    column(width=7,
           fluidRow(infoBox("# Nurseries", nrow(locations), icon=icon("info-circle"))),
           fluidRow(infoBox("# Batches", nrow(treestats), icon=icon("group"))),
           fluidRow(infoBox("# Species", length(unique(treestats$species)), icon=icon("tree")))
    )
  ),
  fluidRow(
    box(DT::dataTableOutput("treestatsdata"))
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

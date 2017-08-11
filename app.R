library(shiny)
library(leaflet)
#library(shinythemes)
library(shinydashboard)
library(dplyr)
locations <- read.csv("data/nursery_locations.csv")
locations$howmany <- paste(locations$nursery, locations$trees, sep=" ")

treestats <- read.csv("data/tree_stats.csv")

treestats_tab <- dplyr::select(treestats, volume, species, nursery, sizeindex) %>%
  dplyr::mutate(sizeindex = round(sizeindex,1),
         species = Hmisc::capitalize(gsub("_"," ", species))) %>%
  dplyr::arrange(species, volume)

server <- function(input, output, session) {
  
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles%>%
      addCircleMarkers(locations[1:nrow(locations),3],
                       locations[1:nrow(locations),2],
                       popup = locations[1:nrow(locations),5],
                       col=c(rep("darkgreen",23),"#990033"),
                       opacity=c(.85),
                       fillColor = c("white"),
                       fillOpacity = 100,
                       weight=3)
    
  })
  
  output$treestatsdata <- DT::renderDataTable({
         datatable(treestats_tab)
  })
  
}


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

ui <- dashboardPage(
  header,
  dashboardSidebar(),
  body
)

shinyApp(ui, server)

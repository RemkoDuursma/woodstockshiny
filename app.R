library(shiny)
library(leaflet)
#library(shinythemes)
library(shinydashboard)
library(dplyr)
library(DT)
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
  title = tags$a(href='https://www.westernsydney.edu.au/hie',
                 tags$img(src='wsu_badge_invert_small.png'),
                  tags$style(HTML('.skin-black .main-header .logo {
                              background-color: #9F2137;
                              }
                              .skin-black .main-header .logo:hover {
                              background-color: #9F2137;
                              }'))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
      fluidRow(
        column(width = 5,
               box(width = 300, solidHeader = TRUE,
                   title = "Locations of nurseries included in the study",
                   footer= "Zoom with +/-, move the map by dragging",
                   leafletOutput("mymap", height = 500)
               )
        ),
        column(width=7,
               fluidRow(box(p("Some text goes here describing what is to be found here. Like iusb siub ciubviub coish siufsv ojsci cdois  codi scoin."))),
               fluidRow(infoBox("# Nurseries", nrow(locations), icon=icon("info-circle"))),
               fluidRow(infoBox("# Batches", nrow(treestats), icon=icon("group"))),
               fluidRow(infoBox("# Species", length(unique(treestats$species)), icon=icon("tree")))
        )
      )),
    tabItem(tabName ="data",
      fluidRow(
        box(solidHeader=TRUE,
            title="Information on all batches sampled.",
            DT::dataTableOutput("treestatsdata"))
      )
    )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "map", icon = icon("map-o")),
    menuItem("Data", icon = icon("database"), tabName = "data")
  )
)

ui <- dashboardPage(  header,
  sidebar,
  body,
  skin="black"
)

shinyApp(ui, server)

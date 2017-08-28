library(shiny)
library(leaflet)
#library(shinythemes)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggiraph)
library(Hmisc)
library(scales)
library(ggplot2)
locations <- read.csv("data/nursery_locations.csv")
locations$howmany <- paste(locations$nursery, locations$trees, sep=" ")

treestats <- read.csv("data/tree_stats.csv")

treestats_tab <- dplyr::select(treestats, volume, species, nursery, sizeindex) %>%
  dplyr::mutate(sizeindex = round(sizeindex,1),
         species = capitalize(gsub("_"," ", species))) %>%
  dplyr::arrange(species, volume)

server <- function(input, output, session) {
  
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  
  locations$popup <- paste(
    '<strong>Nursery:</strong>', capitalize(as.character(locations$nursery)), '<br>',
    '<strong>Number of trees:</strong>', locations$trees
  )
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles('CartoDB.Positron') %>% #addTiles%>%
      addCircleMarkers(lng = locations[1:nrow(locations),3],
                       lat = locations[1:nrow(locations),2],
                       clusterOptions = markerClusterOptions(),
                       popup = locations$popup,
                       col=c(rep("darkgreen",23),"#990033"),
                       opacity=.85,
                       fillColor = "white",
                       fillOpacity = 100,
                       weight=3)
  })
  
  output$dataplot <- renderggiraph({
    
    si_means <- readRDS("data/simeans.rds")
    
    standard_df <- data.frame(x=c(20,2500,2500,20), y=c(24, 1627, 2393, 37), limit=c("min","min","max","max"),
                              value="Standard", stringsAsFactors = FALSE)
    
    siplot <- ggplot(data=si_means, aes(x=volume, y=sizeindex.mean, tooltip=tooltip, data_id=row.names(si_means))) + 
      scale_x_log10(limits=c(10,5000)) +
      scale_y_log10() +
      geom_polygon_interactive(data=standard_df, aes(x=x, y=y, alpha=0.2, 
                                                     colour="forestgreen", tooltip="AS2303 Standard", data_id="1")) +
      geom_point_interactive(size=2) +
      theme_bw() +
      labs(x="Container volume (L)", y="Size index") +
      theme(legend.position="none")
    
    ggiraph(code = {print(siplot)}, hover_css = "fill:red;", selection_type="none")
    
  })
  
  output$treestatsdata <- DT::renderDataTable({
         datatable(treestats_tab)
  })
  
  info <- eventReactive(input$uploadedfile, {
    

    req(input$uploadedfile)
    
    # Changes in read.table 
    
    fext <- tools::file_ext(input$uploadedfile)
    
    if(fext == "csv"){
      df <- read.csv(input$uploadedfile$datapath)
    } 
    if(fext %in% c("xls","xlsx")){
      df <- readxl::read_excel(input$uploadedfile$datapath)
    }
    
    vars <- names(df)
    
    # Update select input immediately after clicking on the action button. 
    updateSelectInput(session, "container_column","Column with container volume (L):", choices = vars)
    updateSelectInput(session, "calliper_column","Column with calliper (mm):", choices = vars)
    updateSelectInput(session, "height_column","Column with height (m):", choices = vars)
    
    df
  })

  output$table_display <- renderPlot({
    f <- info()
    
    standard_df <- data.frame(x=c(20,2500,2500,20), y=c(24, 1627, 2393, 37), limit=c("min","min","max","max"),
                              value="Standard", stringsAsFactors = FALSE)
    
    vol <- as.numeric(f[, input$container_column])
    diam <- as.numeric(f[, input$calliper_column])
    height <- as.numeric(f[, input$height_column])
    
    with(standard_df, plot(log10(x), log10(y), type='n', 
                           xlab="Container volume (L)",
                           ylab="Size index (height * calliper)",
                           axes=FALSE, 
                           xlim=log10(c(10,5000)),
                           ylim=log10(c(10,2000))))
    magaxis(side=1:2, unlog=1:2)
    with(subset(standard_df, limit == "min"), lines(log10(x), log10(y)))
    with(subset(standard_df, limit == "max"), lines(log10(x), log10(y)))
    
    #points(log10(vol), log10(diam*height), pch=19, col="red")
  })
  
}


  
header <- dashboardHeader(
  title = tags$a(href='https://www.westernsydney.edu.au/hie',
                 tags$img(src='WSU_badge_invert_small.png'),
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
        column(width = 12,
               box(width = 200, solidHeader = TRUE,
                   title = "Locations of nurseries included in the study",
                   footer= "Zoom with +/-, move the map by dragging",
                   leafletOutput("mymap", height = 500)
               ))),
      fluidRow(
        column(width=12, 
                 infoBox("# Nurseries", nrow(locations), icon=icon("info-circle")),
                 infoBox("# Batches", nrow(treestats), icon=icon("group")),
                 infoBox("# Species", length(unique(treestats$species)), icon=icon("tree")))
      )),
    tabItem(tabName ="data",
        box(solidHeader=TRUE, width=12,
            title="Information on all batches sampled.",
            DT::dataTableOutput("treestatsdata"))
    ),
    tabItem(tabName="dataplot",
          fluidRow(box(width=12,
                       p(paste("The plot below shows all sampled batches in the study. The size index",
                               "is calculated as the calliper (diameter of the seedling) times the height.",
                               "The colored box is the current standard."))
                       )),
          fluidRow(
            box(solidHeader=TRUE, width=12, #height=750,
                title="Size index of all sampled batches.",
                footer="Hover over each point to see the species, container volume, and nursery.",
                ggiraphOutput("dataplot")
          ))
    ),
    tabItem(tabName="info",
            fluidRow(
              box(width=12,
                h2("Tree Planting Stock Assessment"),
                p(paste(readLines("data/infotextblock.txt"), collapse="\n")),
                p(strong(paste("Navigate on the left for a map of all sampled locations,",
                        "to browse the raw data, or to view a plot of size index")))
              )
            )
    ),
    
    tabItem(tabName="testdata",
            fluidRow(
              box(width=12,
                  fileInput("uploadedfile", "Choose Excel or CSV File",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv", ".xlsx", ".xls")),
                  selectInput("container_column", "Column with container volume (L):", choices = NULL), # no choices before uploading 
                  selectInput("calliper_column", "Column with calliper (mm):", choices = NULL),
                  selectInput("height_column", "Column with height (m):", choices = NULL),
                  plotOutput("table_display")
              )
              
              
              
            )       
    )        
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Info", tabName = "info", icon=icon("home")),
    menuItem("Map", tabName = "map", icon = icon("map-o")),
    menuItem("Data", icon = icon("database"), tabName = "data"),
    menuItem("Results", icon=icon("bar-chart"), tabName="dataplot"),
    menuItem("Test your data", icon=icon("question-circle"), tabName="testdata", badgeLabel = "new", badgeColor = "green")
  )
)

ui <- dashboardPage(header,
  sidebar,
  body,
  skin="black",
  title="Woodstock Browser"
)

shinyApp(ui, server)

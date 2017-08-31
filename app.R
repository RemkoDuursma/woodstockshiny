source("R/load_packages.R")
source("R/read_data.R")



server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    make_leaflet_map(locations)
  })
  
  output$dataplot <- renderggiraph({
    make_ggiraph_plot(si_means, standard_df)
  })
  
  output$treestatsdata <- DT::renderDataTable({
         datatable(treestats_tab)
  })
  
  info <- eventReactive(input$uploadedfile, {
    
    req(input$uploadedfile)
    
    # Changes in read.table 
    
    fext <- file_ext(input$uploadedfile)
    
    if(fext == "csv"){
      df <- read.csv(input$uploadedfile$datapath)
    } 
    if(fext %in% c("xls","xlsx")){
      df <- as.data.frame(read_excel(input$uploadedfile$datapath))
    }
    
    vars <- names(df)
    
    # Update select input immediately after clicking on the action button. 
    updateSelectInput(session, "container_column","Column with container volume (L):", choices = vars, selected="")
    updateSelectInput(session, "calliper_column","Column with calliper (mm):", choices = vars, selected="")
    updateSelectInput(session, "height_column","Column with height (m):", choices = vars, selected="")
    
    df
  })

  output$table_display <- renderPlot({
    f <- info()
    comparison_standard_plot(input, f, standard_df)
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
                  selectInput("container_column", "Column with container volume (L):", choices = NULL),  
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

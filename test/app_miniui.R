source("R/load_packages.R")
source("R/read_data.R")

library(miniUI)

# Preamble ------------
# header <- dashboardHeader(
#   title = tags$a(href='https://www.westernsydney.edu.au/hie',
#                  tags$img(src='WSU_badge_invert_small.png'),
#                  tags$style(HTML('.skin-black .main-header .logo {
#                               background-color: #9F2137;
#                               }
#                               .skin-black .main-header .logo:hover {
#                               background-color: #9F2137;
#                               }'))
#   )
# )


# ui -------------------------

ui <- miniPage(
  gadgetTitleBar("Treestock AS2303 Root-Shoot Balance"),
  
  miniTabstripPanel(
    miniTabPanel("Info", icon=icon("home"),
                 miniContentPanel(
                   
                   #
                 )
    ),
    miniTabPanel("Enter Data", icon=icon("bar-chart"),
                 miniContentPanel(
                   
                   plotOutput("dataplot")
                 )
    ),
    miniTabPanel("Upload Data", icon=icon("upload"),
                 miniContentPanel(
                   
                   fileInput("uploadedfile", "Choose Excel or CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv", ".xlsx", ".xls")),
                   selectInput("container_column", "Column with container volume (L):", choices = NULL),  
                   selectInput("calliper_column", "Column with calliper (mm):", choices = NULL),
                   selectInput("height_column", "Column with height (m):", choices = NULL),
                   plotOutput("table_display"),
                   downloadButton('downloadPlot', 'Download Plot')
                   
                 )
    ),
    miniTabPanel("Map", icon=icon("map-o"),
                 miniContentPanel(
                   
                   leafletOutput("mymap")
                 )
    )
  )
  
)



# server ----------------------- 

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    make_leaflet_map(locations)
  })
  
  output$dataplot <- renderPlot({
    with(si_means, plot(log10(volume), log10(sizeindex.mean), 
                        xlab="Container volume (L)",
                        ylab="Size index (calliper x height)",
                        axes=FALSE, pch=19, col="cornflowerblue"))
    magaxis(side=1:2, unlog=1:2)
    box()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "woodstockplot.png",
    content = function(file) {
      png(file)
      f <- info()
      comparison_standard_plot(input, f, standard_df)
      dev.off()
    })    
  
  output$treestatsdata <- DT::renderDataTable({
    datatable(treestats_tab)
  })
  
  info <- eventReactive(input$uploadedfile, {
    
    req(input$uploadedfile)
    
    # Changes in read.table 
    
    fext <- file_ext(input$uploadedfile)[1]
    
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



# Run app -------------
runGadget(shinyApp(ui, server), 
          viewer = paneViewer())


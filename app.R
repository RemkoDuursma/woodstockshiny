source("R/load_packages.R")
source("R/read_data.R")


# ui -------------------------

ui <- miniPage(
  gadgetTitleBar("Treestock AS2303", left=NULL, right=NULL),
  
  miniTabstripPanel(
    miniTabPanel("Info", icon=icon("home"),
                 miniContentPanel(
                   
                   h4("Welcome to the new AS2303 Standard."),
                   h4("Manually enter size index data and compare to the national database, or upload a datafile."),
                   h4("View the map of nurseries included in the national study.")
                 )
    ),
    miniTabPanel("Enter Data", icon=icon("bar-chart"),
                 miniContentPanel(
                   useShinyjs(),
                   
                   dropdownButton(
                     tags$h3("Input"),
                     numericInput("volume_entry", label=h3("Container volume (L)"), value=0),
                     numericInput("calliper_entry", label=h3("Calliper (mm)"), value=0),
                     numericInput("height_entry", label=h3("Height (cm)"), value=0),
                     radioGroupButtons("everdeci_entry", label=h3("Type"), 
                                  choices=list("Deciduous" = "deci", "Evergreen" = "ever"),
                                  selected=1),
                     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                     tooltip = tooltipOptions(title = "Click to enter data")
                   ),
                   

                   plotOutput("dataplot"),
                   textOutput("sizeindex_message", container=h2)
                   
                 )
    ),
    miniTabPanel("Upload", icon=icon("upload"),
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
  
  observeEvent(input$volume_entry, {
    toggle("everdeci_entry", !(input$volume_entry >= 100 | input$volume_entry == 0))
  })
  
  output$sizeindex_message <- renderText({
    
    vals <- c(input$volume_entry,input$height_entry,input$calliper_entry)
    if(any(vals == 0)){
      return("")
    } else {
      si <- input$height_entry * input$calliper_entry
      msg <- sizeindex_evaluate(input$volume_entry, si, qf_large)
      return(msg)
    }
    
  })
  
  output$mymap <- renderLeaflet({
    make_leaflet_map(locations)
  })
  
  output$dataplot <- renderPlot({
    with(treestats, plot(log10(volume), log10(si), 
                        xlab="Container volume (L)",
                        ylab="Size index (calliper x height)",
                        axes=FALSE, type='n'))
    for(i in 1:length(qf_large)){
      abline(qf_large[[i]], lty=5)
    }
    
    magaxis(side=1:2, unlog=1:2)
    box()
    
    points(log10(as.numeric(input$volume_entry)), log10(as.numeric(input$calliper_entry) * as.numeric(input$height_entry)),
            pch=15, col="red", cex=2)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "woodstockplot.png",
    content = function(file) {
      png(file)
      f <- info()
      comparison_standard_plot(input, f, standard_df)
      dev.off()
    })    
  
  info <- eventReactive(input$uploadedfile, {
    
    req(input$uploadedfile)
    
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
# runGadget(shinyApp(ui, server), 
#           viewer = paneViewer())
shinyApp(ui, server)

source("R/load_packages.R")
source("R/read_data.R")



# ui -------------------------

ui <- miniPage(theme="miniUI.css",
  gadgetTitleBar("Treestocker", left=NULL, right=NULL),
  useShinyalert(),
  
  tags$style(HTML("
    @import url('//fonts.googleapis.com/css?family=Roboto');
    h2, h3, button,p {
      font-family: 'Roboto';
    }")),
  
  miniTabstripPanel(
    miniTabPanel("Info", icon=icon("home"),
                 miniContentPanel(
                   h4(strong("Welcome to the app for the AS2303 Standard")),
                   h4("Compare size index of your trees to the national database"),
                   h4(a("Visit this page to read about this project", 
                        href="https://www.westernsydney.edu.au/hie/research/research_projects/tree_stock_standard")),
                   img(src="tree.png", align="left")
                 )
    ),
    miniTabPanel("Enter Data", icon=icon("bar-chart"),
                 miniContentPanel(
                   h3("Enter container volume, calliper and height and compare to the new standard"),
                   useShinyjs(),
                   
                   numericInput("volume_entry", label=h4("Container volume (L) (18 - 3000L)"), value=NA),
                   numericInput("calliper_entry", label=h4("Calliper (mm)"), value=NA),
                   numericInput("height_entry", label=h4("Height (m)"), value=NA),
                   # radioGroupButtons("everdeci_entry", label=h4("Type"), 
                   #                   choices=list("Deciduous" = "deci", "Evergreen" = "ever"),
                   #                   selected=1),
                   
                   textOutput("sizeindex_message", container=h2),
                   plotOutput("dataplot", width="100%")
                 )
    ),
    # miniTabPanel("Upload", icon=icon("upload"),
    #              miniContentPanel(
    #                h3("Upload a data file and compare to the new standard"),
    #                p("Recommended for desktop use only"),
    #                fileInput("uploadedfile", "Choose Excel or CSV File",
    #                          multiple = FALSE,
    #                          accept = c("text/csv",
    #                                     "text/comma-separated-values,text/plain",
    #                                     ".csv", ".xlsx", ".xls")),
    #                selectInput("container_column", "Column with container volume (L):", choices = NULL),  
    #                selectInput("calliper_column", "Column with calliper (mm):", choices = NULL),
    #                selectInput("height_column", "Column with height (m):", choices = NULL),
    #                plotOutput("table_display"),
    #                downloadButton('downloadPlot', 'Download Plot')
    #              )
    # ),
    miniTabPanel("Database", icon=icon("map-o"),
                 miniContentPanel(
                   h3("Locations of participating nurseries"),
                   p("This app is based on data collected at", 
                     strong("23"), "nurseries",
                     strong("632"), "batches",
                     "and", strong("13796"), "trees"),
                   p("To download the raw data", 
                     a("visit this page", href="https://github.com/courtneycampany/HIA_size_index")),
                   withSpinner(leafletOutput("mymap"))
                 )
    )
  )
  
)



# server ----------------------- 

server <- function(input, output, session) {
  
  # observeEvent(input$volume_entry, {
  #   if (input$volume_entry >= 100 | is.na(input$volume_entry) | input$volume_entry == 0){
  #     shinyjs::hide("everdeci_entry")
  #   } else {
  #     shinyjs::show("everdeci_entry")
  #   }
  #   
  # })
  observeEvent(input$volume_entry, {
    if(!is.na(input$volume_entry) && input$volume_entry > 3000){
      shinyalert("Error", "Please enter a container volume less than 3000L", type = "error")
    }
  })
  
  output$sizeindex_message <- renderText({
    
    req(input$volume_entry)
    # if(input$volume_entry <= 100)req(input$everdeci_entry)
    
    vals <- c(input$volume_entry, input$height_entry, input$calliper_entry)
    vals_num <- as.numeric(vals)
    
    if(any(is.na(vals_num))){
      return("")
    }
    
    if(any(vals == 0)){
      return("")
    } else {
      si <- input$height_entry * input$calliper_entry
      if(isTruthy(input$volume_entry) && input$volume_entry > 100){

        # check if data point in range of figure
        in_range <- findInterval(input$volume_entry, x_range_large) == 1 &
          findInterval(si, y_range_large) == 1
        
      } else {
        
        in_range <- findInterval(input$volume_entry, x_range_small) == 1 &
          findInterval(si, y_range_small) == 1
        
      }
      msg <- sizeindex_evaluate(input$volume_entry, si, qf_plot)
      
      if(!in_range){
        msg <- paste0(msg, "\nData point is outside figure range.")
      }
      
      return(msg)
    }
    
  })
  
  output$dataplot <- renderPlot({

    req(input$volume_entry,  input$calliper_entry, input$height_entry)
    
    plot_si_grid_interf(input$volume_entry, input$everdeci_entry)
    
    points(log10(as.numeric(input$volume_entry)), 
           log10(as.numeric(input$calliper_entry) * as.numeric(input$height_entry)),
           pch=15, col="red", cex=2)
    
  })
  
  output$mymap <- renderLeaflet({
    make_leaflet_map(locations)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "woodstockplot.png",
    content = function(file) {
      png(file)
      f <- info()
      
      plot_uploaded_data(input, f)
      
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
    
    plot_uploaded_data(input, f)
  
  })
  
}



# Run app -------------
# runGadget(shinyApp(ui, server), 
#           viewer = paneViewer())
shinyApp(ui, server)

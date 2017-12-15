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
                   h4("View the map of nurseries included in the national study."),
                   img(src="tree.png", align="center")
                 )
    ),
    miniTabPanel("Enter Data", icon=icon("bar-chart"),
                 miniContentPanel(
                   h4("For Mobile use. Enter container volume, calliper and height and compare to the new standard."),
                   useShinyjs(),
                   # 
                   # dropdownButton(
                     
                     numericInput("volume_entry", label=h4("Container volume (L)"), value=NA),
                     numericInput("calliper_entry", label=h4("Calliper (mm)"), value=NA),
                     numericInput("height_entry", label=h4("Height (m)"), value=NA),
                     radioGroupButtons("everdeci_entry", label=h4("Type"), 
                                       choices=list("Deciduous" = "deci", "Evergreen" = "ever"),
                                       selected=1),
                     # #actionButton("button", "Close"),
                     # circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                     # tooltip = tooltipOptions(title = "Click to enter data")
                   
                   
                   textOutput("sizeindex_message", container=h2),
                   plotOutput("dataplot", width="80%")
                   
                   
                 )
    ),
    miniTabPanel("Upload", icon=icon("upload"),
                 miniContentPanel(
                   h4("For Desktop use. Upload a data file and compare to the new standard."),
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
                   h4("Locations of participating nurseries."),
                   leafletOutput("mymap")
                 )
    )
  )
  
)



# server ----------------------- 

server <- function(input, output, session) {
  
  observeEvent(input$volume_entry, {
    if (input$volume_entry >= 100 | is.na(input$volume_entry) | input$volume_entry == 0){
      shinyjs::hide("everdeci_entry")
    } else {
      shinyjs::show("everdeci_entry")
    }
    
  })
  
  output$sizeindex_message <- renderText({
    
    req(input$volume_entry)
    
    vals <- c(input$volume_entry,input$height_entry,input$calliper_entry)
    vals_num <- as.numeric(vals)
    
    if(any(is.na(vals_num))){
      return("")
    }
    
    if(any(vals == 0)){
      return("")
    } else {
      si <- input$height_entry * input$calliper_entry
      if(isTruthy(input$volume_entry) && input$volume_entry > 100){
        fits <- qf_large_all
      } else {
        if(isTruthy(input$everdeci_entry)){
          if(input$everdeci_entry == "ever"){
            fits <- qf_small_ever_all
          } else if(input$everdeci_entry == "deci"){
            fits <- qf_small_deci_all
          }
        } else {
          fits <- qf_small_all
        }
        
      }
      msg <- sizeindex_evaluate(input$volume_entry, si, fits)
      return(msg)
    }
    
  })
  
  output$mymap <- renderLeaflet({
    make_leaflet_map(locations)
  })
  
  output$dataplot <- renderPlot({

    plot_si_grid_interf(input$volume_entry, input$everdeci_entry)
    
    points(log10(as.numeric(input$volume_entry)), 
           log10(as.numeric(input$calliper_entry) * as.numeric(input$height_entry)),
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
    
    if(input$container_column != '')vol <- as.numeric(f[, input$container_column])
    if(input$calliper_column != '')diam <- as.numeric(f[, input$calliper_column])
    if(input$height_column != '')height <- as.numeric(f[, input$height_column])
    
    if(input$container_column != ''){
      plot_si_grid_interf(max(vol), "all")
    }
    
    if(!('' %in% c(input$container_column,input$calliper_column,input$height_column))){
      points(log10(vol), log10(diam*height), pch=19, col="red")
    }
    
  })
  
}



# Run app -------------
# runGadget(shinyApp(ui, server), 
#           viewer = paneViewer())
shinyApp(ui, server)

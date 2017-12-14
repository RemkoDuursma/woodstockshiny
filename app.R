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
                   h4("For Mobile use. Enter container volume, calliper and height and compare to the new standard."),
                   useShinyjs(),
                   
                   dropdownButton(
                     tags$h3("Input"),
                     numericInput("volume_entry", label=h3("Container volume (L)"), value=0),
                     numericInput("calliper_entry", label=h3("Calliper (mm)"), value=0),
                     numericInput("height_entry", label=h3("Height (cm)"), value=0),
                     radioGroupButtons("everdeci_entry", label=h3("Type"), 
                                       choices=list("Deciduous" = "deci", "Evergreen" = "ever"),
                                       selected=1),
                     #actionButton("button", "Close"),
                     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                     tooltip = tooltipOptions(title = "Click to enter data")
                   ),
                   
                   plotOutput("dataplot", width="80%"),
                   textOutput("sizeindex_message", container=h2)
                   
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
    toggle("everdeci_entry", (input$volume_entry >= 100 | input$volume_entry == 0))
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
      if(input$volume_entry > 100){
        fits <- qf_large
      } else {
        if(isTruthy(input$everdeci_entry)){
          if(input$everdeci_entry == "ever"){
            fits <- qf_small_ever
          } else if(input$everdeci_entry == "deci"){
            fits <- qf_small_deci
          }
        } else {
          fits <- qf_small
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
    
    
    
    par(cex.lab=1.3)
    if(!isTruthy(input$volume_entry) || input$volume_entry == 0){
      plot(1, type='n', ann=F, axes=F)
    } else {
      
      if(input$volume_entry >= 100){
        with(treestats_large, plot(log10(volume), log10(si), 
                          xlab="Container volume (L)",
                          ylab="Size index (calliper x height)",
                          axes=FALSE, type='n'))
        for(i in 1:length(qf_large)){
          abline(qf_large[[i]], lty=5)
        }
      } else {
        if(isTruthy(input$everdeci_entry)){
          if(input$everdeci_entry == "ever"){
            with(treestats_small_ever, plot(log10(volume), log10(si), 
                                       xlab="Container volume (L)",
                                       ylab="Size index (calliper x height)",
                                       axes=FALSE, type='n'))
            for(i in 1:length(qf_small_ever)){
              abline(qf_small_ever[[i]], lty=5)
            }
          }
          if(input$everdeci_entry == "deci"){
            with(treestats_small_deci, plot(log10(volume), log10(si), 
                                            xlab="Container volume (L)",
                                            ylab="Size index (calliper x height)",
                                            axes=FALSE, type='n'))
            for(i in 1:length(qf_small_deci)){
              abline(qf_small_deci[[i]], lty=5)
            }
          }
          magaxis(side=1:2, unlog=1:2)
          box()
          
          points(log10(as.numeric(input$volume_entry)), log10(as.numeric(input$calliper_entry) * as.numeric(input$height_entry)),
                 pch=15, col="red", cex=2)
        } else {
          with(treestats_small, plot(log10(volume), log10(si), 
                                          xlab="Container volume (L)",
                                          ylab="Size index (calliper x height)",
                                          axes=FALSE, type='n'))
          for(i in 1:length(qf_small)){
            abline(qf_small[[i]], lty=5)
          }
          magaxis(side=1:2, unlog=1:2)
          box()
          
          points(log10(as.numeric(input$volume_entry)), log10(as.numeric(input$calliper_entry) * as.numeric(input$height_entry)),
                 pch=15, col="red", cex=2)
        }
      }
    }
    

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

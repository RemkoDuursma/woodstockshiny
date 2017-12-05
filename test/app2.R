library(shiny)
source("R/load_packages.R")
source("R/read_data.R")

ui <- fluidPage(
  titlePanel("This is a scatterplot"),

  sidebarLayout(
    sidebarPanel(
      
      fileInput("uploadedfile", "Choose Excel or CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xlsx", ".xls")),
      selectInput("container_column", "Column with container volume (L):", choices = NULL),  
      selectInput("calliper_column", "Column with calliper (mm):", choices = NULL),
      selectInput("height_column", "Column with height (m):", choices = NULL),
      ,
      downloadButton('downloadPlot', 'Download Plot')
      
    ),
    
    mainPanel(          
      h4("Here is your scatterplot"),
      plotOutput("table_display")
    )
  ))


server <- function(input, output, session) {
  
  
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
  
  output$downloadPlot <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file)
      f <- info()
      comparison_standard_plot(input, f, standard_df)
      dev.off()
    })    
  
}




# Run the application 
shinyApp(ui = ui, server = server)

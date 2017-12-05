library(shiny)
library(foreign)


ui <- fluidPage(
  titlePanel("This is a scatterplot"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput('datafile', 'Choose CSV file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      
      uiOutput("varselect1"),
      
      uiOutput("varselect2"),
      
      downloadButton('downloadPlot', 'Download Plot')
      
    ),
    
    mainPanel(          
      h4("Here is your scatterplot"),
      plotOutput("plot1")
    )
  ))

server <- function(session,input, output) {
  
  DataInput <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
  output$varselect1 <- renderUI({
    
    if (identical(DataInput(), '') || identical(DataInput(),data.frame())) return(NULL)
    
    cols <- names(DataInput())
    selectInput("var1", "Select a variable:",choices=c("---",cols[3:length(cols)]), selected=("---"))
    
  })
  
  output$varselect2 <- renderUI({
    
    if (identical(DataInput(), '') || identical(DataInput(),data.frame())) return(NULL)
    
    cols <- names(DataInput())
    selectInput("var2", "Select a variable:",choices=c("---",cols[3:length(cols)]), selected=("---"))
    
  })
  
  
  
  plotInput <- reactive({
    
    a <- which(names(DataInput())==input$var1)
    x_lab <- as.numeric(DataInput()[,a])
    
    
    b <- which(names(DataInput())==input$var2)
    y_lab <- as.numeric(DataInput()[,b])      
    
    main.text <- paste("Scatterplot of the variables",colnames(DataInput())[a],"and", colnames(DataInput())[b],sep = " ", collapse = NULL)
    
    plot(x_lab, y_lab, main=main.text, xlab=colnames(DataInput())[a], ylab=colnames(DataInput())[b], xlim=c(min(x_lab),max(x_lab)*1.05), ylim=c(min(y_lab), max(y_lab)*1.05))
    
    observations <- DataInput()[,1]
    
    text(x_lab, y_lab, labels=observations, pos=3)
    
    
  })
  
  output$plot1 <- renderPlot({
    print(plotInput())
  })
  
  
  output$downloadPlot <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file)
      print(plotInput())
      dev.off()
    })    
  
}




# Run the application 
shinyApp(ui = ui, server = server)



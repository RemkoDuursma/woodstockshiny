library(shiny)
library(leaflet)
library(shinythemes)

locations <- read.csv("data/nursery_locations.csv")
locations$howmany <- paste(locations$nursery, locations$trees, sep=" ")


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  theme=shinytheme("flatly"),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
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
}

shinyApp(ui, server)

locations <- read.csv("data/nursery_locations.csv")
locations$howmany <- paste(locations$nursery, locations$trees, sep=" ")

treestats <- read.csv("data/tree_stats.csv")

treestats_tab <- dplyr::select(treestats, volume, species, nursery, sizeindex) %>%
  mutate(sizeindex = round(sizeindex,1),
         species = Hmisc::capitalize(gsub("_"," ", species))) %>%
  arrange(species, volume)

shinyServer(function(input, output, session) {
  
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  
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
  
  output$treestatsdata <- DT::renderDataTable(datatable(treestats_tab, caption="Data by batch"))
  
})

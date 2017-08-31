make_leaflet_map <- function(locations){
  
  icons <- awesomeIcons(c(rep("tree-deciduous", 23), "home"),
                        markerColor=c(rep("forestgreen",23), "#990033"))
  
  leaflet(locations) %>% 
    addProviderTiles('CartoDB.Positron') %>% 
    addAwesomeMarkers(lng = ~long,
                      lat = ~lat,
                      clusterOptions = markerClusterOptions(),
                      popup = ~popup,
                      icon=icons)

}
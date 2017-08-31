comparison_standard_plot <- function(input, f, standard_df){
  
  with(standard_df, plot(log10(x), log10(y), type='n', 
                         xlab="Container volume (L)",
                         ylab="Size index (height * calliper)",
                         axes=FALSE, 
                         xlim=log10(c(10,5000)),
                         ylim=log10(c(10,2000))))
  magaxis(side=1:2, unlog=1:2)
  with(subset(standard_df, limit == "min"), lines(log10(x), log10(y)))
  with(subset(standard_df, limit == "max"), lines(log10(x), log10(y)))
  
  if(input$container_column != '')vol <- as.numeric(f[, input$container_column])
  if(input$calliper_column != '')diam <- as.numeric(f[, input$calliper_column])
  if(input$height_column != '')height <- as.numeric(f[, input$height_column])
  
  if(!('' %in% c(input$container_column,input$calliper_column,input$height_column))){
    points(log10(vol), log10(diam*height), pch=19, col="red")
  }
  
}
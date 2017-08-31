
make_ggiraph_plot <- function(si_means, standard_df){
  
  siplot <- ggplot(data=si_means, aes(x=volume, y=sizeindex.mean, tooltip=tooltip, data_id=row.names(si_means))) + 
    scale_x_log10(limits=c(10,5000)) +
    scale_y_log10() +
    geom_polygon_interactive(data=standard_df, aes(x=x, y=y, alpha=0.2, 
                                                   colour="forestgreen", tooltip="AS2303 Standard", data_id="1")) +
    geom_point_interactive(size=2) +
    theme_bw() +
    labs(x="Container volume (L)", y="Size index") +
    theme(legend.position="none")
  
  ggiraph(code = {print(siplot)}, hover_css = "fill:red;", selection_type="none")
  
}

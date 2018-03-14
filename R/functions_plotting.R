plot_si_ranges <- function(fits, whichfits=1:length(fits), xlim, ylim, labsrt=30,
                           labyadj=0,
                           these_lines=2:3,
                           quantiles = c(0.05,0.1,0.25,0.75,0.9,0.95),
                           poly_colors = c("grey95", "lightgrey", "grey", "lightgrey", "grey95")){
  
  fits <- fits[whichfits]
  
  xat <- c(18,25,35,150,1500, as.vector(sapply(1:3, function(i)seq(10^i, 10^(i + 1) - 10^i, by=10^i))))
  yat <- c(15,150,1500,as.vector(sapply(0:3, function(i)seq(10^i, 10^(i + 1) - 10^i, by=10^i))))
  
  par(yaxs="i", xaxs="i", las=2, tcl=0, cex.lab=1.2, 
      mgp=c(2.4, 0.25, 0), cex.axis=0.7, pty='s',
      family="Gotham Narrow Book")
  
  plot(1, pch=16, cex=0.5, 
       type='n',
       panel.first={
         abline(v=log10(xat), col="grey")
         abline(h=log10(yat), col="grey")
         
         poly_rqs(fits[[1]], fits[[2]], reverse=TRUE)
         poly_rqs(fits[[2]], fits[[3]], gradient=FALSE)
         poly_rqs(fits[[3]], fits[[4]])
       },
       ylim=ylim,
       xlim=xlim,
       xlab="Container Volume (L)",
       ylab="Size Index (calliper x height)",
       col="darkgrey", axes=FALSE)
  for(i in these_lines)abline(fits[[i]], lty=2, lwd=1)
  
  # for(i in 1:(length(fits)-1)){
  #   poly_rqs(fits[[i]], fits[[i+1]], col=alpha(poly_colors[i], 0.6))
  # }
  
  
  
  axis(1, at=log10(xat), labels=xat)
  axis(2, at=log10(yat), labels=yat)
  box()
  
  u <- par("usr")
  xat <- u[1] + 0.2*(u[2] - u[1])
  y <- sapply(fits, function(x)predict(x, newdata=data.frame(volume = 10^xat)))
  labs <- paste0(100*quantiles, "%")
  text(xat, y+labyadj, labs, pos=2, font=2, cex=0.8, srt=labsrt)
  
  xat <- u[1] + 0.5*(u[2] - u[1])
  y <- sapply(fits, function(x)predict(x, newdata=data.frame(volume = 10^xat)))
  y <- y[1:(length(y)-1)] + diff(y)/2
  labs <- c("","Preferred Range","")
  text(xat, y, labs, pos=2, font=2, cex=1.1, srt=labsrt, col="dimgrey")
  
}



#--- For quantile regressions
poly_rqs <- function(mod1, mod2, reverse=FALSE, gradient=TRUE, ...){
  
  pu <- par("usr")
  x <- pu[1:2]
  
  newdat <- data.frame(volume=10^x)
  y1 <- predict(mod1, newdata=newdat)
  y2 <- predict(mod2, newdata=newdat)
  
  if(gradient){
    cols <- grey(seq(0.7, 0.92, length=20), alpha=0.7)
    if(reverse)cols <- rev(cols)
    n <- length(cols)
    
    for(i in 1:n){
      yto <- y1 + (i/n)*(y2 - y1)
      yfrom <- y1 + ((i-1)/n)*(y2 - y1)
      polygon(x=c(x, rev(x)), y=c(yfrom, rev(yto)), border=NA, col=cols[i])
    }
  } else {
    polygon(x=c(x, rev(x)), y=c(y1, rev(y2)), border=NA, col=grey(0.7, alpha=0.9))
  }
}

# axis limits are set by constants ('x_range_large' etc.), defined in read_data.R
plot_si_grid <- function(size=c("small", "large")){
  
  size <- match.arg(size)
  if(size == "large"){
    plot_si_ranges(qf_plot, 
                   quantiles=taus_plot,
                   xlim=log10(x_range_large), ylim=log10(y_range_large),
                   labsrt=34)
  } else {
  
    plot_si_ranges(qf_plot, 
                     quantiles=taus_plot,
                     labsrt=22,
                     xlim=log10(x_range_small), ylim=log10(y_range_small))
  }
  
}

plot_si_grid_interf <- function(volume, everdeci){
  
  if(is.na(volume)){
    plot(-9999, type='n', ann=F, axes=F)
  } else {
    
    if(volume >= 100){
      plot_si_grid("large")
      legend("top", "> 100L", bty='n', text.font=3)
    } else {
      plot_si_grid("small")
      legend("top", "< 100L", bty='n', text.font=3)
    }
  }
}


plot_uploaded_data <- function(input, f){
  
  if(input$container_column != '')vol <- as.numeric(f[, input$container_column])
  if(input$calliper_column != '')diam <- as.numeric(f[, input$calliper_column])
  if(input$height_column != '')height <- as.numeric(f[, input$height_column])
  
  if(input$container_column != ''){
    plot_si_grid_interf(max(vol), "all")
  }
  
  if(!('' %in% c(input$container_column,input$calliper_column,input$height_column))){
    points(log10(vol), log10(diam*height), pch=19, col="red")
  }
  
}




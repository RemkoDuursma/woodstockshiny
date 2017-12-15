plot_si_ranges <- function(fits, whichfits=1:length(fits), xlim, ylim, 
                           labsrt=30,
                           labyadj=0,
                           plot_lab=TRUE,
                           quantiles = c(0.05,0.1,0.25,0.75,0.9,0.95),
                           plot_poly=TRUE,
                           poly_colors = c("grey95", "lightgrey","grey", 
                                           "lightgrey", "grey95"),
                           plot_data=NULL,
                           ...){
  
  fits <- fits[whichfits]
  
  xat <- as.vector(sapply(1:3, function(i)seq(10^i, 10^(i + 1) - 10^i, by=10^i)))
  yat <- as.vector(sapply(0:3, function(i)seq(10^i, 10^(i + 1) - 10^i, by=10^i)))
  
  par(yaxs="i", xaxs="i", las=1, tcl=0, cex.lab=1.2, 
      mgp=c(2.4, 0.25, 0), 
      cex.axis=0.7, pty='s')
      #family="Gotham Narrow Book")
  
  plot(1, pch=16, cex=0.5, 
       type='n',
       panel.first={
         abline(v=log10(xat), col="grey")
         abline(h=log10(yat), col="grey")
         
         if(plot_poly){
           for(i in 1:(length(fits)-1)){
             poly_rqs(fits[[i]], fits[[i+1]], 
                      col=alpha(poly_colors[i], 0.6))
           }
         }
         
         if(!is.null(plot_data)){
           with(plot_data, points(log10(volume), log10(si), pch=16, 
                                  col="grey", cex=0.5))
         }
         
       },
       ylim=ylim,
       xlim=xlim,
       xlab="Container Volume (L)",
       ylab="Size Index (calliper x height)",
       col="darkgrey", axes=FALSE)
  for(i in 1:length(fits))abline(fits[[i]], lty=2, lwd=1)
  
  axis(1, at=log10(xat), labels=xat)
  axis(2, at=log10(yat), labels=yat)
  box()
  
  if(plot_lab){
    u <- par("usr")
    xat <- u[1] + 0.2*(u[2] - u[1])
    y <- sapply(fits, function(x)predict(x, newdata=data.frame(volume = 10^xat)))
    labs <- paste0(100*quantiles, "%")
    text(xat, y+labyadj, labs, pos=2, font=2, cex=0.8, srt=labsrt)
    
    xat <- u[1] + 0.5*(u[2] - u[1])
    y <- sapply(fits, function(x)predict(x, newdata=data.frame(volume = 10^xat)))
    y <- y[1:(length(y)-1)] + diff(y)/2
    labs <- c("Small","Medium","Large")
    text(xat, y, labs, pos=2, font=2, cex=1.1, srt=labsrt, col="dimgrey")
  }
  
}


make_si_plot <- function(data, quantiles, ...){
  
  fits <- lapply(quantiles, function(x)rq(log10(si) ~ log10(volume), data=data, tau=x))
  
  plot_si_ranges(fits, 
                 poly_colors=c("lightgrey","grey","lightgrey"),
                 quantiles=quantiles, ...)
  
}

#--- For quantile regressions
poly_rqs <- function(mod1, mod2, ...){
  
  pu <- par("usr")
  x <- pu[1:2]
  
  newdat <- data.frame(volume=10^x)
  y1 <- predict(mod1, newdata=newdat)
  y2 <- predict(mod2, newdata=newdat)
  
  polygon(x=c(x, rev(x)), y=c(y1, rev(y2)), border=NA, ...)
}


plot_si_grid <- function(size=c("small", "large"), type=c("all","deci","ever")){
  
  size <- match.arg(size)
  if(size == "large"){
    plot_si_ranges(qf_large_plot, 
                   quantiles=taus_plot,
                   xlim=log10(c(100,3000)), ylim=log10(c(50,3000)),
                   labsrt=34)
  } else {
    
    type <- match.arg(type)
    
    if(type == "ever"){
      plot_si_ranges(qf_small_ever_plot, 
                     quantiles=taus_plot,
                     labsrt=22,
                     xlim=log10(c(18,100)), ylim=log10(c(8,200)))
    }
    if(type == "deci"){
      plot_si_ranges(qf_small_deci_plot, 
                     quantiles=taus_plot,
                     labsrt=22,
                     xlim=log10(c(18,100)), ylim=log10(c(8,200)))
    }
    if(type == "all"){
      plot_si_ranges(qf_small_plot, 
                     quantiles=taus_plot,
                     labsrt=22,
                     xlim=log10(c(18,100)), ylim=log10(c(8,200)))
    }
    
  }
  
  
}


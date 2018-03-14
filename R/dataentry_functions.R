
# rqfit needs to be a list, e.g.:
# fits <- lapply(taus, function(x)rq(log10(si) ~ log10(volume), data=treestats_small_ever, tau=x))
#
# with(treestats_small_ever, plot(log10(volume), log10(si), axes=F))
# magicaxis::magaxis(side=1:2, unlog=1:2)
# 
# for(i in 1:length(qf_small_ever)){
#   abline(qf_small_ever[[i]])
# }
# points(log10(50),log10(30), pch=19, col="red")
# sizeindex_evaluate(50,30, qf_small_ever)
sizeindex_evaluate <- function(.X, .Y, rqfit){
  
  # taus <- seq(0,1,by=0.05)
  # fit1 <- rq(y ~ x, data=dat, tau=taus)
  # 
  # .X <- 0.5
  # .Y <- -0.4
  
  # Size index at each of the quantiles used in the rqfit object (itself a list of rq objects)
  p <- 10^sapply(rqfit, function(x)predict(x, newdata=data.frame(volume=.X)))
  
  # Which quantiles does our observation fall between?
  int <- try(findInterval(.Y,p))
  
  # Actual quantiles
  if(inherits(int, "try-error"))return("")
  tau_low <- taus_plot[int]
  tau_high <- taus_plot[int+1]
  
  if(any(is.na(c(.X, .Y))))return("")
  
  if(int == 0){
    
    msg <- "Your trees are smaller than 5% of trees in the database."
    
  } else if (int == 4){
    
    msg <- "Your trees are larger than 95% of trees in the database."
    
  } else if(int == 1){
    
    msg <- "Your trees are in the lower range."
    
  } else if(int == 2){
    
    msg <- "Your trees are in the preferred range."
  } else if(int == 3) {
    msg <- "Your trees are in the upper range."
  }
  
  return(msg)
}






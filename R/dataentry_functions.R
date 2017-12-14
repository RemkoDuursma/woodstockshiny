
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
  
  p <- 10^sapply(rqfit, function(x)predict(x, newdata=data.frame(volume=.X)))
  
  int <- findInterval(.Y,p)
  tau_low <- taus[int]
  tau_high <- taus[int+1]
  
  if(any(is.na(c(.X, .Y))))return("")
  
  if(int == 0){
    
    msg <- sprintf("Your trees are small compared to the database (%s%% were larger).",
                   (1 - taus[1])*100)
    
  } else if (int == length(p)){
    
    msg <- sprintf("Your trees are large compared to the database (%s%% were smaller).",
                   taus[length(taus)]*100)
    
  } else if(tau_low < 0.5){
    
    msg <- sprintf("Your trees are smaller than %s%% of trees in the database.", 
                   (1 - tau_high)*100)
    
  } else {
    
    msg <- sprintf("Your trees are larger than %s%% of batches in the database.", 
                   tau_low*100)
  }
  
  return(msg)
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


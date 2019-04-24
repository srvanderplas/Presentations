
library(ggplot2)
library(reshape2)
library(plyr)
# source("./code/themeStimuli.R")

theme_stimuli <- function(base_size = 12, base_family = ""){
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(legend.background = element_blank(), 
          legend.key = element_blank(), 
          panel.background = element_rect(fill="white",color="black"), 
          panel.border = element_blank(), 
          axis.title = element_blank(),
          strip.background = element_rect(fill="grey80",color="black"), 
          plot.background = element_blank(),
          panel.grid.major = element_line(color="grey85"),
          axis.text=element_blank())
}


createSine <- function(n=200, len=1, f=f, fprime=fprime, f2prime=f2prime, a=0, b=2*pi) {
  x <- seq(a, b, length=n+2)[(2:(n+1))]
  ell <- rep(len, length=length(x))
  fx <- f(x)
  ystart <- fx - .5*ell
  yend <- fx + .5*ell
  
  # now correct for line illusion in vertical direction
  dy <- diff(range(fx))
  dx <- diff(range(x))
  # fprime works in framework of dx and dy, but we represent it in framework of dx and dy+len
  # needs to be fixed by factor a:  
  a <- dy/(dy + len) 
  # ellx is based on the "trig" correction
  ellx <- ell / cos(atan(abs(a*fprime(x))))
  # ellx2 is based on linear approximation of f  
  ellx2 <- ell * sqrt(1 + a^2*fprime(x)^2)
  
  # make this a data frame - ggplot2 doesn't do well with floating vectors
  dframe <- data.frame(x=x, xstart=x, xend=x, y=fx, ystart=ystart, yend=yend, ell=ell, ellx = ellx, ellx2=ellx2)
  
  # third adjustment is based on quadratic approximation of f.
  # this needs two parts: correction above and below f(x)  
  
  fp <- a*fprime(x)
  f2p <- a*f2prime(x)
  lambdap <- (sqrt((fp^2+1)^2-f2p*fp^2*ell) + fp^2 + 1)^-1    
  lambdam <- -(sqrt((fp^2+1)^2+f2p*fp^2*ell) + fp^2 + 1)^-1    
  
  
  dframe$ellx4.l <- (4*abs(lambdap)*sqrt(1+fp^2))^-1
  dframe$ellx4.u <- (4*abs(lambdam)*sqrt(1+fp^2))^-1
  
  dframe
}

weightYTrans <- function(df, w){
  df$elltrans <- w*df$ellx2/2 + (1-w)*df$ell/2
  df$ystart <- df$y - df$elltrans
  df$yend <- df$y + df$elltrans
  df$w <- w
  df
}

getYlim <- function(w, orig, f, fprime, f2prime){
  temp <- melt(ldply(c(0, 1.4), function(i) weightYTrans(orig, i)[,c(1,5, 6)]), id.vars="x", value.name="y", variable.name="var")
  dy <- diff(apply(temp[,c(1,3)], 2, function(k) diff(range(k))))
  dx <- 0
  if(dy>0) {
    dx <- dy
    dy <- 0
  }
  return(list(dx=range(temp$x)+c(-1, 1)*dx/2, dy=range(temp$y)+c(-1, 1)*-dy/2))
}

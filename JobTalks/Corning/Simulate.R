library(lubridate)
library(plyr)
library(ggplot2)
data <- data.frame(
  time = seq(ymd('2001-09-01'),ymd('2012-9-01'),by='days'),
  capacity.factor = 100)

# set seed
set.seed(413482534)

# ********** Generate Simulated Uptime Data ************************************
# Outages
outage.start <- seq(ymd('2002-02-15'),ymd('2012-09-15'), by='months')[1+18*(0:7)]
outage.length <- rpois(8, 45)
idx <- which(data$time%in%outage.start)
outage <- unlist(lapply(1:8, function(i) seq(idx[i],(idx+outage.length)[i])))
outage <- outage[outage<=nrow(data)]
data$capacity.factor[outage] <- 0
data$outage <- FALSE
data$outage[outage] <- TRUE

# Downpowers
# assume 2-3 days, approximately every 90 days
interpolate.downpower <- function(i=1, hours.out=NULL, duration, cap.factor, ramp.speed=2){
  if(is.null(hours.out)) hours.out <- rep(16, length(duration))
  x <- if(duration[i]>1) 1:duration[i] else 1
  tmp <- data.frame(hours=1:(24*duration[i]), 
                    day=rep(x, each=24), 
                    capacity.factor=100)
  tmp$capacity.factor[1:hours.out[i]] <- cap.factor[i]
  tmp$capacity.factor[(1+hours.out[i]):nrow(tmp)] <- pmin(100, cap.factor[i]+(0:(nrow(tmp)-1-hours.out[i]))*ramp.speed)
  as.numeric(unlist(dlply(tmp, .(day), summarize, capacity.factor=mean(capacity.factor))))
}

idx <- which(data$capacity.factor>0)
downpower.cf <- sample(seq(30, 70, 10), 100, replace=T, prob=c(.2, .15, .1, .25, .4))
downpower.start <- rpois(100, 90)
downpower.duration <- ceiling(((100-downpower.cf)/2+16)/24)
downpower.start <- cumsum(downpower.start)
downpower.start <- idx[downpower.start]
downpower.duration <- downpower.duration[!is.na(downpower.start)]
downpower.start <- downpower.start[!is.na(downpower.start)] 
downpower.end <- downpower.start + downpower.duration[1:length(downpower.start)]
downpower <- unlist(lapply(1:length(downpower.start), function(i) downpower.start[i]:(downpower.end[i]-1)))
downpower.cftot <- unlist(lapply(1:length(downpower.start), interpolate.downpower, duration=downpower.duration, cap.factor=downpower.cf))
data$capacity.factor[downpower] <- downpower.cftot


# Unplanned Outages
# assume once a year, from 1-6 days
idx <- which(data$capacity.factor==100)
unplanned.start <- sample(idx, 10)
unplanned.duration <- sample(1:4, 10, replace=T, prob=c(.1, .2, .4, .3))
unplanned.end <- unplanned.start + unplanned.duration
unplanned <- unlist(lapply(1:10, function(i) unplanned.start[i]:(unplanned.end[i]+2)))
unplanned.cftot <- unlist(lapply(1:length(unplanned.start), interpolate.downpower, hours.out=unplanned.duration*24, duration = unplanned.duration+3, cap.factor=rep(0, length(unplanned.duration))))
data$capacity.factor[unplanned] <- unplanned.cftot

# one other outage of a month(ish)
idx <- which(data$capacity.factor==100)[300:1000]
unplanned.start <- sample(idx, 1)
unplanned.duration <- 40 #round(.5*rnorm(1, 10, 2)+.5*rnorm(1, 30, 10))
unplanned.end <- unplanned.start + unplanned.duration
unplanned <- unplanned.start:(unplanned.end+2)
data$capacity.factor[unplanned] <- interpolate.downpower(hours.out=unplanned.duration*24, duration=unplanned.duration+3, cap.factor=0)


# slightly tweak "full" capacity factor
idx <- which(data$capacity.factor==100)
data$capacity.factor[idx] <- 100-rnorm(length(idx), 1, .25)


# Daily CF plot
# qplot(data=data, x=time, y=capacity.factor, geom="line")

# EFPDL calc - effective full power days lost
data$EFPDL <- (1-data$capacity.factor/100)

# Summarize data by month
data$monthyear <- floor_date(data$time, "month")
month.summary <- ddply(data, .(monthyear), summarize, capacity.factor=mean(capacity.factor), EFPDL = sum(EFPDL), outage=sum(outage)>0)

# Monthly CF plot
# qplot(data=month.summary, x=monthyear, y=capacity.factor, 
#       geom=c("line", "point")) + 
#   geom_segment(data=subset(month.summary, outage), 
#                aes(x=monthyear, xend=monthyear, y=0, yend=capacity.factor), 
#                colour="red", inherit.aes=F) + 
#   theme_bw() + geom_smooth()

# Add counter of month during cycle
month.summary$cyclemonth <- c(12:18, 1:18, 1:18, 1:18, 1:18, 1:18, 1:18, 1:18) 

# acf(month.summary$capacity.factor, main="Autocorrelation of Capacity Factor")

N <- 100000
boots <- matrix(0, nrow=N, ncol=23)
EFboot <- rep(0, N)
nsamp <- dim(month.summary)[1]
for(i in 1:N){
  boots[i,] <- c(sample(which(!month.summary$outage & month.summary$cyclemonth>0 & month.summary$cyclemonth<=12), 12, replace=TRUE), # Sample 12 months from first 12 months of cycle
                 sample(which(!month.summary$outage & month.summary$cyclemonth>12), 6, replace=TRUE),    # Sample 6 months from months 13-19 of cycle
                 sample(which(!month.summary$outage), 5, replace=TRUE))              # Sample 5 months of full-cycle spectrum events **Assumption**
  EFboot[i] <- sum(month.summary$EFPDL[boots[i,]])  
}

EFmeandens <- density(EFboot)
EFmeanCDF <- cumsum(EFmeandens$y/sum(EFmeandens$y))
# qplot(x=EFboot, y=..density.., main="Total EFPDL, Segmented Cycle, Extra Wear", geom="density", xlab="EFPDL", ylab="Density") + 
#   geom_segment(aes(x=EFmeandens$x[which.min(abs(EFmeanCDF-.95))],
#                    xend=EFmeandens$x[which.min(abs(EFmeanCDF-.95))], 
#                    y=-Inf,
#                    yend=.007), 
#                colour="red") + 
#   geom_text(aes(x=EFmeandens$x[which.min(abs(EFmeanCDF-.95))]-2, y=0.008, 
#                 label=paste(" P(X<=", round(EFmeandens$x[which.min(abs(EFmeanCDF-.95))], 2), ") = .95 ", sep="")), hjust=0) + 
#   geom_segment(aes(x=EFmeandens$x[which.min(abs(EFmeanCDF-.9))],
#                    xend=EFmeandens$x[which.min(abs(EFmeanCDF-.9))],
#                    y=-Inf,
#                    yend=.015), 
#                colour="red") + 
#   geom_text(aes(x=EFmeandens$x[which.min(abs(EFmeanCDF-.9))]-2, y=0.016, 
#                 label=paste(" P(X<=", round(EFmeandens$x[which.min(abs(EFmeanCDF-.9))], 2),")  = .90 ", sep="")), hjust=0) + 
#   geom_segment(aes(x=EFmeandens$x[which.min(abs(EFmeanCDF-.5))],
#                    xend=EFmeandens$x[which.min(abs(EFmeanCDF-.5))],
#                    y=-Inf,
#                    yend=0.048), 
#                colour="red", linetype=2) +
#   geom_text(aes(x=EFmeandens$x[which.min(abs(EFmeanCDF-.5))]-2, y=0.049, 
#                 label=paste(" P(X<=", round(EFmeandens$x[which.min(abs(EFmeanCDF-.5))], 2), ") = .50 ", sep="")), hjust=0) 



N <- 100000
boots2 <- matrix(0, nrow=N, ncol=23)
EFboot2 <- rep(0, N)
nsamp <- dim(month.summary)[1]
for(i in 1:N){
  boots2[i,] <- sample(which(!month.summary$outage), 23, replace=TRUE)
  EFboot2[i] <- sum(month.summary$EFPDL[boots2[i,]])  
}

EFmeandens2 <- density(EFboot2)
EFmeanCDF2 <- cumsum(EFmeandens2$y/sum(EFmeandens2$y))
# qplot(x=EFboot2, y=..density.., main="Total EFPDL, Random Cycle, Extra Wear", geom="density", xlab="EFPDL", ylab="Density") + 
#   geom_segment(aes(x=EFmeandens2$x[which.min(abs(EFmeanCDF2-.95))],
#                    xend=EFmeandens2$x[which.min(abs(EFmeanCDF2-.95))], 
#                    y=-Inf,
#                    yend=.007), 
#                colour="red") + 
#   geom_text(aes(x=EFmeandens2$x[which.min(abs(EFmeanCDF2-.95))]-2, y=0.008, 
#                 label=paste(" P(X<=", round(EFmeandens2$x[which.min(abs(EFmeanCDF2-.95))], 2), ") = .95 ", sep="")), hjust=0) + 
#   geom_segment(aes(x=EFmeandens2$x[which.min(abs(EFmeanCDF2-.9))],
#                  xend=EFmeandens2$x[which.min(abs(EFmeanCDF2-.9))],
#                  y=-Inf,
#                  yend=.02), 
#              colour="red") + 
#   geom_text(aes(x=EFmeandens2$x[which.min(abs(EFmeanCDF2-.9))]-2, y=0.021, 
#                 label=paste(" P(X<=", round(EFmeandens2$x[which.min(abs(EFmeanCDF2-.9))], 2),")  = .90 ", sep="")), hjust=0) + 
#   geom_segment(aes(x=EFmeandens2$x[which.min(abs(EFmeanCDF2-.5))],
#                    xend=EFmeandens2$x[which.min(abs(EFmeanCDF2-.5))],
#                    y=-Inf,
#                    yend=0.045), 
#                colour="red", linetype=2) +
#   geom_text(aes(x=EFmeandens2$x[which.min(abs(EFmeanCDF2-.5))]-2, y=0.046, 
#                 label=paste(" P(X<=", round(EFmeandens2$x[which.min(abs(EFmeanCDF2-.5))], 2), ") = .50 ", sep="")), hjust=0) 

N <- 100000
boots3 <- matrix(0, nrow=N, ncol=22)
EFboot3 <- rep(0, N)
nsamp <- dim(month.summary)[1]
for(i in 1:N){
  boots3[i,] <- c(sample(which(!month.summary$outage & month.summary$cyclemonth>0 & month.summary$cyclemonth<=12), 12, replace=TRUE), # Sample 12 months from first 12 months of cycle
                  sample(which(!month.summary$outage & month.summary$cyclemonth>12), 6, replace=TRUE),    # Sample 6 months from months 13-19 of cycle
                  sample(which(!month.summary$outage), 4, replace=TRUE))               # Sample 4 months of EFPDL events from all months in the cycle **Assumption**
  EFboot3[i] <- sum(month.summary$EFPDL[boots3[i,]])  
}

EFmeandens3 <- density(EFboot3)
EFmeanCDF3 <- cumsum(EFmeandens3$y/sum(EFmeandens3$y))
# qplot(x=EFboot3, y=..density.., main="Total EFPDL, Segmented Cycle, Normal Wear", geom="density", xlab="EFPDL", ylab="Density") + 
#   geom_segment(aes(x=EFmeandens3$x[which.min(abs(EFmeanCDF3-.95))],
#                    xend=EFmeandens3$x[which.min(abs(EFmeanCDF3-.95))], 
#                    y=-Inf,
#                    yend=.0075), 
#                colour="red") + 
#   geom_text(aes(x=EFmeandens3$x[which.min(abs(EFmeanCDF3-.95))]-2, y=0.0085, 
#                 label=paste(" P(X<=", round(EFmeandens3$x[which.min(abs(EFmeanCDF3-.95))], 2), ") = .95 ", sep="")), hjust=0) + 
#   geom_segment(aes(x=EFmeandens3$x[which.min(abs(EFmeanCDF3-.9))],
#                    xend=EFmeandens3$x[which.min(abs(EFmeanCDF3-.9))],
#                    y=-Inf,
#                    yend=.0195), 
#                colour="red") + 
#   geom_text(aes(x=EFmeandens3$x[which.min(abs(EFmeanCDF3-.9))]-2, y=0.0205, 
#                 label=paste(" P(X<=", round(EFmeandens3$x[which.min(abs(EFmeanCDF3-.9))], 2),")  = .90 ", sep="")), hjust=0) + 
#   geom_segment(aes(x=EFmeandens3$x[which.min(abs(EFmeanCDF3-.5))],
#                    xend=EFmeandens3$x[which.min(abs(EFmeanCDF3-.5))],
#                    y=-Inf,
#                    yend=0.05), 
#                colour="red", linetype=2) +
#   geom_text(aes(x=EFmeandens3$x[which.min(abs(EFmeanCDF3-.5))]-2, y=0.051, 
#                 label=paste(" P(X<=", round(EFmeandens3$x[which.min(abs(EFmeanCDF3-.5))], 2), ") = .50 ", sep="")), hjust=0) 

N <- 100000
boots4 <- matrix(0, nrow=N, ncol=22)
EFboot4 <- rep(0, N)
nsamp <- dim(month.summary)[1]
for(i in 1:N){
  boots4[i,] <- sample(which(!month.summary$outage), 22, replace=TRUE)
  EFboot4[i] <- sum(month.summary$EFPDL[boots4[i,]])  
}

EFmeandens4 <- density(EFboot4)
EFmeanCDF4 <- cumsum(EFmeandens4$y/sum(EFmeandens4$y))
# qplot(x=EFboot4, y=..density.., main="Total EFPDL, Random Cycle, Normal Wear", geom="density", xlab="EFPDL", ylab="Density") + 
#   geom_segment(aes(x=EFmeandens4$x[which.min(abs(EFmeanCDF4-.95))],
#                    xend=EFmeandens4$x[which.min(abs(EFmeanCDF4-.95))], 
#                    y=-Inf,
#                    yend=.0075), 
#                colour="red") + 
#   geom_text(aes(x=EFmeandens4$x[which.min(abs(EFmeanCDF4-.95))]-2, y=0.0085, 
#                 label=paste(" P(X<=", round(EFmeandens4$x[which.min(abs(EFmeanCDF4-.95))], 2), ") = .95 ", sep="")), hjust=0) + 
#   geom_segment(aes(x=EFmeandens4$x[which.min(abs(EFmeanCDF4-.9))],
#                    xend=EFmeandens4$x[which.min(abs(EFmeanCDF4-.9))],
#                    y=-Inf,
#                    yend=.0195), 
#                colour="red") + 
#   geom_text(aes(x=EFmeandens4$x[which.min(abs(EFmeanCDF4-.9))]-2, y=0.0205, 
#                 label=paste(" P(X<=", round(EFmeandens4$x[which.min(abs(EFmeanCDF4-.9))], 2),")  = .90 ", sep="")), hjust=0) + 
#   geom_segment(aes(x=EFmeandens4$x[which.min(abs(EFmeanCDF4-.5))],
#                    xend=EFmeandens4$x[which.min(abs(EFmeanCDF4-.5))],
#                    y=-Inf,
#                    yend=0.05), 
#                colour="red", linetype=2) +
#   geom_text(aes(x=EFmeandens4$x[which.min(abs(EFmeanCDF4-.5))]-2, y=0.051, 
#                 label=paste(" P(X<=", round(EFmeandens4$x[which.min(abs(EFmeanCDF4-.5))], 2), ") = .50 ", sep="")), hjust=0) 

save(list=ls(), file="Simulate.Rdata")
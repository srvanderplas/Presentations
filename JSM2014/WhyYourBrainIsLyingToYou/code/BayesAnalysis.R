load("./code/BayesAnalysis.Rdata")
require(ggplot2)
require(plyr)
library(msm)

load("./code/BayesAnalysis.Rdata")
# # set.seed(70032608) # so that intervals don't change
# 
# turkdata.full <- read.csv("./data/turkdataclean.csv", stringsAsFactors=FALSE)
# turkdata <- subset(turkdata, len>10)
# # 
# # #-----------------------Distribution of LieFactor-------------------------------
# logpost <- function(data, par){
#   #   temp <- sum(dtnorm(data$ans.liefactor, mean=par[1], sd=par[2], lower=.95, upper=2.5, log=TRUE))
#   temp <- sum(dtnorm(data$ans.liefactor, mean=par[1], sd=par[2],lower=1, log=TRUE))
#   temp+34
# }
# 
# get_posterior_density <- function(data, pars){
#   temp <- sapply(1:nrow(pars), function(i) logpost(data, pars[i,]))
#   temp <- exp(temp)/sum(exp(temp))
#   data.frame(mean=pars[,1], sd=pars[,2], f=temp)
# }
# 
# # #--------------------Overall Marginals------------------------------------------
# # pars <- as.matrix(expand.grid(seq(1, 4, .025), seq(.1, 2, .025)))
# # 
# # overall <- ddply(turkdata, .(test_param), get_posterior_density, pars=pars)
# # overall.mean <- ddply(overall[,-3], .(test_param, mean), summarise, f=sum(f))
# # overall.mean <- ddply(overall.mean, .(test_param), transform, f=f/sum(f))
# # 
# # # Posterior marginal distribution over individual and std. deviation (does not consider individual in the data at all - equivalent to a simple linear model as opposed to a mixed model)
# # qplot(data=overall.mean, x=mean, y=f, geom="line", colour=test_param, group=test_param) + 
# #   scale_colour_discrete("Function Type") + 
# #   xlab("Mean Psychological Lie Factor") + ylab("Density") + theme_bw()  + theme(legend.position="bottom")  + facet_wrap(~test_param, scales="free")
# # # ggsave("figure/fig-OverallMeans.pdf", width=4, height=4, units="in")
# # # 
# # # overall.sd <- ddply(overall[,-2], .(test_param, sd), summarise, f=sum(f))
# # # overall.sd <- ddply(overall.sd, .(test_param), transform, f=f/sum(f))
# # 
# # # # Posterior marginal distribution over individual and mean
# # # qplot(data=overall.sd, x=sd, y=f, geom="line", colour=factor(test_param), group=test_param)
# # 
# # # Posterior joint dist of mean, sd over individuals
# # # since stat_density2d won't use weights, ... improvise!
# # overall.joint.sample <- sample(1:nrow(overall), size=50000, replace=TRUE, prob=overall$f)
# # 
# # ggplot(data=overall, aes(x=mean, y=sd)) + geom_tile(aes(fill=f))+facet_wrap(~test_param)+scale_fill_continuous(trans="log")
# # 
# # ggplot(data=overall[overall.joint.sample,1:3], aes(x=mean, y=sd)) + 
# #   stat_density2d(n=c(1000, 1000), geom="density2d", aes(colour=test_param)) + 
# #   facet_wrap(~test_param, scales="free") + scale_colour_discrete(guide="none") + theme_bw() + 
# #   xlab("Mean Psychological Lie Factor") + ylab("Std. Deviation") 
# # ggsave("figure/fig-Joint2dDensity.pdf", width=4, height=4, units="in")
# # 
# # #--------------------Individual Distribution of Theta---------------------------
# 
# library(plyr) 
# library(doMC) 
# registerDoMC(cores=12) 
# 
# turkdata <- ddply(turkdata, .(ip.id, test_param), transform, n=length(test_param))
# turkdata <- subset(turkdata, n>4)
# 
# pars <- as.matrix(expand.grid(seq(1, 4, .01), seq(.1, 2, .025)))
# 
# test <- ddply(turkdata, .(ip.id, test_param), get_posterior_density, pars=pars, .parallel=TRUE)
# 
# test.mean <- ddply(test, .(ip.id, test_param, mean), summarise, f=sum(f), .parallel=TRUE) # "integrate out" std dev. 
# test.mean <- ddply(test.mean, .(ip.id, test_param), transform, f=f/sum(f), .parallel=TRUE) # normalize
# 
# overall.mean.f <- ddply(test, .(test_param, mean), summarise, f=sum(f), .parallel=TRUE) # integrate out participant and sd
# overall.mean.f <- ddply(overall.mean.f, .(test_param), transform, f=f/sum(f), .parallel=TRUE) #normalize
# 
# participants <- dcast(ddply(turkdata, .(ip.id, test_param), summarise, n=length(test_param)), ip.id~test_param, value.var="n")
# ipsubset <- subset(participants, rowSums(is.na(participants))==0 & rowSums(participants[,2:4]>6, na.rm=TRUE)==3)$ip.id
# 
# par_labeller <- function(var, value){
#   n <- sapply(value, function(i) sum(subset(participants, ip.id%in%i)[,2:4]))
#   value <- paste("Participant ", as.character(value), "\n(n = ", n, ")", sep="")
#   return(value)
# }
# 
# Plot 4 individuals who did at least 6 figures of each trial 
# test.mean$test_param <- factor(test.mean$test_param, levels=c("Sin", "Exp", "Inv"))
# test.mean$functions <- factor(c("Sine", "Exponential", "Inverse")[as.numeric(test.mean$test_param)], levels=c("Sine", "Exponential", "Inverse"))
# qplot(data=subset(test.mean, ip.id%in%ipsubset), x=mean, y=f, group=functions, colour=functions, geom="line") + 
#   facet_grid(.~ip.id, labeller=par_labeller) + scale_colour_discrete("Function Type") + theme_bw() + 
#   theme(legend.position="bottom", text=element_text(size=10)) + ylab("Density") + scale_x_continuous(breaks=c(1, 1.5, 2), limits=c(1, 2), name="Mean Psychological Lie Factor")
# ggsave("figure/fig-IndivMeanAllFcns.pdf", width=6, height=3)      
# 
# 
# 
# # Posterior predictive theta estimates, including CI information for the individual MEAN 
# # (i.e. not for any individual observation)
# test.post.indiv<- ddply(test.mean, .(ip.id, test_param), 
#                         function(x){
#                           ex=sum(x$mean*x$f)
#                           n=100
#                           samp <- matrix(sample(x$mean, n*11, prob=x$f, replace=TRUE), ncol=11)
#                           z <- as.numeric(quantile(rowMeans(samp), c(.025, .5, .975)))
#                           data.frame(ip.id=unique(x$ip.id), test_param=unique(x$test_param), lb=z[1], mean = ex, median=z[2], ub=z[3], n=n)
#                           })
# 
# 
# overall.mean.bounds <- ddply(test.mean, .(test_param), function(x){
#   ex=sum(x$mean*x$f)
#   n=length(unique(turkdata$ip.id[which(turkdata$test_param==unique(x$test_param))]))
#   samp <- matrix(sample(x$mean, n*11, prob=x$f, replace=TRUE), ncol=11)
#   sample.mean = mean(samp)                          
#   sdev = sd(rowMeans(samp))/sqrt(n)
#   lb = as.numeric(quantile(rowMeans(samp), .025))
#   med = as.numeric(quantile(rowMeans(samp), .5))
#   ub = as.numeric(quantile(rowMeans(samp), .975))
#   data.frame(lb=lb, mean=sample.mean, median=med, ub=ub)
# })
# 
# test.post.indiv$functions <- c("Inverse", "Sine", "Exponential")[as.numeric(test.post.indiv$test_param)]
# test.post.indiv$functions <- factor(test.post.indiv$functions, levels=c("Sine", "Exponential", "Inverse"))
# overall.mean.bounds$functions <- c("Exponential", "Inverse", "Sine")[as.numeric(factor(overall.mean.bounds$test_param))]
# overall.mean.bounds$functions <- factor(overall.mean.bounds$functions, levels=c("Sine", "Exponential", "Inverse"))
# 
# qplot(data=test.post.indiv,  x=lb, xend=ub, y=ip.id, yend=ip.id, geom="segment", colour=functions) + 
#   facet_wrap(~functions) + geom_point(aes(x=median), colour="black") + 
#   geom_vline(data=overall.mean.bounds, aes(xintercept=lb), linetype=3) + 
#   geom_vline(data=overall.mean.bounds, aes(xintercept=median)) + 
#   geom_vline(data=overall.mean.bounds, aes(xintercept=ub), linetype=3) + 
#   ylab("Participant ID") + xlab("Mean Lie Factor") + theme_bw() + theme(legend.position="none", text=element_text(size=10)) + 
#   scale_colour_discrete("Function Type")
# ggsave("figure/fig-CIindivMean.pdf", width=6, height=4, units="in")
# # 
# # # Posterior estimates, including CI information for the individual observations 
# # # (i.e. not for any individual observation)
# # # indiv.value.bounds <- ddply(test.mean, .(ip.id, test_param), function(x){
# # #   lb=x$mean[which.min(abs(cumsum(x$f)-.025))]
# # #   med=x$mean[which.min(abs(cumsum(x$f)-.5))]
# # #   ub=x$mean[which.min(abs(cumsum(x$f)-.975))]
# # #   data.frame(lb=lb, median=med, ub=ub)
# # # })
# # # 
# # # overall.value.bounds <- ddply(overall.mean.f, .(test_param), function(x){
# # #   xnew <- sample(x$mean, length(x$mean), prob=x$f, replace=TRUE)
# # #   z <- as.numeric(quantile(xnew, c(.025, .5, .975)))
# # #   data.frame(lb=z[1], median=z[2], ub=z[3])
# # # })
# # # # Posterior Distribution for theta without averaging over individuals
# # # qplot(data=overall.mean.f, x=mean, y=f, geom="line", colour=test_param) + 
# # #   xlab("Psychological Lie Factor\nEstimated Distribution for All Individuals") + 
# # #   theme_bw() + theme(legend.position="bottom") + scale_color_discrete("Function Type") + 
# # #   ylab("Density")
# # # 
# # # qplot(data=indiv.value.bounds,  x=lb, xend=ub, y=ip.id, yend=ip.id, geom="segment", colour=test_param) + 
# # #   facet_wrap(~test_param) + geom_point(aes(x=median), colour="black") + 
# # #   geom_vline(data=overall.value.bounds, aes(xintercept=lb), linetype=3) + 
# # #   geom_vline(data=overall.value.bounds, aes(xintercept=median)) + 
# # #   geom_vline(data=overall.value.bounds, aes(xintercept=ub), linetype=3) + 
# # #   ylab("Participant ID") + xlab("Lie Factor") + theme_bw() + theme(legend.position="bottom") + 
# # #   scale_colour_discrete("Function Type")
# # 
# # 
# test.mean.marginal <- test.mean
# # test.mean.marginal <- ddply(test.mean.marginal, .(ip.id, test_param), summarise, mean=mean, f=f/sum(f), .parallel=TRUE)
# test.mean.marginal$functions <- c("Exponential", "Inverse", "Sine")[as.numeric(as.factor(test.mean.marginal$test_param))]
# test.mean.marginal$functions <- factor(test.mean.marginal$functions, levels=c("Sine", "Exponential", "Inverse"))
# overall.mean$functions <- c("Exponential", "Inverse", "Sine")[as.numeric(as.factor(overall.mean$test_param))]
# 
# ggplot() + 
#   geom_line(data=test.mean.marginal, aes(x=mean, y=f, group=ip.id, colour=functions), alpha=I(.175)) + 
#   geom_line(data=overall.mean, aes(x=mean, y=f, group=functions), colour="black") + 
#   facet_wrap(~functions, scales="free_y") + ylab("Density") + xlab("Lie Factor") + theme_bw() + scale_colour_discrete("Function Type") +
#   theme(legend.position="none", text=element_text(size=10)) + xlim(c(1, 2.5)) + 
#   guides(colour = guide_legend(override.aes = list(alpha = 1)))
# ggsave("figure/fig-spaghettiIndivDists.pdf", width=6, height=3, units="in")

# # 
# # posterior.modes <- ddply(test, .(ip.id, test_param), summarise, theta=mean[which.max(f)])
# # 
# # qplot(data=posterior.modes, x=theta, geom="line", stat="density", colour=test_param, group=test_param) + 
# #   scale_colour_discrete("Function Type") + facet_grid(.~test_param) + 
# #   xlab("Distribution of Individual Mode Psychological Lie Factor") + ylab("Density") + theme_bw()  + theme(legend.position="bottom") 
# # ggsave("figure/fig-OverallModes.pdf", width=4, height=4, units="in")


# save.image("./code/BayesAnalysis.Rdata")

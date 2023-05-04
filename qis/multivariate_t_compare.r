rm(list = ls())

d = 50; r = 100; N = 20
setwd("~/R/Yakowitz")
load(paste0("RData/Multivariate_t","d",d,"r",r,"N",N,".RData"))

set.seed(90210)
# Set global R options
options(scipen = 4)

library(truncnorm)
library(pracma)
library(ggplot2)

vertical.grid = function(l,N,type = NULL){
  # "u" - uniform
  # "e" - exponential
  
  if(type == "u"){
    ugrid = runif(N)
    res = c(sort(ugrid),1)
    # res = sort(runif(N))
  }else if(type == "e"){
    res = exp(-(0:l)/N)
  }
  
  return(res)
}
# Quantile
sQ = function(q,Y){
  # q-quantile of Y
  N = length(Y)
  res = Y[ceiling(N*q)]
  return(res)
}

# Test example
# Prior and Likelihood

d = 50
tau = 1
nu = 2

trueZ <- 1.9445572*10^(-29) ## U(26,2,1), d = 50, nu = 2, s = 1

# trueZ <- 2.62*10^(-9) ## U(11,2,1), d = 20, nu = 2, s = 1
# trueZ <- 4.57*10^(-37) ## U(31, 2, 1), d = 60, nu = 2, s = 1

dtmvr <- function(x, nu){
  d = length(x)
  logden = -0.5*(nu + d)*log(1 + (t(x)%*%x)/nu)
  return(exp(logden))
}

library(LaplacesDemon)

# dtmvr <- function(x, nu){
#   d = length(x)
#   mu = rep(0,d)
#   S = diag(d)
#   return(exp(dmvt(x, mu, S, df=nu, log=T)))
# }

r = 100
# d = 50

mc.naive = NULL
verbose = TRUE
for(i in 1:r){
  if(isTRUE(verbose) && i %% 10 == 0)
    cat("Iteration ",i, "\n")
  M = 10000
  # X = rmvn(M, rep(0,d), eye(d))
  Y = numeric(M)
  for(j in 1:M){
    Y[j] = dtmvr(x = rnorm(d, 0, 1), nu = 2)
  }
  mc.naive = c(mc.naive,mean(Y))
}

mean(mc.naive)
summary(mc.naive)

hist(mc.naive)
abline(v = trueZ)

## QIS 

N = 20
# r = 100

mc.qis = NULL
verbose = TRUE

# simu.grid.exp = vertical.grid(N,N,"e")
# simu.grid.unif = c(0,sort(runif(N)),1)

simu.grid.unif = vertical.grid(l=NULL,N,type = "u")

for(i in 1:r){
  
  if(isTRUE(verbose) && i %% 10 == 0)
    cat("Iteration ",i, "\n")
  M = 10000
  # X = rmvn(M,rep(0,d), eye(d))
  Y = numeric(M)
  for(j in 1:M){
    Y[j] = dtmvr(x = rnorm(d, 0, 1), nu = 2)
  }
  Y = sort(Y)
  Lambda = sQ(simu.grid.unif,Y)
  
  x = simu.grid.unif
  y = Lambda
  # Use a correction term at the boundary: -h^2/12*(f'(b)-f'(a))
  # h  <- x[2] - x[1]
  # ca <- (y[2]-y[1]) / h
  # cb <- (y[N]-y[N-1]) / h
  # YakoMC <- trapz(x, y) - h^2/12 * (cb - ca)
  YakoMC <- trapz(x, y)
  
  # mc.qis = c(mc.qis, trapz(simu.grid.unif,Lambda)) ## QIS Original 
  mc.qis = c(mc.qis, YakoMC) ## QIS Corrected
}

cbind(mean(mc.qis), mean(mc.naive), trueZ)

hist(log(mc.qis), breaks = 30)
abline(v = log(trueZ))

# cat("QIS MAPE",mape.qis ,"\n",
#     "Naive NAPE",mape.naive, "\n")
# 
# cat("QIS RMSE", rmse.qis,"\n",
#     "Naive RMSE", rmse.naive, "\n")



boxplot(data.frame(MC=mc.naive,Yakowitz=mc.qis), log = "y", main = "N = 10000") #N=10000
abline(h=trueZ,col="red",lwd=2)

## Trim top and bottom 2.5%

trim_ul <- function(x, p = 0.01){
   x <- x[x >= quantile(x, p) & x <= quantile(x, 1-p)]
   return(x)
}
  
mean(trim_ul(mc.qis, p = 0.025))
mean(trim_ul(mc.naive, p = 0.025))

## GGPLOT

source("~/R/Yakowitz/vertical_funs.r")

library(ggplot2)
mc.data = rbind(data.frame(Z = trim_ul(mc.qis, p = 0.025), method = "QIS"),
                data.frame(Z = trim_ul(mc.naive, p = 0.025), method = "Naive MC"))

(mvt.plt <- ggplot(mc.data, aes(Z, fill = method)) + 
    geom_histogram(alpha=0.75, bins = 20, position="identity",aes(y = after_stat(density)))+
    geom_density(alpha=0.75, adjust = 2/3, position="identity",aes(y = after_stat(density)))+
    # expand_limits(x = c(1e-33,1e-27))+
        geom_vline(xintercept=trueZ)+scale_x_log10()+
    # coord_flip()+ 
    facet_wrap(vars(method), ncol = 1, scales = "free_y")+
    theme_bw()+
    theme(legend.position = "bottom")+
    labs(title = "QIS vs. Naive MC", subtitle = "Multivariate t, MVN prior")+
    plotTheme())

ggsave(paste0("~/R/Yakowitz/art/","qis_vs_naive_mvt",N,r,"d",d,".pdf"), mvt.plt, width = 9, height = 7, device = cairo_pdf)
ggsave(paste0("~/R/Yakowitz/art/","qis_vs_naive_mvt",N,r,"d",d,".eps"), mvt.plt, width = 9, height = 7, device = cairo_ps)
ggsave(paste0("~/R/Yakowitz/art/","qis_vs_naive_mvt",N,r,"d",d,".png"), mvt.plt, width = 9, height = 7)

## Numerical 

mean.qis <- mean(mc.qis, trim = 0.025); mean.naive <- mean(mc.naive,trim = 0.025)
median.qis <- median(mc.qis); median.naive <- median(mc.naive)
mape.qis <- median(abs((mc.qis)-(trueZ))/(trueZ))
mape.naive <- median(abs(((mc.naive)-(trueZ))/(trueZ)))
rmse.qis <- sqrt((mean((mc.qis-trueZ)^2)))
rmse.naive <- sqrt((mean((mc.naive-trueZ)^2)))

perf <- rbind((cbind(mean.qis,mean.naive)),
              (cbind(median.qis,median.naive)),
              (cbind(mape.qis, mape.naive)),
              (cbind(rmse.qis,rmse.naive)))

colnames(perf) <- c("QIS", "Naive"); row.names(perf) <- c("Mean", "Median",
                                                          "MAPE", "RMSE")
perf

library(knitr)
library(kableExtra)

kable(perf, format = "latex", digits = 99)

save.image(paste0("RData/Multivariate_t","d",d,"r",r,"N",N,".RData"))

load(paste0("RData/Multivariate_t","d",d,"r",r,"N",N,".RData"))

library(PupillometryR)

(mvt.plt.2 <- mc.data%>%
  ggplot() +
  aes(x = method,
      y = MC,
      fill = method) +
  geom_point(aes(color = method),
             position = position_jitter(w = 0),
             size = 1,
             alpha = 0.8) +
  geom_boxplot(width = .2,
               outlier.shape = NA,
               alpha = 0.5) +
  geom_flat_violin(position = position_nudge(x = .2),
                   alpha = 0.7,
                   adjust = 0.5)  +
    scale_y_log10()+
  geom_hline(yintercept=trueZ)+ #coord_flip()+
  coord_flip() +
  # scale_x_discrete(expand = c(0,0)) +
  labs(x = "",
       title = "QIS vs. Naive MC") +
  theme(legend.position = "bottom") + theme_minimal())


setwd("~/R/Yakowitz/art")
ggsave("convergence248_new.eps",conv.plt, width = 7, height = 7, device = cairo_ps)
ggsave("convergence248_new.pdf",conv.plt, width = 7, height = 7, device = cairo_pdf)


# library(knitr); library(dplyr)

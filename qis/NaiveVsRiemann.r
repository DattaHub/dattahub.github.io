rm(list = ls())
set.seed(09012)
library(pracma)

source("C:/Users/jyotishka/OneDrive/Documents/R/Yakowitz/vertical_funs.r")



## Example 1: Arbitrary Integral
# underlying uniforms

# Define L
a = 3
b = 3

L <- function(u){u^{a-1}*(1-u)^{b-1}}

# L <- function(x){exp(x*log(x))}

## Define true value 
trueZ <- beta(a,b)

# trueZ <- 1

N.set = seq(10, 500,by = 10)

naiveMC <- numeric(length(N.set))
YakoMC <- numeric(length(N.set))

verbose = T


for (i in 1:length(N.set)){
  
  if(isTRUE(verbose) && i %% 10 == 0)
    cat("Size",N.set[i], "\n")
  U <- runif(N.set[i], 0, 1)
  
  # monte carlo approximation
  naiveMC[i] <-  mean(L(U))
  
  ## Yakowitz 
  
  # sortU = sort(U)
  # YakoMC[i] <- trapz(sortU,L(sortU))
  
  n <- N.set[i]
  x = sort(U)
  y = L(x)
  # Use a correction term at the boundary: -h^2/12*(f'(b)-f'(a))
  h  <- x[2] - x[1]
  ca <- (y[2]-y[1]) / h
  cb <- (y[n]-y[n-1]) / h
  YakoMC[i] <- trapz(x, y) - h^2/12 * (cb - ca)
  
  
}

plot(naiveMC, type = "l", lty = "dotdash")
lines(YakoMC, col = "red", lty = "longdash")

library(ggplot2)

mc.data = rbind(data.frame(MC = rep(trueZ, length(N.set)), method = "True Value"),
                data.frame(MC = naiveMC, method = "Empirical Average"),
                data.frame(MC = YakoMC, method = "Riemann sum: Yakowitz"))

mc.data = cbind(mc.data, iteration = N.set)

(plt <- ggplot(mc.data, aes(y= MC, x = iteration, color = method, linetype = method)) + 
    geom_line(linewidth = rel(0.8))+
    scale_color_discrete(type = "qual")+
    scale_linetype_manual(values =c("dotted","dashed","solid"))+
    # geom_histogram(alpha=0.75,binwidth = 0.005, position="identity",aes(y = ..density..))+
    # geom_density(alpha=0.75,adjust = 1/2, stat="density",position="identity",aes(y = ..density..))+
    # geom_hline(yintercept=trueZ, linetype = "dashed")+ #coord_flip()+
    xlim(10,500)+
    # facet_grid(~method)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ggtitle("Empirical Average vs. Riemannian sum") 
  )

setwd("C:/Users/jyotishka/OneDrive/Documents/R/Yakowitz")

ggsave(filename = "art/NaiveVsRiemann.pdf", plt, width = 7, height = 5, device = cairo_pdf)
ggsave(filename = "art/NaiveVsRiemann.eps", plt, width = 7, height = 5, device = cairo_ps)


(plt.bw <- ggplot(mc.data, aes(y= MC, x = iteration, linetype = method)) + 
    geom_line(linewidth = rel(0.5))+
    scale_linetype_manual(values =c("dotted","longdash","solid"))+
    # geom_histogram(alpha=0.75,binwidth = 0.005, position="identity",aes(y = ..density..))+
    # geom_density(alpha=0.75,adjust = 1/2, stat="density",position="identity",aes(y = ..density..))+
    # geom_hline(yintercept=trueZ, linetype = "dashed")+ #coord_flip()+
    xlim(10,500)+
    # facet_grid(~method)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ggtitle("Empirical Average vs. Riemannian sum") 
)

setwd("C:/Users/jyotishka/OneDrive/Documents/R/Yakowitz")

ggsave(filename = "art/NaiveVsRiemann_BW.pdf", plt.bw, width = 7, height = 5, device = cairo_pdf)
ggsave(filename = "art/NaiveVsRiemann_BW.eps", plt.bw, width = 7, height = 5, dpi = 300, device = cairo_ps)



# # R's numerical integration approximation
# 
# h <- function(u) sin(u*cos(u))
# integrate(h, 0, 1)$val
# 
# curve(2*pi*sin(x*cos(x)))


#### Lorenz



rm(list = ls())
set.seed(09012)
source("C:/Users/jyotishka/OneDrive/Documents/R/Yakowitz/vertical_funs.r")

## Example 1: Expectation(1/1+X) where X is Gamma(alpha, beta)
## E(X) = beta^{alpha} e^{\beta} Upper_Gamma(1-\alpha, \beta)
# underlying uniforms

library(pracma)

## Define L

# a = 0.9
# b = 10

# a <- 1.5;  b <- 1
# sy <- 66 ; n <- 44

L <- function(u){1/(1+u)} 

# L <- function(u){exp(-u^2)}

# L <- function(u){1*(u<1.75)} ## Peter Hoff Example
# theta.mc1000 <- rgamma(1000 , a+sy , b+n)
# mean(L(theta.mc1000))



## Define true value 

# (trueZ <- b^a*exp(b)*gammainc(b,1-a)[["uppinc"]])
# (trueZ <- b^a*exp(b)*incgam(b,1-a))

# (trueZ <- pgamma(1.75,a+sy,b+n)) ##PH example

# (trueZ <- integral(function(u){L(u)*exp(-u)}, 0, Inf))

(trueZ <- exp(1)*expint_E1(1))

## log(2) 1/2*log(3)

N.set = seq(10, 500,by = 10)

naiveMC <- numeric(length(N.set))
YakoMC <- numeric(length(N.set))

verbose = T


for (i in 1:length(N.set)){
  
  if(isTRUE(verbose) && i %% 10 == 0)
    cat("Size",N.set[i], "\n")
  n <- N.set[i]
  # U <- rgamma(N.set[i], a, b)
  
  U <- rexp(N.set[i])
  
  # monte carlo approximation
  naiveMC[i] <-  mean(L(U))
  
  ## Yakowitz 
  
  # x = sort(U)
  # y = L(x)
  sortU = sort(U)
  # YakoMC[i] <- trapz(sortU,L(sortU))
  
  YakoMC[i] <- trapz(sortU,L(sortU)*exp(-1*sortU))
  
  # # Use a correction term at the boundary: -h^2/12*(f'(b)-f'(a))
  # h  <- x[2] - x[1]
  # ca <- (y[2]-y[1]) / h
  # cb <- (y[n]-y[n-1]) / h
  # YakoMC[i] <- trapz(x, y) - h^2/12 * (cb - ca)
  
  
  # Y = L(U)
  # Y = sort(Y)
  # simu.grid = vertical.grid(l=NULL, N.set[i], type = "u")
  # 
  # Lambda = sQ(simu.grid,Y)
  # YakoMC[i] <-trapz(simu.grid,Lambda)
  
}

plot(naiveMC, type = "l", lty = "dotdash")
lines(YakoMC, col = "red", lty = "longdash")
abline(h = trueZ)

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

ggsave(filename = "art/NaiveVsRiemannExpInt.pdf", plt, width = 7, height = 5, device = cairo_pdf)
ggsave(filename = "art/NaiveVsRiemannExpInt.eps", plt, width = 7, height = 5, device = cairo_ps)

## BW plot 

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

ggsave(filename = "art/NaiveVsRiemannExpInt_BW.pdf", plt.bw, width = 7, height = 5, device = cairo_pdf)
ggsave(filename = "art/NaiveVsRiemannExpInt_BW.eps", plt.bw, width = 7, height = 5, device = cairo_ps)



##
## Uniform grid (Yakowitz) + trapezoid, but no nested. 
r = 1000
N = 100
mc = NULL
verbose = TRUE
for(i in 1:r){
  
  if(isTRUE(verbose) && i %% 10 == 0)
    cat("Iteration ",i, "\n")
  M = 1000
  X = rgamma(M, shape = a, rate = b)
  Y = L(X)
  Y = sort(Y)
  
  simu.grid = vertical.grid(l=NULL,N,type = "u")
  
  Lambda = sQ(simu.grid,Y)
  mc = c(mc,trapz(simu.grid,Lambda))
}

mean(mc)

mc2 = NULL
for(i in 1:r){
  if(isTRUE(verbose) && i %% 10 == 0)
    cat("Iteration ",i, "\n")
  X = rgamma(M, shape = a, rate = b)
  Y = L(X) 
  mc2 = c(mc2,sum(Y)/N)
}

mean(mc2)
sd(mc2)
sqrt(mean((mc2-trueZ)^2))
hist(mc2,breaks=30,main = "MC")
abline(v=trueZ,col="red",lwd=2)


mean(mc)
sd(mc)
sqrt(mean((mc-trueZ)^2))
hist(mc,breaks=30,main = "MC + Yakowitz")
abline(v=trueZ,col="red",lwd=2)


# # R's numerical integration approximation
# 
# h <- function(u) sin(u*cos(u))
# integrate(h, 0, 1)$val
# 
# curve(2*pi*sin(x*cos(x)))


#### Lorenz

# Generate a Normal random variable with mean 0 and standard deviation 1
n <- 10
x <- runif(n, 0, 3)

# Sort the random variable in ascending order
x_sorted <- sort(x)

# Compute the cumulative proportions of the sorted variable
cum_prop <- cumsum(x_sorted) / sum(x_sorted)

# Compute the cumulative proportion of the population
pop_prop <- seq(0, 1, length.out = n)

# Plot the Lorenz curve
plot(pop_prop, cum_prop, type = "l", lwd = 2, xlab = "Population proportion", ylab = "Cumulative proportion of variable")
abline(0, 1, lty = 2)

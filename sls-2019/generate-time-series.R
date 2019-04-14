rm(list = ls())
set.seed(123)
## Simple AR(1) model
phi = 0.9
nu = 1
T = 100
x = numeric(T)
for(t in 1:(T-1)){
  x[t+1] = phi*x[t]+rnorm(1,0,nu)
}
par(mfrow=c(1,2))
plot(x, type = "l")
acf_x = acf(x, lag.max = 30, plot = F)
plot(acf_x, type ="l")
par(mfrow=c(1,1))

## Simple HMM 

w = 2
y = x + rnorm(T, 0, w)
plot(y, type = "l", main = "HMM")
lines(x, col = "blue", lwd = 2)
legend("topleft",c("y","x"), lty = 1, lwd = c(1,2), col = c(1,4))

## Simple Stochastic Volatility 

mu = 2; 
sigma_t = exp(mu + x)
y = rnorm(T, 0, sigma_t)

par(mfrow=c(1,2))
plot(y, type ="l", main = "y")
plot(x, type ="l", col = 2, lwd = 2, main = "SVM")
lines(log(y^2)/2, col = 4, lwd = 2)
legend("topleft",c("log(|y|)","x"), lty = 1, lwd = c(2,2), col = c(2,4))
par(mfrow=c(1,1))

## animate AR models 

## Simple AR(1) model
rm(list = ls())
phiset = c(0.9,-0.9,0.3)
nu = 2
T = 1000
x = matrix(0,3,T)

for (k in 1:3){
  phi = phiset[k]
  for(t in 1:(T-1)){
    x[k, t+1] = phi*x[k, t]+rnorm(1,0,nu)
  }
}

acf_data = apply(x,1, function(z) {acf(z, plot = F)$acf})

df = rbind(data.frame(values = acf_data[,1], phi = "0.9", lag = seq_along(acf_data[,1])),
           data.frame(values = acf_data[,2], phi = "-0.9", lag = seq_along(acf_data[,1])),
           data.frame(values = acf_data[,3], phi = "0.3", lag = seq_along(acf_data[,1])))

# df = rbind(df, lag = seq(1,31))

# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')

library(gganimate)
library(gifski)
library(png)
library(transformr)

ggplot(df, aes(x = lag, y = values, group = phi))+
  geom_line()+
  # Here comes the gganimate specific bits
  geom_segment(aes(xend = 31, yend = values), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 31.1, label = phi), hjust = 0) + 
  transition_reveal(lag) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Temperature in New York', y = 'Temperature (°F)') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
  

par(mfrow=c(1,2))
plot(x, type = "l")
plot(acf(x, plot = F))
par(mfrow=c(1,1))

plotTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 12, family = "sans", face = "italic", hjust = 0), 
    axis.title.x = element_text(size = rel(1.1), family = "sans", face = "plain", hjust = 1, vjust = -0.5),
    axis.title.y = element_text(size = rel(1.1), family = "sans", face = "plain", hjust = 1, vjust = 1),
    axis.text = element_text(size = rel(1.1), family = "sans", face = "plain"),
    panel.background = element_blank(),
    # panel.grid.minor = element_line(colour = "gray"),
    panel.grid.minor = element_blank(),
    # panel.grid.major = element_line(colour = "gray"),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 10, family = "sans"),
    axis.line = element_blank()
  )
}

## Example
# Generate grid
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
# Nested sampling (normal prior, normal likelihood)
nested_sampling = function(mu1,sigma1,mu2,sigma2,N,tol=0.001){
  # "mu1", "sigma1" - likelihood
  # "mu2", "sigma2" - prior
  
  Lmax = 1/(sqrt(2*pi)*sigma1)
  
  theta = rnorm(N,mu2,sigma2)
  L = dnorm(theta,mu1,sigma1)
  phi = NULL
  error = 1
  
  while(error >= tol)
  {
    index = which.min(L)
    Lmin = min(L)
    phi = c(phi,Lmin)
    
    error = abs(Lmin-Lmax)/Lmax
    
    term = -log(Lmin*sqrt(2*pi)*sigma1)
    a = mu1 - sqrt(term*2*sigma1^2) 
    # a = ifelse(a>0, a, 0)
    # a = a + abs(a)
    b = mu1 + sqrt(term*2*sigma1^2)
    
    newTheta = rtruncnorm(1,a,b,mean = mu2,sd = sigma2)
    newL = dnorm(newTheta,mu1,sigma1)
    theta[index] = newTheta
    L[index] = newL
  }
  return (phi)
}
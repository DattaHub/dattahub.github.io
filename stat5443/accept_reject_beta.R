# Always remember to initialize the random number generator
set.seed(345687)


# This function performs accept reject sampling
# dist = the distribution function
# minsup = minimum value of support
# maxsup = maximum value of support
# maxdist = maximum value of the distribution
# n = number of desired samples
arsampler <- function(dist, minsup, maxsup, maxdist, n){
  rv=rep(0,n)
  for(i in 1:n){
    while(1){
      x=runif(n = 1, min = minsup, max = maxsup)
      u=runif(n=1, min =0 , max=maxdist)
      if(u < dist(x)){
        rv[i]=x
        break
      }
    }
  }
  return(rv)  
}

betafun <- function(x, a=2, b=5){
  return(x^(a-1)*(1-x)^(b-1)/beta(a, b))
}


# Let us generate samples from the beta distribution
a=2
b=5

## Verify for yourself that the maximum of the Beta distribution is
## attained at x = (a-1/a+b-2) provided a > 1

maxx=(a-1)/(a+b-2)
maxdist=betafun(maxx)
n=10000
x=arsampler(betafun, 0, 1.0, maxdist,n)
hist(x,freq=F) # Histogram of the samples
curve(betafun, add = T) ##Plotting the density function to verify
#plot(density(x))



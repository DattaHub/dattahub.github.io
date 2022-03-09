# don't forget comments!

5                # just repeats

1+3+5            # calculator

x <- 5           # assignment -- my bias against =

x = 5 

x == 6

x                # print me x

x+3              # add

x*x              # multiply

x^3              # power

x^x              

y <- x + 7

y/3              # divide

y^2 -2*y + 4

# rounding

(.1 + 2/5) == .5

(.1 + 1/5) == .5

(.7 - .2) == .5 

(.7 - .2) - .5


## Why doesn't R think these numbers are equal?
b <- 1 - 0.8
b == 0.2
b - 0.2

## The only numbers that can be represented exactly in R's numeric
# type are integers and fractions whose denominator is a
# power of 2. Other numbers have to be rounded to (typically) 53
# binary digits accuracy. As a result, two floating point numbers
# will not reliably be equal unless they have been computed by
# the same algorithm, and not always even then. For example

a <- sqrt(2)
a * a == 2
a * a - 2

# use some functions

sqrt(x)

log(1+x/1000)     # recall approx of log(1+z)

sin(pi/3)

asin(1/2)/pi      # can I ever remember?

# what's there?

ls()            # list objects

rm(x)           # remove an object 

rm(list=ls())   # clean up

#
# Key functions:
#  c(): makes a vector
#  seq: create a sequence
#  rep: repeat

v <- c(-2, -1, 0, 1, 2)     # *** important function c() ***

vmore <- c(v,0,v,3)

v <- (-2:2)                 # sequence  (or use seq(-2,2) )

v <- seq(-2,2)

v <- seq(-2,2,1)            # seq(start, finish, by)

seq(-100,100, by = 10)

seq(0, 20, length.out = 1000)

w <- seq(along.with=v)      # from 1 to length(along.with)

w <- seq_along(v)           # short form of above

ones <- rep(1,5)            # repeat rep(what,howmany)

doublev <- rep(v,2)         # with vector, scalar

dmore <- rep(c(1,2),c(3,4)) # repeat each in accordingly

longer <- rep(v,seq_along(v))# lengths must match

z1 <- rep(1:2,3)            # 3 times (1,2)

z2 <- rep(1:2,rep(3,2))     # repeat within

z2 <- rep(1:2,each=3)       # same as above

v + w

v * w                       # elementwise multiplication

v/w                         # elementwise

1/w                         # elementwise, recycle 1 as needed

outer(v,w)                   # outer product  v w' (or v%o%w)

crossprod(w,v)               # inner product
t(w) %*% v                   # inner product

##### indexing and subsets
v[2:4]

v > 1

w[ v >  1 ]                   # example of coercion

is.vector(w)                 # is this thing a vector?

(1:10)[c(5,3,1)]

length((1:10)[c(5,3,1)])

sum((1:10)[c(5,3,1)])        # also cumsum, diff

prod((1:10)[c(5,3,1)])       # also cumprod, cummax, cummin                  

#####  Danger!!!!!!!!!!!
(1:2) + (1:3)                # c(1,2) recycled until same length

(1:2) + (1:4)                # c(1,2) recycled, but NO WARNING

rev(v)                       # in reverse order

#####  Comparisons, max, min, pmax, pmin
v <= 2                       # recycle 2 as needed to make vector

v <= w                       # vector result

max(v)                       # max element

max(v,w)                     # max element

pmax(v,w)                    # pairwise max

diff(w)                      # first differences

which.max(w)                 # not max, but whence it came

x = rnorm(10)
which.max(x)

# show the cubic spline idea with the 24-point hump data
 # load spline library
library(splines)
  # read in the hump data
df1 <- read.table('hump.csv',header=TRUE,sep=',')

x1 <- cut(df1$x, c(6.5, 12.5, 18.5))

table(x1)

plot(df1$x,df1$y)

plot(df1$x,df1$truth)

fit.lm <- lm(y ~ cut(x, c(6.5, 12.5, 18.5)), data = df1)
summary(fit.lm)

summary(df1)

# make a dense prediction grid that goes past the boundaries
pred.df = data.frame(x=seq(-2,27,length=200))

# fit a cubic spline and get predictions
fit2=lm(y~bs(x,knots=10.5),data=df1)
pred2=predict(fit2,newdata=pred.df,se=T)
  # make a plot
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,1,1),pty='m')
plot(df1$x,df1$y,xlab='x',ylab='y',xlim=c(-2,27))  # data
lines(pred.df$x, pred2$fit,col="blue",lwd=2)       # fit
lines(df1$x, df1$truth, col = "magenta")
abline(v=10.5,lty=3,col='grey40')                  # knot location

# natural spline - notice the edge effects (linear at edge)
fit2=lm(y~ns(x,knots=10.5),data=df1)  
  # get fit
pred2=predict(fit2,newdata=pred.df,se=T)
  # plot it
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,1,1),pty='m')
plot(df1$x,df1$y,xlab='x',ylab='y',xlim=c(-2,27))
lines(pred.df$x, pred2$fit,col="blue",lwd=2)
  # show boundary knots as well.  By default they are at the
  # edges of the dataset; see info on ns()
abline(v=c(1,10.5,24),lty=3,col='grey40')

# fit the model using two interior knots at 1/3 and 2/3
# through the data
fit2=lm(y~ns(x,knots=c(8.5,16.5)),data=df1)
pred2=predict(fit2,newdata=pred.df,se=T)
 # make a plot
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,1,1),pty='m')
plot(df1$x,df1$y,xlab='x',ylab='y',xlim=c(-2,27))
lines(pred.df$x, pred2$fit,col="blue",lwd=2)
 # show interior and boundary knots
abline(v=c(1,8.5,16.5,24),lty=3,col='grey40')

 # last but not least, try a local regression
fit=loess(y~x,span=.3,data=df1)
fit2=loess(y~x,span=.9,data=df1)
pred.df = data.frame(x=seq(1,24,length=200))
plot(df1$x,df1$y,xlab='x',ylab='y')
lines(pred.df$x,predict(fit,pred.df), col="red",lwd=2)
lines(pred.df$x,predict(fit2,pred.df), col="blue",lwd=2)
legend("topright",legend=c("Span=0.3","Span=0.9"), col=c("red","blue"),lty=1,lwd=2,cex=.8)

 # compare the degrees of freedom for these two models:
summary(fit)
summary(fit2)


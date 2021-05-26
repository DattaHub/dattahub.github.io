

##
#### vector and matrix
##
# define a vector
x = c(121, 98, 95, 94, 102, 106, 112, 120, 108, 109)
#character vector
w = c("F","M","M","F","F","M","M","F","M","M")
w
# print the data
x

#define y to be the log transformation of x
y=log(x)

#construct a 10 by 2 matrix, with columns x and y
mat = cbind(x, y) # cbind: combine by columns
mat
#add another row to the data set
new = c(100, log(100))
mat2 = rbind(mat, new)
mat2
#check the dimension of a matrix
dim(mat)
dim(mat2)
ncol(mat2) # the number of columns of a matrix
nrow(mat2) # the number of rows of a matrix

mat[1,2] # value in the second row and the third column of mat
mat[1:3,] # the first three rows of mat
mat[,2] # the second column of mat
mat[-1,] # exclude the first row
mat[,-1] # exclude the first column

####access the documentation of a certain function

?cbind
help(cbind)
##
#### Arithmetics and simple functions
##
x + y
z = x - y
z
x*y
sum(x) # summation of all the elements in x
# summation of the 2nd, 3rd and the 5th elements of x
sum(x[c(2,3,5)])
# product of all the elements in x
prod(x)
# product of all the elements in x except the first two
prod(x[-c(1,2)])

#logic operation
sum(x) > 10
1*(x[1]>10) #returns 1 if true and 0 if false

(w == "F")
sum(w=="F") #number of "F" in the vector w

##
#### graphical analysis
##
par(mfrow=c(2,3)) # will result in 4 plots, 2 rows & 2 columns
plot(x, main="plot of x") # scatter plot of x
plot(y~x, main="y versus x") # plot y versus x
hist(x, main="hist of x") # show the histogram of x
boxplot(x, main="boxplot of x") # plot the boxplot of x
hist(y)
boxplot(y)
# some summary statistics
# calculate the sample mean (average)
mean(x)
# calculate the sample standard deviation
sd(x)
# calculate the correlation of y and x
cor(y,x)
# compute the median of x
median(x)
# compute the 90th percentile of x
quantile(x, 0.9)
# compute the 0th, 25th, 50th, 75th, 100th percentiles of x
quantile(x)
## 5 number summary
summary(x)

###################################
#### Importing and Exporting Data
###################################

# you can change the directory to where the data is saved
## Save the csv dataset in the same location where you save the R code

##setwd("C:/Users/Jyotishka/OneDrive/Documents/latex files/Course Notes/STAT4033/R")

setwd("Z:\\Course materials\\Stat4033\\R")
# This is the location of the data-set on my laptop, yours should be different!!

cigarettes = read.csv("cigarettes.csv")
##Structure of the data
str(cigarettes)
#check the dimension of the matrix
dim(cigarettes)
#print the first five rows
cigarettes[1:5,]
#check the correlation of Tar and Nicotine
cor(cigarettes[,2], cigarettes[,3])
#add another column log(Q1)
logTar = log(cigarettes[,2])
cigarettes = cbind(cigarettes, logTar)
#take a look at the first 5 rows
cigarettes[1:5,]
#export the new modified cigarettes data
write.table(cigarettes, "modified_cigarettes.csv",
            sep=",", col.names=TRUE, row.names=FALSE)
##
#### Random number generation
##
# generate 100 data points from N(mu=0, sigma=2)
?rnorm
x = rnorm(100, 0, 1)
mean(x)
max(x)
quantile(x, 0.9)
mean(x) > 1

par(mfrow=c(1,1))
hist(x)

# generate 100 data points from exponential distribution
x = rexp(100, 2)
mean(x)
max(x)
quantile(x, 0.9)
1*(mean(x) > 1)
hist(x)
abline(v=quantile(x,0.9),col="red")




x=c(37, 55, 57); y=c(23, 31, 70)

x = c(68,75,92); y = c(69,76,81)

# manual calculation
num = mean(x)-mean(y)
m=length(x); n=length(y)

Sp2 = ((m-1)*var(x)+(n-1)*var(y))/(m+n-2)
denom = sqrt(Sp2) * sqrt(1/m+1/n)
tobs = num/denom

(pval = 1-pt(tobs, m+n-2))
alpha=0.05
(critical.val = qt(1-alpha, m+n-2))

##95% confidence interval

c(num-qt(1-0.025,m+n-2)*denom, num+qt(1-0.025,m+n-2)*denom)


# or use the existing function 
t.test(x, y, var.equal=T, alternative="less") # one-sided confi. bound
t.test(x, y, var.equal=T, alternative="two.sided") 
## together with 95% two-sided confidence interval


###  Permutation test R code
?combn
combn(x = c("A","B","C","D"),m=2)
combn(x = c("A","B","C","D","E","F"),m=3)

idx = combn(x = 6, m=3)
x=c(37,55,57)
y=c(23,31,70)
# x = c(68,75,92); y = c(69,76,81)
xy = c(x,y) # the combined data set
permut = NULL # the permuted data set (a 20*6 matrix)

for(i in 1:20){
  permut = rbind(permut, c(xy[idx[,i]], xy[-idx[,i]]))
}
permut.x = permut[, 1:3] # the permuted X matrix (20*3)
permut.y = permut[, 4:6] # the permuted Y matrix (20*3)

?apply
delta1 = apply(permut.x, 1, mean) - apply(permut.y, 1, mean)
delta2 = apply(permut.x, 1, median) - apply(permut.y, 1, median)

delta1.obs = mean(x)-mean(y)
delta2.obs = median(x) - median(y)

par(mfrow=c(1,2))
hist(delta1,breaks=10);abline(v = delta1.obs,col="red")
hist(delta2,breaks=10);abline(v = delta2.obs,col="red")

#pvalue for permutation of sample mean
(pval1.upper = mean(delta1 >= delta1.obs)) #upper-tailed
(pval1.2sided = mean(abs(delta1) >= abs(delta1.obs))) #two-tailed

#pvalue for permutation of sample median
(pval2.upper = mean(delta2 >= delta2.obs)) #upper-tailed
(pval2.2sided = mean(abs(delta2) >= abs(delta2.obs))) #two-tailed


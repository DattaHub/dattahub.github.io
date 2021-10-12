challenger <- read.delim("~/Course Notes/stat5525/2021/R codes/datasets/challenger.txt")
View(challenger)

str(challenger)

library(ggplot2)
library(dplyr)

challenger.subset = challenger %>% filter(nfails.field>0)

ggplot(challenger.subset, aes(x = temp, y = nfails.field)) + 
  geom_point(aes(x = temp, y = nfails.field)) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE)


ggplot(challenger, aes(x = temp, y = nfails.field)) + 
  geom_point(aes(x = temp, y = nfails.field)) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE)


lmfit <- lm(nfails.field ~ temp, data = challenger)

par(mfrow = c(1,2), ask = FALSE) 
plot(lmfit, which = c(1,2))
par(mfrow = c(1,1))

## Now logistic 

nasa <- glm(fail.field ~ temp, family = "binomial", data = challenger)
summary(nasa)

exp(coef(nasa)) 

# Plot data
plot(challenger$temp, challenger$fail.field, xlim = c(-1, 30), xlab = "Temperature",
     ylab = "Incident probability")

# Draw the fitted logistic curve
x <- seq(-1, 30, l = 200)
y <- exp(-(nasa$coefficients[1] + nasa$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

# The Challenger
points(-0.6, 1, pch = 16)
text(-0.6, 1, labels = "Challenger", pos = 4)

confint(nasa, level = 0.95)

predict(nasa, newdata = data.frame(temp = -0.6), type = "response")


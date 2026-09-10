set.seed(3504)

## 1. Inverse-CDF sampler for Pareto(xm, alpha) ---------------------------
## CDF:  F(x) = 1 - (xm/x)^alpha,  x >= xm
## Solve u = F(x)  ->  x = xm / (1 - u)^(1/alpha)
rpareto <- function(n, xm, alpha) {
  u <- runif(n)
  xm / (1 - u)^(1 / alpha)
}
ppareto <- function(x, xm, alpha) ifelse(x < xm, 0, 1 - (xm / x)^alpha)

## 2. Generate "income" data 
xm    <- 30000   # incomes modelled above a $30k floor (scale parameter)
alpha <- 2       # tail index (smaller alpha = heavier tail = more inequality)

income <- rpareto(1e2, xm, alpha)

plot(ecdf(income), xlim = c(xm, 3e5), col = "grey40",
     main = "Pareto income: generated ECDF vs true CDF",
     xlab = "income ($)", ylab = "F(x)")
curve(ppareto(x, xm, alpha), add = TRUE, col = "red", lwd = 2)
legend("bottomright", c("generated (ecdf)", "true F(x)"),
       col = c("grey40", "red"), lwd = c(1, 2), bty = "n")

## 3. Why you need this for a Monte Carlo study ---------------------------
## Take alpha = 1.5: the MEAN exists (needs alpha > 1) but the VARIANCE is
## infinite (needs alpha > 2). So normal-theory inference should misbehave.

alpha_h   <- 1.5
n         <- 200
reps      <- 5000
true_mean <- alpha_h * xm / (alpha_h - 1) 

means <- replicate(reps, mean(rpareto(n, xm, alpha_h)))

## Sampling distribution of the mean (top 2% trimmed so the plot is legible)
m_plot <- means[means <= quantile(means, 0.98)]
hist(m_plot, breaks = 40, probability = TRUE,
     main = "Sampling distribution of the sample mean (alpha = 1.5)",
     xlab = "sample mean of income (top 2% trimmed)")
abline(v = true_mean, col = "red", lwd = 2)

## Actual coverage of the textbook 95% normal-theory CI:
covered <- replicate(reps, {
  x  <- rpareto(n, xm, alpha_h)
  ci <- mean(x) + c(-1, 1) * 1.96 * sd(x) / sqrt(n)
  ci[1] <= true_mean & true_mean <= ci[2]
})
mean(covered)   # nominal 0.95

## 4. The classic 80/20 "Pareto principle" -------------------------------
inc <- rpareto(1e5, xm, alpha = log(5) / log(4))
cut <- quantile(inc, 0.80)
sum(inc[inc >= cut]) / sum(inc)   # ~ 0.80: top 20% hold ~80% of income

Homework 4
================
Vy Dang
2024-10-28

``` r
xy <- read.csv("xy.csv")
```

## Problem 1 (through lecture 13)

The first part of our course focused, in part, on the theoretical
underpinnings of the standard errors and p-values reported by R after
conducting ordinary least squares regression. Through this exercise,
we’ll assess the impact of various violations of these assumptions on
confidence intervals. For ease of visualization we’ll focus on simple
regression, but the insights developed here extend without issue to
multiple regression. The data file xy.csv contains 100 values for a
predictor variable in the column x, which we will use as the fixed
values of the predictor variable in our forthcoming illustration. Store
the values in this data set as a variable called x. It also contains
responses in the column y, which you should store as a variable called
y.

1.  (2 pt) Run a regression of y on x, and show the appropriate
    diagnostic checks for linearity, homoskedasticity, and normality. Do
    these checks raise cause for concern? Explain.

``` r
model <- lm(y ~ x, data = xy)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = xy)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5499 -0.9772 -0.3922  0.3329  9.2143 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.2945     0.3627   3.569 0.000558 ***
    ## x             4.9667     0.6543   7.591 1.88e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.821 on 98 degrees of freedom
    ## Multiple R-squared:  0.3703, Adjusted R-squared:  0.3638 
    ## F-statistic: 57.62 on 1 and 98 DF,  p-value: 1.875e-11

``` r
plot(model)
```

![](Homework-4_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->![](Homework-4_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->![](Homework-4_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->![](Homework-4_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

- Linearity: The residuals vs. fitted plot shows the values spread wider
  on the 2 sides than in the middle, and more condensed in the middle.
  Linearity might not hold.

- Homoscedasticity: Similar to the residuals vs fitted plot, the
  Scale-Location plot shows a pattern across fitted values. The values
  spread wider on the 2 sides than in the middle, and more condensed in
  the middle. Homoscedasticity might not hold.

- Normality: The Normal Q-Q plot points don’t lie on a straight line
  since there are deviating points at the end on the line, indicating
  residuals might not normally distributed. Normality might not hold.

- Concern: There are clear deviations or trends in these diagnostic
  checks. This necessitate further model refinement or alternate
  approaches like transformations or robust regression methods.

2.  (2 pts) Report an 80% confidence interval for the slope on x using
    the default standard error reported by R along with quantiles from
    the t distribution. Based on your diagnostic checks, are you worried
    about the coverage of this interval? Explain.

``` r
coef_summary <- summary(model)$coefficients
slope_estimate <- coef_summary["x", "Estimate"] 
slope_se <- coef_summary["x", "Std. Error"]
df <- df.residual(model)
conf_level <- 0.8
alpha <- 1 - conf_level
t_crit <- qt(1 - alpha/2, df)
moe <- t_crit * slope_se
ci_lower <- slope_estimate - moe
ci_upper <- slope_estimate + moe
ci_lower
```

    ## [1] 4.122433

``` r
ci_upper
```

    ## [1] 5.810897

Since there were significant based on the previous diagnostic plots, the
interval calculated might not have adequate coverage.

3.  (2 pts) Report an 80% confidence interval for the slope on x using a
    heteroskedasticity-consistent standard error along with quantiles
    from the t distribution. Based on your diagnostic checks, are you
    worried about the coverage of this interval? Explain.

``` r
library(sandwich)
```

    ## Warning: package 'sandwich' was built under R version 4.2.1

``` r
library(lmtest)
```

    ## Warning: package 'lmtest' was built under R version 4.2.3

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.2.2

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
slope_se_robust <- robust_se["x"]
conf_level <- 0.80
alpha <- 1 - conf_level
t_crit <- qt(1 - alpha/2, df.residual(model))
moe_robust <- t_crit * slope_se_robust
ci_lower_robust <- slope_estimate - moe_robust
ci_upper_robust <- slope_estimate + moe_robust
ci_lower_robust
```

    ##        x 
    ## 3.969356

``` r
ci_upper_robust
```

    ##        x 
    ## 5.963975

Given substantial evidence of heteroskedasticity in the previous plot
analysis, this robust confidence intervals might better reflect the true
uncertainty around the slope than those obtained using ordinary least
squares standard errors. So this calculation in part c might have better
coverage than coverage in part b.

## Problem 2 (through lecture 14)

Using the x values from the data file, we will now generate new data
sets through a generative process in which the assumption of normality
of residuals does not hold. As a result, the extent to which method for
inference derived under the stronger linear model continue to be valid
is unclear at the onset. The simulation code generates 10,000 data sets,
each of size n = 100, with yi = 1 + 5xi + εi, where εi still have
expectation zero but instead follow a right-skewed distribution. For
this simulation, SD(εi) = σε = √0.75, and SD( ˆβ1) = 0.31. Execute the
simulation code for problem 2, found in the R script accompanying this
assignment. The vectors b1.skew, se.skew, and se.skew.hc contain the
estimated slope coefficient, standard error for the slope coefficient
derived under homoskedasticity, and the standard error using
heteroskedasticity-consistent standard errors respectively for each of
the 10,000 simulated data sets. The 100 × 10, 000 matrix Epsilon.skew
contains the random error terms εi for each data set.

1.  (1 pt) Visualize the distribution of the error terms from the first
    iteration of this simulation, stored in Epsilon.skew\[,1\], to
    confirm that they are not normally distributed. Provide a histogram
    and a normal quantile plot reflecting this.

``` r
x <- xy$x
y <- xy$y
n <- length(x)
beta0 <- 1
beta1 <- 5
sigma.error <- sqrt(.75)
SD.b1.skew <- sigma.error/(sqrt(n-1)*sd(x))

nsim <- 10000
b1.skew <- rep(0, nsim)
se.skew<- rep(0, nsim)
se.skew.hc <- rep(0, nsim)
Epsilon.skew <- matrix(0, n, nsim)

set.seed(123)
for(i in 1:nsim)
{
  errors <- exp(rnorm(n, 0, sqrt(log(1.5)))) - exp(log(1.5)/2)
  Y <- beta0 + beta1*x + errors
  lm.temp <- lm(Y~x)
  b1.skew[i] <- lm.temp$coef[2]
  se.skew[i] <- summary(lm.temp)$coef[2,2]
  se.skew.hc[i] <- sqrt(diag(vcovHC(lm.temp, type = "HC2")))[2]
  Epsilon.skew[,i] <- errors
}
epsilon_first_iter <- Epsilon.skew[, 1]
hist(epsilon_first_iter, breaks = 30, xlab = "Error Terms", col = "lightblue", border = "black")
```

![](Homework-4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
qqnorm(epsilon_first_iter)
qqline(epsilon_first_iter, col = "red")
```

![](Homework-4_files/figure-gfm/unnamed-chunk-5-2.png)<!-- --> b. (2
pts) Does the normal distribution provide a reasonable approximation to
the distribution of the sample slope in this simulation? Support your
answer through an appropriate visualization based upon output from this
simulation study.

``` r
hist(b1.skew, breaks = 50, probability = TRUE,
     xlab = "Sample Slope Estimates", col = "lightblue", border = "black")
```

![](Homework-4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
mean_b1 <- mean(b1.skew)
sd_b1 <- sd(b1.skew)

qqnorm(b1.skew)
qqline(b1.skew, col = "red", lwd = 2)
```

![](Homework-4_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

The histogram of the slope estimates b1.skew aligns well with the normal
curve. The points in the Q-Q plot lie largely on the reference line with
minor deviations. Despite the non-normality of the residuals, the
sampling distribution of β1^ approximates a normal distribution
reasonably well.

3.  (2 pts) Create a histogram for the distributions of se.skew and
    se.skew.hc. Do the means of these histograms roughly align with the
    the true value of the standard deviation for the slope, SD( ˆβ1) =
    0.31?

``` r
hist(se.skew, breaks = 50, probability = TRUE, main = "Histogram of se.skew",
     xlab = "Standard Error (SE)", col = "lightblue", border = "black")
abline(v = 0.31, col = "red", lwd = 2)  
```

![](Homework-4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
mean_se_skew <- mean(se.skew)
hist(se.skew.hc, breaks = 50, probability = TRUE, main = "Histogram of se.skew.hc",
     xlab = "Robust Standard Error (SE)", col = "lightblue", border = "black")
abline(v = 0.31, col = "red", lwd = 2)  
```

![](Homework-4_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
mean_se_skew_hc <- mean(se.skew.hc)
mean_se_skew
```

    ## [1] 0.3064537

``` r
mean_se_skew_hc
```

    ## [1] 0.3031857

The means are close to 0.31, so the standard errors are reasonably
accurate.

4.  (2 pts) Write code which finds the upper and lower bounds of 95%
    confidence intervals for the population slope based upon the t
    distribution in each of the 10,000 data sets. Create two sets of
    confidence intervals: one using se.skew as the standard error, and
    another using se.skew.hc as the standard error.

``` r
alpha <- 0.05
df <- n-2
t_crit <- qt(1 - alpha/2, df)
lower_bounds_se <- rep(0, nsim)
upper_bounds_se <- rep(0, nsim)

lower_bounds_se_hc <- rep(0, nsim)
upper_bounds_se_hc <- rep(0, nsim)

for (i in 1:nsim) {
  lower_bounds_se[i] <- b1.skew[i] - t_crit * se.skew[i]
  upper_bounds_se[i] <- b1.skew[i] + t_crit * se.skew[i]
  
  lower_bounds_se_hc[i] <- b1.skew[i] - t_crit * se.skew.hc[i]
  upper_bounds_se_hc[i] <- b1.skew[i] + t_crit * se.skew.hc[i]
}

head(cbind(lower_bounds_se, upper_bounds_se))
```

    ##      lower_bounds_se upper_bounds_se
    ## [1,]        4.002021        5.106905
    ## [2,]        5.041702        6.448051
    ## [3,]        4.029501        5.277668
    ## [4,]        4.345251        5.580325
    ## [5,]        4.300522        5.419885
    ## [6,]        4.051109        5.088967

``` r
head(cbind(lower_bounds_se_hc, upper_bounds_se_hc))
```

    ##      lower_bounds_se_hc upper_bounds_se_hc
    ## [1,]           4.011034           5.097892
    ## [2,]           5.097078           6.392674
    ## [3,]           4.038835           5.268333
    ## [4,]           4.346425           5.579151
    ## [5,]           4.294420           5.425986
    ## [6,]           4.152039           4.988037

5.  (5 pts) Use these upper and lower bounds to estimate the true
    coverage of your confidence intervals (that is, the true probability
    that intervals constructed in this fashion capture the population
    slope) using se.skew and se.skew.hc. Show your code along with your
    answer. Discuss your findings, and in particular what they suggest
    about the impact of non-normality in the residuals on inference
    conducted assuming the truth of the simple regression model when n
    is reasonably large (here, n = 100). Describe how your findings
    reflect a theorem from class.

``` r
true_slope <- 5
coverage_se <- mean(lower_bounds_se <= true_slope & upper_bounds_se >= true_slope)
coverage_se_hc <- mean(lower_bounds_se_hc <= true_slope & upper_bounds_se_hc >= true_slope)
coverage_se
```

    ## [1] 0.9528

``` r
coverage_se_hc
```

    ## [1] 0.9533

Both methods (`se.skew` and `se.skew.hc`) produce confidence intervals
that capture the true slope approximately 95% of the time, even with
right-skewed (non-normal) errors, thanks to the Central Limit Theorem
(CLT), which ensures the asymptotic normality of OLS estimators. The
slightly better coverage provided by heteroskedasticity-robust standard
errors (`se.skew.hc`) suggests marginal improvement, although
traditional standard errors (`se.skew`) still perform well in large
samples. These findings highlight the robustness of OLS methods. With a
reasonably large sample size (n=100), the estimates and associated
confidence intervals remain accurate under non-normality and
heteroskedasticity. While robust standard errors are generally more
reliable in the presence of heteroskedasticity, the traditional standard
errors provide close to nominal coverage due to the asymptotic
properties of OLS estimators as demonstrated by the simulations.

## Problem 3 (through lecture 13)

Using the x values from the data file, we will now generate new data
sets through a generative process in which the assumption of
homoskedasticity does not hold. The simulation code a generates 10,000
data sets, each of size n = 100, with yi = 1 + 5xi + εi, where εi are
now mean zero and normally distributed. For this simulation SD( ˆβ1) =
0.427, but Var(εi \| xi) varies as a function of xi. Execute the
simulation code for problem 3. The vectors b1.het, se.het, and se.het.hc
contain the estimated slope coefficient, standard error for the slope
coefficient assuming homoskedasticity, and the heteroskedasticity
consistent standard error respectively for each of the 10,000 simulated
data sets, computed using the formulae we derived in class (which assume
the truth of the simple regression model). The 100 × 10, 000 matrix
Epsilon.het contains the random error terms for each data set.

1.  (2 pts) Show a scatter plot with x on the x axis and
    Epsilon.het\[,1\] on the y axis. Describe the nature of the
    heteroskedasticity present here.

``` r
Sigma <- diag((3.6 * sqrt(.75) * abs(x - 1/2)))^2
Xmat <- cbind(rep(1, n), x)

SD.b1.het <- sqrt((solve(t(Xmat) %*% Xmat) %*% t(Xmat) %*% Sigma %*% Xmat %*% solve(t(Xmat) %*% Xmat))[2, 2])

b1.het <- rep(0, nsim)
se.het <- rep(0, nsim)
se.het.hc <- rep(0, nsim)
Epsilon.het <- matrix(0, n, nsim)

for (i in 1:nsim) {
    errors <- rnorm(n, 0, 3.6 * sqrt(.75) * abs(x - 1/2))
    Y <- beta0 + beta1 * x + errors
    lm.temp <- lm(Y ~ x)
    b1.het[i] <- lm.temp$coef[2]
    se.het[i] <- summary(lm.temp)$coef[2, 2]
    se.het.hc[i] <- sqrt(diag(vcovHC(lm.temp, type = "HC2")))[2]
    Epsilon.het[, i] <- errors
}
epsilon_het_first_iter <- Epsilon.het[,1]

plot(x, epsilon_het_first_iter,
     xlab = "x values", ylab = "Error Terms (ε)",
     col = "blue", pch = 19, cex = 0.5)
abline(h = 0, col = "red", lwd = 2)
```

![](Homework-4_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> We can
see a “funnel-shaped” pattern where errors spread out more widely as x
moves away from the center point (here around x = 0.5).

2.  (1 pt) Does the normal approximation provide a reasonable fit for
    the distribution of the sample slopes in this simulation? Support
    your answer through an appropriate visualization.

``` r
hist(b1.het, breaks = 50, probability = TRUE,
     xlab = "Sample Slope Estimates", col = "lightblue", border = "black")
```

![](Homework-4_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
qqnorm(b1.het)
qqline(b1.het, col = "red", lwd = 2)
```

![](Homework-4_files/figure-gfm/unnamed-chunk-11-2.png)<!-- --> The
histogram of b1.het suggests the normal approximation is reasonable. In
the Q-Q plot, the sample quantiles lie along the reference line, it
indicates that the distribution of b1.het is approximately normal. Given
the large sample size (n = 100) and the Central Limit Theorem, we expect
the distribution of the sample slopes to be approximately normal.

3.  (2 pts) Create a histogram for the distributions of se.het and
    se.het.hc. How do the means of these distributions compare with the
    true value of the standard deviation for the slope, SD( ˆβ1) =
    0.427? What does this reflect about the appropriateness of the
    standard errors derived under homoskedasticity and the
    heteroskedasticity-consistent standard errors in this situation?

``` r
hist(se.het, breaks = 50, probability = TRUE,
     xlab = "Standard Error (SE)", col = "lightblue", border = "black")
abline(v = 0.427, col = "red", lwd = 2)
```

![](Homework-4_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
mean_se_het <- mean(se.het)
hist(se.het.hc, breaks = 50, probability = TRUE,
     xlab = "Robust Standard Error (SE)", col = "lightblue", border = "black")
abline(v = 0.427, col = "red", lwd = 2)
```

![](Homework-4_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
mean_se_het_hc <- mean(se.het.hc)
mean_se_het
```

    ## [1] 0.3093379

``` r
mean_se_het_hc
```

    ## [1] 0.4211014

- Mean of se.het (0.3098): This mean is substantially lower than the
  true standard deviation of 0.427. It suggests that the standard errors
  calculated under the assumption of homoskedasticity are
  underestimating the true variability of the slope estimator. This
  underestimation indicates that the homoskedasticity assumption does
  not hold, making these standard errors inappropriate in the presence
  of heteroskedasticity.

- Mean of se.het.hc (0.4214): This mean is very close to the true
  standard deviation of 0.427. It suggests that the
  heteroskedasticity-consistent standard errors provide a more accurate
  estimate of the true variability of the slope estimator under
  heteroskedastic conditions. This accuracy indicates that the robust
  standard errors are more suitable when heteroskedasticity is present
  in the data.

4.  (2 pts) Create code which finds the upper and lower bounds of a 95%
    confidence interval for the population slope based upon the t
    distribution in each of the 10,000 data sets using the formula we
    derived in class based on the t-distribution. Create two sets of
    confidence intervals: one using se.het as a standard error, and one
    using se.het.hc.

``` r
alpha <- 0.05
df <- n - 2 
t_crit <- qt(1 - alpha / 2, df)
lower_bounds_se_het <- rep(0, nsim)
upper_bounds_se_het <- rep(0, nsim)

lower_bounds_se_het_hc <- rep(0, nsim)
upper_bounds_se_het_hc <- rep(0, nsim)

for (i in 1:nsim) {
  lower_bounds_se_het[i] <- b1.het[i] - t_crit * se.het[i]
  upper_bounds_se_het[i] <- b1.het[i] + t_crit * se.het[i]
  
  lower_bounds_se_het_hc[i] <- b1.het[i] - t_crit * se.het.hc[i]
  upper_bounds_se_het_hc[i] <- b1.het[i] + t_crit * se.het.hc[i]
}
head(cbind(lower_bounds_se_het, upper_bounds_se_het))
```

    ##      lower_bounds_se_het upper_bounds_se_het
    ## [1,]            4.858713            6.183820
    ## [2,]            4.472823            5.583438
    ## [3,]            4.193444            5.429362
    ## [4,]            4.184004            5.309525
    ## [5,]            4.415113            5.446253
    ## [6,]            4.249189            5.337182

``` r
head(cbind(lower_bounds_se_het_hc, upper_bounds_se_het_hc))
```

    ##      lower_bounds_se_het_hc upper_bounds_se_het_hc
    ## [1,]               4.610051               6.432482
    ## [2,]               4.296830               5.759431
    ## [3,]               4.004872               5.617934
    ## [4,]               3.961129               5.532400
    ## [5,]               4.244126               5.617241
    ## [6,]               4.172416               5.413956

5.  (5 pts) Use these upper and lower bounds to estimate the true
    coverage of your confidence intervals (that is, the true probability
    that intervals constructed in this fashion capture the population
    slope) using se.het and se.het.hc. Show your code along with your
    answer. Discuss your findings, and in particular what they suggest
    about the potential impact of heteroskedasticity on inference.

``` r
library(sandwich)
true_slope <- 5
coverage_se_het <- mean(lower_bounds_se_het <= true_slope & upper_bounds_se_het >= true_slope)
coverage_se_het_hc <- mean(lower_bounds_se_het_hc <= true_slope & upper_bounds_se_het_hc >= true_slope)
coverage_se_het
```

    ## [1] 0.8396

``` r
coverage_se_het_hc
```

    ## [1] 0.94

- 0.845: The coverage probability is significantly lower than the
  nominal level of 0.95. This suggests that se.het are not capturing the
  true variability in the slope estimates. The confidence intervals can
  be too narrow and fail to contain the true population slope (β1=5) as
  often as they should. This under coverage highlights the inadequacy of
  using homoskedastic standard errors when there is heteroskedasticity.

- 0.9438: The coverage probability is very close to the nominal level of
  0.95. This indicates that the heteroskedasticity-consistent standard
  errors provide a much better approximation of the true variability.
  The confidence intervals are more accurate and reliable in containing
  the true population slope. This shows how
  heteroskedasticity-consistent standard errors suitable for inference
  under heteroskedastic conditions.

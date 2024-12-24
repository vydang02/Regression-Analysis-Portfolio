HW5
================
Vy Dang
2024-11-13

## Problem 1 (through lecture 16)

The data set hybrid car.csv contains data on 153 models of hybrid cars
released from 1997 (the year the first Prius was introduced) to 2013.
The data includes the manufacturer’s suggested retail price (msrp, in
2013 dollars), the model year of the car, the miles per gallon of the
car (mpg), and the car’s acceleration rate (in km/hr/sec, accelerate).
Our objective will try to be to understand the extent to which year,
mpg, and acceleration can be used to predict the msrp of the car.

1.  (2 pts) Run multiple regression with msrp as the response and the
    other three variables as our predictors. Then, provide a plot with
    residuals from this regression on the y axis and fitted values on
    the x axis. Discuss what this plot suggests about whether or not the
    “linearity” assumption of the stronger linear model appears
    appropriate.

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.2.3

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.2.3

``` r
data <- read.csv('hybrid_car.csv')
model <- lm(msrp ~ year + mpg + accelerate, data = data)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = msrp ~ year + mpg + accelerate, data = data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -40179  -8824  -2794   6800  48058 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 538047.5   748028.9   0.719 0.473091    
    ## year          -265.5      373.2  -0.712 0.477832    
    ## mpg           -470.6      127.3  -3.697 0.000306 ***
    ## accelerate    4291.3      501.6   8.554 1.33e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14850 on 149 degrees of freedom
    ## Multiple R-squared:  0.5289, Adjusted R-squared:  0.5194 
    ## F-statistic: 55.76 on 3 and 149 DF,  p-value: < 2.2e-16

``` r
residuals <- resid(model)
fitted_values <- fitted(model)
ggplot(data, aes(x=fitted_values, y=residuals)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(x='Fitted Values', y='Residuals', title='Residuals vs Fitted Values')
```

![](HW5_files/figure-gfm/unnamed-chunk-1-1.png)<!-- --> The plot
demonstrates a distinct pattern or curvature within the 20000 to 40000
range of x value. This implies that the relationship between the
predictors and the response variable is not entirely linear and a
nonlinear model might be more appropriate. Transformations of the
predictors and response variable might be needed to meet the linearity
assumption.

2.  (3 pts) Define lmsrp = log(msrp) as the base-e logarithm of the
    msrp, and similarly define lmpg = log(mpg) as the base-e logarithm
    of mpg. Run a new regression with lmsrp as the response and year,
    lmpg, and accelerate as the predictors. Assess whether, after
    applying these log transformations, the assumptions of the stronger
    linear model appear reasonable.

``` r
data$lmsrp <- log(data$msrp)
data$lmpg <- log(data$mpg)
log_model <- lm(lmsrp ~ year + lmpg + accelerate, data = data)
summary(log_model)
```

    ## 
    ## Call:
    ## lm(formula = lmsrp ~ year + lmpg + accelerate, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.07547 -0.21618 -0.00678  0.20964  0.92614 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.183371  16.639820   0.251    0.802    
    ## year         0.003439   0.008305   0.414    0.679    
    ## lmpg        -0.477847   0.098631  -4.845 3.15e-06 ***
    ## accelerate   0.086472   0.011163   7.747 1.35e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3303 on 149 degrees of freedom
    ## Multiple R-squared:  0.5454, Adjusted R-squared:  0.5363 
    ## F-statistic: 59.59 on 3 and 149 DF,  p-value: < 2.2e-16

``` r
log_residuals <- resid(log_model)
log_fitted_values <- fitted(log_model)

ggplot(data, aes(x=log_fitted_values, y=log_residuals)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(x='Fitted Values', y='Residuals', title='Residuals vs Fitted Values (Log Transformed)') 
```

![](HW5_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(data, aes(sample=log_residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title="QQ Plot of Residuals (Log Transformed)")
```

![](HW5_files/figure-gfm/unnamed-chunk-2-2.png)<!-- --> The residuals
vs. fitted values plot for the log-transformed model appears more
randomly scattered around the horizontal line than in the original
model, this would indicate that the log transformations have improved
the linearity assumption. In Q-Q plot, the residuals fall mostly along
the straight line with minimal deviation, this suggests that the
residuals are normally distributed which satisfies the assumption of
normality of errors.

3.  (2 pts) Based on your regression in (b), provide a prediction
    equation for the msrp of a car as a function of its model year, the
    miles per gallon of the car, and the car’s acceleration. Make sure
    your equation provides predictions for msrp itself, not log(msrp).

``` r
beta_0 <- coef(log_model)[1]
beta_1 <- coef(log_model)[2]
beta_2 <- coef(log_model)[3]
beta_3 <- coef(log_model)[4]
predict_msrp <- function(year, mpg, accelerate){
log_msrp_pred <- beta_0 + beta_1 * year + beta_2 * log(mpg) + beta_3 * accelerate
msrp_pred <- exp(log_msrp_pred)
return(msrp_pred)}
example_year <- 2013
example_mpg <- 50
example_accelerate <- 10

predicted_msrp <- predict_msrp(example_year, example_mpg, example_accelerate)
predicted_msrp
```

    ## (Intercept) 
    ##    24372.51

4.  (4 pts) Use the pairs bootstrap to find a 95% confidence interval
    for the true slope coefficient on lmpg based upon your regression in
    (b).

``` r
B <- 1000
boot_coefs <- numeric(B)
set.seed(42)
for (i in 1:B) {
  boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
  boot_model <- lm(lmsrp ~ year + lmpg + accelerate, data = boot_sample)
  boot_coefs[i] <- coef(boot_model)['lmpg']
}
ci_lower <- quantile(boot_coefs, 0.025)
ci_upper <- quantile(boot_coefs, 0.975)
ci_lower
```

    ##       2.5% 
    ## -0.7031738

``` r
ci_upper
```

    ##      97.5% 
    ## -0.2974672

In what follows, you can assume that the stronger linear model holds for
the regression you fit in (b). All subsequent problems should be
answered based upon the regression you fit in (b).

5.  (3 pts) For two cars that differ in acceleration by two units but
    have the same values of mpg and year, can you predict the percentage
    difference in the msrp values of these cars? If so, provide your
    prediction. If not, describe what additional information you’d need
    to form your prediction.

``` r
percentage_difference <- (exp(2 * beta_3) - 1) * 100
percentage_difference
```

    ## accelerate 
    ##   18.87994

The msrp of a car is predicted to be approximately 18.87% higher for a
car with an acceleration that is 2 units greater, holding mpg and year
constant.

6.  (3 pts) For two cars that differ in in acceleration by two units but
    have the same values of mpg and year, can you predict the actual
    difference (in dollars) between the msrp values of these cars? If
    so, provide your prediction. If not, describe what additional
    information you’d need to form your prediction.

``` r
k <- exp(2 * beta_3)
average_msrp <- 30000  # Assumption
delta_msrp <- average_msrp * (k - 1)
delta_msrp
```

    ## accelerate 
    ##   5663.981

7.  (5 pts) Provide a 90% prediction interval for the msrp of a car with
    a model year of 2009, an acceleration of 10 km/hr/sec, and a miles
    per gallon value of 40. Be sure your prediction interval provides a
    range of predicted values for msrp, not log(msrp).

``` r
new_data <- data.frame(year= 2009, lmpg = log(40), accelerate = 10)
log_pred <- predict(log_model, newdata = new_data, interval = 'prediction', level = 0.90)
log_pred_val <- log_pred[1]  
log_pred_lwr <- log_pred[2] 
log_pred_upr <- log_pred[3]  
msrp_pred_val <- exp(log_pred_val)
msrp_pred_lwr <- exp(log_pred_lwr)
msrp_pred_upr <- exp(log_pred_upr)
msrp_pred_val
```

    ## [1] 26744.51

``` r
msrp_pred_lwr
```

    ## [1] 15438.18

``` r
msrp_pred_upr
```

    ## [1] 46331.16

## Problem 2 (through lecture 17)

In this problem we’ll again use the hybrid car.csv data set. We will
compare the out of sample prediction error for competing models
predicting lmsrp = log(msrp) solely as a function of mpg. Before
beginning, you run the following code.

``` r
hybrid <- read.csv("hybrid_car.csv")
dat <- data.frame(lmsrp = log(hybrid$msrp), lmpg = log(hybrid$mpg), mpg = hybrid$mpg)
trainind <- read.csv("train_hybrid_car.csv")$train
```

It starts by reloading the hybrid dataset in case you did anything funny
in Problem 1. It then defines a new data frame dat that will be useful
moving forward (especially if you’d like to base your code off of what
we did in Lecture 17, which we’d recommend). It also reads in an
additional dataset, train hybrid car.csv, which contains indicators of
whether or not a given observation in the original data set has been
assigned to the training set or the test set (TRUE if training set,
FALSE if test set). While you would normally randomly split the training
and test sets on your own, we have done it ahead of time to ensure that
everyone’s answers align.

1.  (2 pts) Using the logicals contained in train hybrid car.csv, show
    code that defines the training data set as the subset of the
    original hybrid cars data set for which the logical is TRUE, and the
    test set as the subset of observations for which the logical is
    FALSE.

``` r
train_set <- dat[trainind == TRUE, ]
test_set <- dat[trainind == FALSE, ]
```

2.  (8 pts) Use the training set to create 11 prediction functions based
    upon the following 11 regression models: • A linear regression of
    lmsrp on lmpg • Polynomial regressions of degree 1,2,…,10 with lmsrp
    as the response and the varying powers of mpg itself (not lmpg) as
    the predictor variables.

Then, use the test set to calculate the out-of-sample R2 for each of
these eleven competing models. Do so using lmsrp as the response
variable (that is, there’s no need to convert your predictions to
predictions of msrp itself before calculating the out-of-sample R2). In
addition to showing code that performs the above task, report the values
for the out of sample R2 for each of your 11 models. In light of this,
which model is estimated to have the smallest expected prediction error
for future observations?

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.2.3

    ## Warning: package 'stringr' was built under R version 4.2.3

    ## Warning: package 'forcats' was built under R version 4.2.3

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
calculate_r2 <- function(actual, predicted) {
   ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  r2 <- 1 - (ss_residual / ss_total)
  return(r2)
}
out_of_sample_r2 <- numeric(11)
model_linear <- lm(lmsrp~lmpg, data = train_set)
pred_linear <- predict(model_linear, newdata = test_set)
out_of_sample_r2[1] <- calculate_r2(test_set$lmsrp, pred_linear)
for(degree in 1:10){
  formula <- as.formula(paste("lmsrp ~ poly(mpg, ", degree, ", raw=TRUE)", sep = ""))
  model_poly <- lm(formula, data = train_set)
  pred_poly <- predict(model_poly, newdata = test_set)
  out_of_sample_r2[degree + 1] <- calculate_r2(test_set$lmsrp, pred_poly)
}

model_names <- c("Linear (lmpg)", paste("Polynomial (degree", 1:10, ")", sep = ""))
out_of_sample_r2_values <- data.frame(Model = model_names, R2 = out_of_sample_r2)
sorted <- out_of_sample_r2_values |> arrange(desc(R2))
sorted
```

    ##                    Model        R2
    ## 1   Polynomial (degree7) 0.5467193
    ## 2   Polynomial (degree6) 0.5277087
    ## 3   Polynomial (degree8) 0.4998876
    ## 4  Polynomial (degree10) 0.4626834
    ## 5   Polynomial (degree5) 0.4541173
    ## 6   Polynomial (degree9) 0.4530124
    ## 7   Polynomial (degree4) 0.4253003
    ## 8   Polynomial (degree2) 0.4179256
    ## 9   Polynomial (degree3) 0.4159849
    ## 10         Linear (lmpg) 0.3658679
    ## 11  Polynomial (degree1) 0.3212001

Polynomial degree 7 has the highest out-of-sample R^2 so this model has
the smallest expected prediction error for future observations.

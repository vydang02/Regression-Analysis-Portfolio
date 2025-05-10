Vy Dang
2024-12-02

# Wine Auction Price Prediction: Model Selection and Multiple Testing

## Executive Summary
This comprehensive analysis examines factors affecting Bordeaux wine auction prices, employing advanced statistical techniques to address multicollinearity and model selection challenges. Using data from 46 vintage years of Bordeaux wines, I developed predictive models that account for growing conditions, harvest factors, and market indicators. The study includes a critical examination of multiple testing issues in model selection, providing valuable insights for wine investors, collectors, and auction houses.

## Research Context
The premium wine market, particularly for Bordeaux vintages, represents a significant investment sector where pricing depends on complex interactions between environmental factors, aging, and market conditions. Understanding these relationships is crucial for accurate valuation and investment decisions. This analysis addresses two key challenges:
1. Developing robust predictive models for wine prices despite severe multicollinearity
2. Understanding the statistical implications of model selection procedures

## Part I: Wine Price Prediction Modeling

### Data Description
The dataset encompasses 46 vintage years of Bordeaux wines auctioned in 2015, with key variables:
- **LogAuctionIndex**: Natural logarithm of the auction price index (response variable)
- **Weather Variables**: Winter rain, harvest rain, growing temperature, harvest temperature
- **Wine Characteristics**: Age (years since vintage)
- **Market Indicators**: French population, US alcohol consumption

``` r
wine <- read.csv("wine.csv")

### Splitting the data into a training set and a test set
# Split by year, remove column for year
train <- subset(wine, Year <= 1985)[,-1]
test <- subset(wine, Year > 1985)[,-1]
```

### Initial Model Development
I began with a comprehensive multiple regression model:

``` r
full_model <- lm(LogAuctionIndex ~., data = train)
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = LogAuctionIndex ~ ., data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.58538 -0.22286 -0.01742  0.19992  0.58292 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -4.9662699  9.3823951  -0.529  0.60166    
    ## WinterRain    0.0011863  0.0005628   2.108  0.04616 *  
    ## HarvestRain  -0.0033137  0.0010650  -3.112  0.00491 ** 
    ## GrowTemp      0.6582753  0.1221937   5.387 1.79e-05 ***
    ## HarvestTemp   0.0044212  0.0599935   0.074  0.94189    
    ## Age           0.0240080  0.0507587   0.473  0.64068    
    ## FrancePop    -0.0290258  0.1369627  -0.212  0.83403    
    ## USAlcConsump  0.1092561  0.1678945   0.651  0.52166    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3307 on 23 degrees of freedom
    ## Multiple R-squared:  0.7894, Adjusted R-squared:  0.7253 
    ## F-statistic: 12.31 on 7 and 23 DF,  p-value: 1.859e-06

``` r
summary(full_model)$r.squared
```

    ## [1] 0.7893825

**Key Results:**
- In-sample R² = 0.789 (strong explanatory power)
- Significant predictors: Winter rain, harvest rain, growing temperature
- Surprisingly, Age shows non-significance (p = 0.641)

### Investigating the Age Paradox
The conventional wisdom suggests wine quality improves with age, yet our model shows Age as non-significant. This paradox warranted investigation:

``` r
reduced_model <- lm(LogAuctionIndex ~ . - Age, data = train)
anova(reduced_model, full_model)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: LogAuctionIndex ~ (WinterRain + HarvestRain + GrowTemp + HarvestTemp + 
    ##     Age + FrancePop + USAlcConsump) - Age
    ## Model 2: LogAuctionIndex ~ WinterRain + HarvestRain + GrowTemp + HarvestTemp + 
    ##     Age + FrancePop + USAlcConsump
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1     24 2.5402                           
    ## 2     23 2.5157  1   0.02447 0.2237 0.6407

P-value is 0.6407 greater than 0.05, we do not have enough evidence to
conclude that Age improves the model.

**Multicollinearity Findings:**
- Age VIF = 66.94 (extreme multicollinearity)
- Correlation analysis revealed:
  - Age vs. FrancePop: r = -0.990
  - Age vs. USAlcConsump: r = -0.925

These extremely high correlations explain the paradox: as wines age, both French population and US alcohol consumption have changed systematically over time, creating severe multicollinearity.

To help uncover what has happened, I will first report the
    variance inflation factor for the variable Age:

``` r
library(car)
```

    ## Warning: package 'car' was built under R version 4.2.2

    ## Loading required package: carData

``` r
vif_full_model <- vif(full_model)
vif_full_model['Age']
```

    ##      Age 
    ## 66.93626

``` r
cor_matrix <- cor(train)
cor_matrix['Age', ]
```

    ## LogAuctionIndex      WinterRain     HarvestRain        GrowTemp     HarvestTemp 
    ##      0.28848187     -0.12390646      0.14486296      0.01044961     -0.15797654 
    ##             Age       FrancePop    USAlcConsump 
    ##      1.00000000     -0.98999198     -0.92500432

**Key Findings:**

Age highly negatively correlates with FrancePop and USAlcConsump
predictors. The high correlations between Age and FrancePop suggest that
as Age increases, both FrancePop and USAlcConsump decrease in a nearly
linear fashion. The highly negative correlations and high VIF values
imply multicollinearity is present.

The counterintuitive finding is primarily due to the severe
multicollinearity between Age and other predictors (FrancePop and
USAlcConsump). This multicollinearity makes it difficult to determine
the individual contribution of Age, leading to a non-significant p-value
in the F-test despite the generally accepted belief regarding the
importance of Age in determining wine quality and price.

Multicollinearity inflates the standard errors of the estimated
regression coefficients. This reduces the statistical power to detect a
significant effect of Age, even if Age has a meaningful relationship
with the response variable (LogAuctionIndex). When predictors are highly
correlated, it becomes difficult to isolate the unique contribution of
each predictor. This makes the coefficient estimates unreliable. The
inflated standard errors often result in non-significant p-values for
predictors that are actually important.

### Model Selection Approaches

#### 1. Best Subset Selection
Using information criteria for optimal model selection:

``` r
library(leaps)
```

    ## Warning: package 'leaps' was built under R version 4.2.3

``` r
full_formula <- as.formula(paste('LogAuctionIndex ~', paste(names(train)[-1], collapse = " + ")))
best_subset <- regsubsets(full_formula, data = train)
summary(best_subset)
```

    ## Subset selection object
    ## Call: regsubsets.formula(full_formula, data = train)
    ## 7 Variables  (and intercept)
    ##              Forced in Forced out
    ## WinterRain       FALSE      FALSE
    ## HarvestRain      FALSE      FALSE
    ## GrowTemp         FALSE      FALSE
    ## HarvestTemp      FALSE      FALSE
    ## Age              FALSE      FALSE
    ## FrancePop        FALSE      FALSE
    ## USAlcConsump     FALSE      FALSE
    ## 1 subsets of each size up to 7
    ## Selection Algorithm: exhaustive
    ##          WinterRain HarvestRain GrowTemp HarvestTemp Age FrancePop USAlcConsump
    ## 1  ( 1 ) " "        " "         "*"      " "         " " " "       " "         
    ## 2  ( 1 ) " "        "*"         "*"      " "         " " " "       " "         
    ## 3  ( 1 ) " "        "*"         "*"      " "         " " "*"       " "         
    ## 4  ( 1 ) "*"        "*"         "*"      " "         "*" " "       " "         
    ## 5  ( 1 ) "*"        "*"         "*"      " "         "*" " "       "*"         
    ## 6  ( 1 ) "*"        "*"         "*"      " "         "*" "*"       "*"         
    ## 7  ( 1 ) "*"        "*"         "*"      "*"         "*" "*"       "*"

``` r
best_model_index <- which.min(summary(best_subset)$bic)
best_model_variables <- names(coef(best_subset, best_model_index))
best_model_variables
```

    ## [1] "(Intercept)" "WinterRain"  "HarvestRain" "GrowTemp"    "Age"

The optimal model includes: Winter rain, Harvest rain, Growing temperature, and Age.

#### 2. Forward Stepwise Selection

``` r
library(MASS)
```

    ## Warning: package 'MASS' was built under R version 4.2.1

``` r
null_model <- lm(LogAuctionIndex ~ 1, data = train)
full_model <- lm(LogAuctionIndex ~ ., data = train)
stepwise_model <- stepAIC(null_model, scope = list(upper = full_model), direction = 'forward')
```

    ## Start:  AIC=-27.57
    ## LogAuctionIndex ~ 1
    ## 
    ##                Df Sum of Sq     RSS     AIC
    ## + GrowTemp      1    5.7043  6.2402 -45.692
    ## + HarvestTemp   1    3.1500  8.7945 -35.056
    ## + HarvestRain   1    2.5144  9.4301 -32.892
    ## + FrancePop     1    1.3441 10.6004 -29.266
    ## + USAlcConsump  1    1.2597 10.6848 -29.020
    ## + Age           1    0.9940 10.9505 -28.259
    ## <none>                      11.9445 -27.565
    ## + WinterRain    1    0.4559 11.4886 -26.772
    ## 
    ## Step:  AIC=-45.69
    ## LogAuctionIndex ~ GrowTemp
    ## 
    ##                Df Sum of Sq    RSS     AIC
    ## + HarvestRain   1   1.78347 4.4568 -54.126
    ## + FrancePop     1   1.06748 5.1728 -49.508
    ## + Age           1   0.94500 5.2952 -48.783
    ## + WinterRain    1   0.91381 5.3264 -48.601
    ## + USAlcConsump  1   0.91378 5.3265 -48.600
    ## + HarvestTemp   1   0.39921 5.8410 -45.741
    ## <none>                      6.2402 -45.692
    ## 
    ## Step:  AIC=-54.13
    ## LogAuctionIndex ~ GrowTemp + HarvestRain
    ## 
    ##                Df Sum of Sq    RSS     AIC
    ## + FrancePop     1   1.43595 3.0208 -64.182
    ## + Age           1   1.39487 3.0619 -63.763
    ## + USAlcConsump  1   1.20771 3.2491 -61.924
    ## + WinterRain    1   0.36109 4.0957 -54.746
    ## <none>                      4.4568 -54.126
    ## + HarvestTemp   1   0.01304 4.4437 -52.217
    ## 
    ## Step:  AIC=-64.18
    ## LogAuctionIndex ~ GrowTemp + HarvestRain + FrancePop
    ## 
    ##                Df Sum of Sq    RSS     AIC
    ## + WinterRain    1   0.42429 2.5965 -66.874
    ## <none>                      3.0208 -64.182
    ## + USAlcConsump  1   0.00792 3.0129 -62.264
    ## + Age           1   0.00256 3.0183 -62.208
    ## + HarvestTemp   1   0.00234 3.0185 -62.206
    ## 
    ## Step:  AIC=-66.87
    ## LogAuctionIndex ~ GrowTemp + HarvestRain + FrancePop + WinterRain
    ## 
    ##                Df Sum of Sq    RSS     AIC
    ## <none>                      2.5965 -66.874
    ## + USAlcConsump  1  0.054963 2.5416 -65.537
    ## + Age           1  0.032873 2.5637 -65.269
    ## + HarvestTemp   1  0.000983 2.5955 -64.886

``` r
names(coef(stepwise_model))
```

    ## [1] "(Intercept)" "GrowTemp"    "HarvestRain" "FrancePop"   "WinterRain"

All other variables are the same, but model in e has ‘Age’ while here we
have FrancePop.

The stepwise model selected: Growing temperature, Harvest rain, France population, and Winter rain.

Notably, stepwise selection chose FrancePop instead of Age, highlighting how correlated predictors can lead to different model selections.

#### 3. Ridge Regression
To address multicollinearity explicitly:

``` r
y.train <- as.vector(train$LogAuctionIndex)
y.test <- as.vector(test$LogAuctionIndex)

x.train <- as.matrix(train[,-1])
x.test <- as.matrix(test[,-1])
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 4.2.3

    ## Loading required package: Matrix

    ## Warning: package 'Matrix' was built under R version 4.2.2

    ## Loaded glmnet 4.1-8

``` r
set.seed(321)
ridge.train <- cv.glmnet(x.train, y.train, alpha = 0)
lambda_optimal <- ridge.train$lambda.min
ridge_model <- glmnet(x.train, y.train, alpha = 0, lambda = lambda_optimal)
ridge_coefficients <- coef(ridge_model)
ridge_coefficients
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s0
    ## (Intercept)  -2.508987159
    ## WinterRain    0.000983245
    ## HarvestRain  -0.002989491
    ## GrowTemp      0.583711146
    ## HarvestTemp   0.021161603
    ## Age           0.012612611
    ## FrancePop    -0.031362631
    ## USAlcConsump  0.025063189

``` r
y_train_pred <- predict(ridge_model, x.train)
SST <- sum((y.train - mean(y.train))^2)
SSE <- sum((y.train - y_train_pred)^2)
R2_ridge <- 1 - SSE/SST
R2_ridge
```

    ## [1] 0.7816978

``` r
summary(full_model)$r.squared
```

    ## [1] 0.7893825

The full model in part a uses ordinary least squares (OLS) without
regularization, optimizing for the best fit within the training data,
often leading to a higher in-sample R2 due to overfitting to the
training data. Ridge regression applies a penalty to reduce the
magnitude of coefficients, aiming to prevent overfitting and improve
generalization. This regularization can result in a slightly lower
in-sample R2, but generally leads to better performance on new unseen
data by avoiding overfitting.

Ridge regression keeps all predictors but shrinks coefficients, particularly for correlated variables:
- All coefficients remain non-zero
- Coefficients for correlated predictors are substantially reduced
- In-sample R² = 0.782 (slightly lower than OLS)

The out-of-sample R2 for the models:

``` r
y_test_pred_a <- predict(full_model, newdata = test)
SST_test <- sum((y.test - mean(y.test))^2)
SSE_test_a <- sum((y.test - y_test_pred_a)^2)
R2_test_a <- 1 - SSE_test_a/SST_test
R2_test_a
```

    ## [1] 0.511275

``` r
best_model_variables_e <- c("WinterRain", "HarvestRain", "GrowTemp", "Age")
best_model_e <- lm(LogAuctionIndex~ ., data = train[, c("LogAuctionIndex", best_model_variables_e)])
y_test_pred_e <- predict(best_model_e, newdata = test[, best_model_variables_e])
SSE_test_e <- sum((y.test - y_test_pred_e)^2)
R2_test_e <- 1 - SSE_test_e/SST_test
R2_test_e
```

    ## [1] 0.7322731

``` r
stepwise_variables_f <- c("GrowTemp", "HarvestRain", "FrancePop", "WinterRain")
stepwise_model_f <- lm(LogAuctionIndex ~ ., data = train[, c("LogAuctionIndex", stepwise_variables_f)])
y_test_pred_f <- predict(stepwise_model_f, newdata = test[, stepwise_variables_f])
SSE_test_f <- sum((y.test - y_test_pred_f)^2)
R2_test_f <- 1 - SSE_test_f/SST_test
R2_test_f
```

    ## [1] 0.7033702

``` r
y_test_pred_g <- predict(ridge_model, newx = x.test)
SSE_test_g <- sum((y.test - y_test_pred_g)^2)
R2_test_g <- 1 - SSE_test_g/SST_test
R2_test_g
```

    ## [1] 0.6958973

The best subset selection model (e) provides the highest out-of-sample
R2 of 0.732.

The initial investigation revealed severe multicollinearity,
particularly between the variable Age and other predictors such as
FrancePop and USAlcConsump. The Variance Inflation Factor (VIF) for Age
was extremely high (66.93626), indicating severe multicollinearity. The
primary motivation for using ridge regression is to improve the
generalization performance of the model by addressing multicollinearity.
Our findings confirm this, as the ridge regression model offered better
out-of-sample R2 than the OLS model, implying that it is better at
predicting new data due to reduced overfitting. Ridge regression ensures
that all predictors remain in the model with nonzero coefficients albeit
shrunk.By applying a penalty to the regression coefficients, ridge
regression stabilizes their estimates, making the model more robust.

### Model Performance Comparison

| Model | In-Sample R² | Out-of-Sample R² |
|-------|--------------|------------------|
| Full OLS | 0.789 | 0.511 |
| Best Subset | 0.786 | 0.732 |
| Stepwise | 0.783 | 0.704 |
| Ridge | 0.782 | 0.696 |

The best subset selection model achieved the highest out-of-sample R² (0.732), demonstrating superior predictive performance.

### Key Insights from Wine Price Modeling

1. **Multicollinearity Impact**: The severe correlation between Age and demographic variables obscures Age's true effect
2. **Weather Dominance**: Growing conditions (temperature and rainfall) are the most reliable predictors
3. **Model Selection Matters**: Different selection methods yield different models, with best subset selection providing optimal out-of-sample performance
4. **Ridge Benefits**: While Ridge had slightly lower performance, it provides stability when all predictors must be retained

## Part II: Multiple Testing in Model Selection

### The Multiple Testing Problem
When selecting the "best" model based on p-values, we implicitly conduct multiple comparisons, inflating the Type I error rate. This simulation study quantifies this inflation and explores corrective measures.

### Simulation Design
Suppose I have 10 covariates, x1, …, x10, and among these 10 covariates
I want to find the “best” simple regression based on whichever covariate
has the smallest p-value in a simple regression. I then decide to report
the result of the hypothesis test for my chosen model. In reality there
will be no relationship between the response variable and either of the
predictor variables, such that the null hypothesis that βj = 0 is true
for j = 1, …, 10. I will conduct the tests at α = 0.05, and I will
generate the observations from the model for i = 1, …, 100 : yi = 0 +
εi, εi iid ∼ N (0, 1)

Before proceeding, suppose instead I had committed myself to
    only reporting the result of the hypothesis test corresponding to
    the slope coefficient in a simple regression of y on x1, rather than
    the result of the “best” hypothesis test. If I conduct my test at α
    = 0.05, the type I error rate would be alpha=0.05, or 5%. If only testing beta1
and assuming all assumptions of the linear regression model are met
(including normally distributed errors and no relationship between x1
and y in reality), the test will correctly follow the specified
significance level. If the null hypothesis H0: beta 1 = 0 is true,
there’s a 5% chance of incorrectly rejecting it with this test. This is
because we have a predetermined significance level. By definition, the
probability of incorrectly rejecting the true null hypothesis (making a
Type I error) is equal to the significance level alpha.

I will design the simulation as follows:

- 10 independent predictors with no true relationship to the response
- Sample size: n = 100
- 10,000 simulation iterations
- Nominal significance level: α = 0.05

### Method 1: Naive Selection
Select the predictor with the smallest p-value:

``` r
nsim <- 10000
reject <- rep(0, nsim)
for(i in 1:nsim)
{
  X <- mvrnorm(100, mu = rep(0, 10), Sigma = diag(10))
  y <- rnorm(100)
  pvals <- rep(0, 10)
  for(j in 1:10)
  {
    pvals[j] <- summary(lm(y~X[,j]))$coef[2,4]
  }
  #######
  # Here: fill in code to say that reject[i] evaluates to 1 if the smallest p-value is less than 0.05 
  # (this corresponds to the p-value from the best model we would have chosen)
  #######
  reject[i] <- (min(pvals) <= 0.05)
}
  
mean(reject)
```

    ## [1] 0.3987

**Result**: Type I error rate = 39.9% (vs. nominal 5%)

### Method 2: Bonferroni Correction
Adjust significance level for multiple testing:

``` r
nsim <- 10000
reject <- rep(0, nsim)
alpha <- 0.05
m <- 10 
alpha_bonferroni <- alpha / m

for (i in 1:nsim) {
  X <- mvrnorm(100, mu = rep(0, 10), Sigma = diag(10))
  y <- rnorm(100)
  pvals <- rep(0, 10)
  for (j in 1:10) {
    pvals[j] <- summary(lm(y ~ X[, j]))$coef[2, 4]
  }
  reject[i] <- (min(pvals) <= alpha_bonferroni)
}
mean(reject)
```

    ## [1] 0.0464

Changes made: - We calculate the Bonferroni-adjusted significance level:
alpha bonferroni=0.05/10=0.005 - We adjusted the rejection condition,
check if the smallest p-value from the 10 tests is less than or equal to
the adjusted significance level alpha bonferroni - We calculate the
proportion of times the null hypothesis is incorrectly rejected using
the adjusted significance level to control the familywise error rate.

**Result**: Type I error rate = 4.6% (properly controlled)

### Method 3: Train-Test Split

Use training data for selection, test data for inference. I will build my
model on the training set without worrying about multiple comparisons
(hence treating the training set as an exploratory data set for
discovering patterns), but then conduct my hypothesis tests on the
test set (using it as a confirmatory data set for testing the hypotheses). 

``` r
library(caTools) # install package if you haven't already
```

    ## Warning: package 'caTools' was built under R version 4.2.3

``` r
nsim <- 10000
reject.new =rep(0, nsim)
for(i in 1:nsim)
{
  X <- mvrnorm(100, mu = rep(0, 10), Sigma = diag(10))
  y <- rnorm(100)
  trainind <- sample.split(y, SplitRatio=0.7)
  X.train <- X[trainind, ]
  y.train <- y[trainind]
  X.test <- X[!trainind, ]
  y.test <- y[!trainind]
  pvals.train <- rep(0, 10)
  for(j in 1:10)
  {
    pvals.train[j] <- summary(lm(y.train~X.train[ ,j]))$coef[2,4]
  }
  
  # use the which.min command to find which p-value was smallest in the TRAINING set. 
  # This is the covariate I have chosen
  smallest <- which.min(pvals.train)
  
  # run a regression on the TEST set using the chosen covariate
  
  pval.test <- summary(lm(y.test~X.test[, smallest]))$coef[2,4]
  reject.new[i] <- (pval.test <= 0.05)
}
mean(reject.new)
```

    ## [1] 0.0491

**Result**: Type I error rate = 4.9% (properly controlled)

## Business Applications

### For Wine Investors
- Focus on weather data when evaluating future vintages
- Consider that apparent age effects may be confounded with market evolution
- Use best subset models for price predictions

### For Auction Houses
- Emphasize growing conditions in marketing materials
- Provide weather context for vintage years
- Use robust prediction intervals for reserve price setting

### For Wine Collectors
- Prioritize vintages with optimal growing conditions
- Understand that age alone doesn't guarantee value appreciation
- Consider market dynamics when timing sales

## Statistical Best Practices

### Model Selection Guidelines
1. Always evaluate out-of-sample performance
2. Consider multicollinearity when interpreting coefficients
3. Use appropriate corrections for multiple testing
4. Validate findings on held-out data

### Addressing Multicollinearity
1. Ridge regression for prediction when all variables needed
2. Best subset selection for interpretable models
3. Domain knowledge to choose between correlated predictors

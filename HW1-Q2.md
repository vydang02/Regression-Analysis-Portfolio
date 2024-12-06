STATS 413 HW1 Q2, Q3e
================
Vy Dang
2024-09-03

``` r
knitr::opts_chunk$set(fig.path='Figs/')
```

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.2.3

### Problem 2

For historical reasons the US has a system of taxing homeowners to fund
a large fraction of local in- frastructure such as local primary, middle
and high schools, town and county administrations, and town and county
roads. The tax, called “property tax”, is based on an assessment
(estimation, determination) of the value of each residence (home) and
the lot (land) that belongs to it. Because the assessments become
outdated after a few years, towns have to hire assessors and update the
assessments every so often. There are of course several factors that
play a role in assessing the value of a property, such as square feet of
livable area, size of the lot (land), quality and condition of the
building, and desirability of the area. We will only consider square
feet of livable area. The following exercise can be used to check for
any property whether its assessed value is in line or out of line with
other properties of similar size in terms of livable area. This data set
is called Residential Property Assessments.csv.

``` r
data <- read.csv('Residential_Property_Assessments.csv')
```

1.  (2 pts) Show a scatterplot of Assessment against Livable Area (i.e.,
    Assessment is the y variable, and Livable Area is the x variable).
    Add a main title along with axis labels.

``` r
data |> ggplot(aes(x = Livable.Area, y = Assessment)) + geom_point() +
  labs(title = "Assessment vs Livable Area",
       x = "Livable Area (sq ft)",
       y = "Assessment ($)")
```

![](Figs/unnamed-chunk-3-1.png)<!-- --> unnamed-chunk-3-1.png

2.  (1 pt) Based on the scatterplot, is the association approximately
    linear?

> The association is approximately linear based on the scatterplot.

3.  (4 pts) Use R to find the equation for the regression line with
    Assessment as the response variable and Living Area as the predictor
    variable. Report the resulting equation (rather than simply showing
    R output).

``` r
model <- lm(Assessment~Livable.Area, data = data)
model
```

    ## 
    ## Call:
    ## lm(formula = Assessment ~ Livable.Area, data = data)
    ## 
    ## Coefficients:
    ##  (Intercept)  Livable.Area  
    ##     168518.6         180.8

``` r
coefficients <- coef(model)
intercept <- coefficients[1]
slope <- coefficients[2]
paste("Assessment =", round(intercept,2), "+", round(slope,2), "* Livable Area" )
```

    ## [1] "Assessment = 168518.56 + 180.82 * Livable Area"

4.  (3 pts) Interpret the slope. Initially, do so first formally
    according to the formulation from class. Then, give an informal
    interpretation in terms of the average value of a square foot of
    livable area.

> Formal interpretation: Equation: Assessment = β0 + β1 \* Livable Area

> Holding all other variables equal (ceteris paribus), if two properties
> differ in their livable area by 1 square foot, then we expect their
> assessments to differ by β1 units on average. β1 represents the change
> in the expected assessment for a one-unit increase in the livable area
> given that other factors are held constant.

> Informal interpretation: For every additional square foot of livable
> area, the property assessment increases by 180.8 dollars on average.
> This means that a larger house with more livable area will have a
> higher assessed value. Each additional square foot is valued at
> approximately 180.8 dollars in terms of property assessment.

5.  (3 pts) Interpret the intercept. Initially, do so formally according
    to the formulation in class. Is there an informal interpretation of
    the intercept in the context of this problem that might be
    meaningful, despite the fact that it represents an extrapolation?

> Formal interpretation: The intercept β0 in a linear regression model
> is interpreted as the expected value of the response variable
> (Assessment) when all predictor variables (Livable Area) are zero.
> Mathematically it represents the value of (E(Y\|X=0)). The intercept
> β0 would be the assessment value predicted for a property with zero
> square feet of livable area. So formally, (β0 = 168518.6) means that
> the expected assessment value for a property with zero square feet of
> livable area is 168518.6 dollars

> Informal interpretation: Despite the fact that a property with zero
> square feet of livable area is unrealistic, this value can
> representthe starting point or base value of the property due to land
> value or fixed costs that are independent of the livable area.

6.  (2 pts) What fraction of variation in Assessment is accounted for by
    Livable Area through our regression model? Report the relevant
    quantity from the R output. Finally, what fraction of variation is
    accounted for by other factors besides livable area, such as
    differences in lot size, condition and quality of the building, and
    viability of the area?

``` r
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = Assessment ~ Livable.Area, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -330914  -35605    -958   38267  516292 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.685e+05  5.894e+03   28.59   <2e-16 ***
    ## Livable.Area 1.808e+02  2.319e+00   77.99   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 64340 on 1624 degrees of freedom
    ## Multiple R-squared:  0.7893, Adjusted R-squared:  0.7891 
    ## F-statistic:  6082 on 1 and 1624 DF,  p-value: < 2.2e-16

> Fraction of variation in Assessment accounted for by Livable Area
> ((R^2)): 0.7893.

> 78.93% of the variation in the Assessment (property value) is
> explained by the Livable Area in this regression model.

> Fraction of variation in Assessment accounted for by other factors:
> 1 - R^2 = 1 - 0.7893 = 0.2107.

> 21.07% of the variation in Assessment is due to other factors besides
> Livable Area.

7.  (2 pts) Point prediction: Based on the fitted equation, what can you
    say about the predicted price for a residence with 2,500 square feet
    of livable area? Use R to calculate this and show your code.

``` r
new_data <- data.frame(Livable.Area=2500)
predict_2500_sqft <- predict(model, newdata = new_data)
predict_2500_sqft
```

    ##        1 
    ## 620569.1

8.  (3 pts) Predict the difference in assessed value for two residences
    where one residence is 200 square feet larger than the other. If you
    cannot complete your calculation with the information provided,
    explain why not.

> Here we use beta-1 to calculate. The slope can directly be scaled to
> predict the difference for any unit change in the independent
> variable.

``` r
difference_in_assessment <- slope * 200
difference_in_assessment
```

    ## Livable.Area 
    ##     36164.05

> The difference in assessed value for two residences where one is 200
> square feet larger than the other would be \$36,164.05.

### Problem 3

5.  (4 pts) Using the regression of Assessment on Livable Area you
    conducted in Problem 2, illustrate your findings in Problem 3,
    parts b) and d). That is, produce code that shows that for the
    regression of Assessment on Livable Area: • The residuals from this
    regression have mean zero. • The predicted values and residuals from
    this regression are uncorrelated. \[Note: you may see very small but
    nonzero numbers come up in your numerical verifications, for
    instance 2 × 10−16 or something of a similar magnitude. Consider
    these as true zeroes - the departures from zero are artifacts of
    machine precision.\]

``` r
residuals <- residuals(model)
fitted_values <- fitted(model)
mean_residuals <- mean(residuals)
mean_residuals
```

    ## [1] 4.853204e-12

``` r
correlation_fitted_residuals <- cor(fitted_values, residuals)
correlation_fitted_residuals
```

    ## [1] 3.398222e-17

``` r
ggplot(data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype="dashed", color="red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")
```

![](Figs/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  geom_vline(aes(xintercept = mean_residuals), color = "red", linetype = "dashed") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency")
```

![](Figs/unnamed-chunk-11-1.png)<!-- -->

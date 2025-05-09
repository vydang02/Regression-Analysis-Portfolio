# Applied Regression Analysis Portfolio

## Project Overview

This portfolio demonstrates my proficiency in applied regression analysis using R, showcasing a comprehensive understanding of statistical modeling techniques through a series of analytical assignments. Throughout this project, I apply various regression methods to real-world datasets to solve complex analytical problems, interpret results, and draw meaningful conclusions.

## Technical Skills Demonstrated

- Multiple linear regression and model selection
- Regression diagnostics and assumption verification
- Interaction effects and coefficient interpretation
- Heteroskedasticity detection and robust standard errors
- Cross-validation and predictive modeling
- Polynomial regression and non-linear relationships
- Subset selection methods (forward, backward, best subset)
- Ridge regression and regularization

## Key Projects

### Housing Price Analysis

Using the Ames, Iowa Housing Prices dataset with 2,838 properties, I developed predictive models for housing prices based on property characteristics.

**Key findings:**
- Developed a log-transformed model with interactions that improved R² from 0.75 to 0.78
- Identified that building type significantly affects how living area influences price
- Validated model assumptions using diagnostic plots and residual analysis
- Created prediction intervals with appropriate confidence levels

```r
# Log-transformed model with interaction effects
log_model <- lm(lmsrp ~ year + lmpg + accelerate, data = data)
summary(log_model)

# Examining interaction effects between building type and living area
model_interaction <- lm(SalePrice ~ . + LivArea:BldgType, data = ames)
```

### Wine Auction Price Modeling

Analyzed factors affecting Bordeaux wine auction prices using regression models to understand how growing conditions, harvest factors, and aging affect prices.

**Key findings:**
- Identified multicollinearity issues between Age, FrancePop, and USAlcConsump variables
- Implemented best subset selection to determine optimal predictor combination
- Applied ridge regression to handle multicollinearity while maintaining prediction performance
- Achieved out-of-sample R² of 0.73 with best subset selection model

```r
# Best subset selection
best_subset <- regsubsets(full_formula, data = train)
summary(best_subset)

# Ridge regression with cross-validation
set.seed(321)
ridge.train <- cv.glmnet(x.train, y.train, alpha = 0)
ridge_model <- glmnet(x.train, y.train, alpha = 0, lambda = ridge.train$lambda.min)
```

### Education-Income Relationship Analysis

Examined the relationship between education, income, and generational effects using multiple regression models with interaction terms.

**Key findings:**
- Discovered significant differences in education-income relationships between Millennials and non-Millennials
- Addressed heteroskedasticity using robust standard errors
- Implemented hypothesis testing to evaluate slope coefficient differences
- Constructed appropriate confidence intervals for group differences

```r
# Analysis with interaction terms
edu_model <- lm(Income ~ Education*Millenial, data = edu)
summary(edu_model)

# Robust standard errors to address heteroskedasticity
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
```

### Hybrid Car Price Prediction

Modeled relationships between hybrid car features and pricing using various transformations and polynomial regression techniques.

**Key findings:**
- Applied log transformations to improve linearity and homoscedasticity
- Implemented polynomial regression of varying degrees
- Conducted out-of-sample validation to select optimal model complexity
- Determined 7th-degree polynomial provided the best predictive performance

```r
# Log-transformed model
data$lmsrp <- log(data$msrp)
data$lmpg <- log(data$mpg)
log_model <- lm(lmsrp ~ year + lmpg + accelerate, data = data)

# Evaluating polynomial models of different degrees
for(degree in 1:10){
  formula <- as.formula(paste("lmsrp ~ poly(mpg, ", degree, ", raw=TRUE)", sep = ""))
  model_poly <- lm(formula, data = train_set)
  # Model evaluation code...
}
```

## Conclusion

This portfolio demonstrates my ability to apply advanced regression techniques to real-world problems, properly diagnose model issues, and develop robust predictive models. I've shown proficiency in handling complex statistical challenges including multicollinearity, heteroskedasticity, and non-linear relationships while maintaining interpretability and predictive power.

These skills are directly applicable to business analytics, economic forecasting, market research, and data-driven decision making across industries. My methodical approach to statistical modeling ensures reliable insights that can drive strategic business decisions.

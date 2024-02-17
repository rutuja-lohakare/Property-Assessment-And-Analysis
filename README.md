# Statistical-Modeling-for-Property-Valuation

## Explanation:
This repository contains the R code for a project focused on estimating property prices in Cook County using statistical modeling. The primary goal is to create a regression model using existing housing data to predict prices for new housing data within the same county.

## Features:
### Data Cleaning:
Handling a large dataset with 60 independent variables.
Selection of around 20 highly explainable variables.
Removal of columns with outliers and null values.
Conversion of categorical columns into factored levels for numerical relevance.

### Variable Selection:
Utilization of Lasso Regression for variable selection, resulting in an optimal set of approximately 50 variables.
Distribution analysis and exclusion of skewed columns to streamline regression analysis.

### Modeling:
Designing a linear regression model with selected variables, achieving an R-Squared value around 80%.
Validation of model explainability with multiple models, including Linear, Lasso, and Random Forest.
Selection of Random Forest as the preferred model due to lower Mean Squared Error (MSE) and reasonable computational efficiency.

### Prediction:
Application of the Random Forest model to predict property prices for a new dataset.
Data cleaning process for the new dataset similar to the original, with imputation for data points lacking sufficient information.

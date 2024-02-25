# Statistical-Modeling-for-Property-Valuation

This repository contains the R code for a project focused on estimating property prices in Cook County using statistical modeling. The primary goal is to create a regression model using existing housing data to predict prices for new housing data within Cook County.

## Data Files
- `codebook.csv`: Defines dataset columns.
- `historic_property_data.csv`: Contains data on 50,000 recent property sales.

## Methodology

### Data Preprocessing
Utilizing the dataset 'historic_property_data', the initial steps included:
- Removal of non-predictive attributes.
- Elimination of duplicate entries.
- Handling of missing data.
- Imputation of missing neighborhood and town codes using the Last Observation Carried Forward method.
- Imputation of numeric missing values by column means.
- Adjustment of sale prices to address negatives and outliers.
- Removal of rows with null values for data integrity.

### Variable Selection
The approach focused on:
- Analysis of location dimensions and flood risk.
- Use of numerical scales for flood risk and direction to avoid conversion to factor variables.
- Performance of a correlation matrix and Variance Inflation Factor (VIF) analyses to assess multicollinearity.
- Removal of highly correlated variables like township codes for model accuracy.

### Modeling
Various modeling techniques were employed:
- Linear regression.
- Random forest.
- Lasso modeling.
Each model was designed using cross-validation to enhance accuracy.

### Model Evaluation & Prediction
- The models' performance was assessed using Mean Squared Error (MSE).
- The Random Forest model, due to lower MSE, was chosen for predicting property prices on a new dataset.
- The data cleaning process for the new dataset was similar to the original, with imputations for data points lacking sufficient information.

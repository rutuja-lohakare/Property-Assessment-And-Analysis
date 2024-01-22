#Group 5 - FGP 
#Total Runtime: 6 minutes
#Loading all required libraries

library(readr)        # For reading CSV files
library(tidyr)        # For tidying data
library(dplyr)        # For data manipulation
library(zoo)          # For ordered indexing and imputation (na.locf())
library(DescTools)    # For Winsorization of data
library(ggplot2)      # For data visualization
library(caret)        # For model training and feature selection
library(gplots)       # For plotting extended functionality for ggplot2
library(car)          # For additional diagnostic tools for regression models
library(randomForest) # For random forest modeling
library(glmnet)       # For lasso and elastic-net regularized models
library(gbm)          # For gradient boosting machines
library(corrplot)
library(doParallel)

#STAGE 1: Preprocessing
# Load the datasets
predict_property_data <- read_csv("predict_property_data.csv")
historic_property_data <- read_csv("historic_property_data.csv")
codebook <- read_csv("codebook.csv")

# Reviewing the structure of the historical property dataset to understand the data types and composition of the dataframe.

str(historic_property_data)



# Removing columns from the historic dataset that are not marked as predictors according to the codebook. This helps in focusing on relevant variables for modeling.

selected_vars <- subset(codebook, var_is_predictor == FALSE, select = var_name_standard)
columns_to_remove <- intersect(names(historic_property_data), selected_vars$var_name_standard)
historic_property_data <- historic_property_data[, !names(historic_property_data) %in% columns_to_remove]

# Calculating and ordering the number of missing values by column to prioritize which columns may need addressing or may be dropped due to extensive missing data.

missing_values <- colSums(is.na(historic_property_data))
missing_df <- data.frame(column_name = names(missing_values), missing_values = missing_values)
missing_df <- missing_df[order(-missing_df$missing_values),]
print(missing_df)

# Calculating the number of unique values per column to identify potential categorical variables and to understand data cardinality.

count_unique <- function(column) {
  length(unique(column))
}

# Apply the function to each column and arrange in descending order
unique_counts <- sapply(historic_property_data, count_unique)
unique_counts <- sort(unique_counts, decreasing = TRUE)

# Display the result
print(unique_counts)


# Removing any duplicate rows from the dataset to ensure data quality and integrity.

historic_property_data <- historic_property_data[!duplicated(historic_property_data),]

# Imputing missing values based on groups defined by 'meta_nbhd' and 'meta_town_code' using Last Observation Carried Forward, which is a method to impute time-series data.

historic_property_data <- historic_property_data %>% 
  group_by(meta_nbhd, meta_town_code) %>% 
  mutate_all(na.locf, na.rm = FALSE, .groups = "drop")

# Imputing missing numeric values by replacing them with the mean of their respective column. This is a common technique for handling missing data before modeling.

numeric_cols <- names(historic_property_data)[sapply(historic_property_data, is.numeric)]
historic_property_data[numeric_cols] <- lapply(historic_property_data[numeric_cols], 
                                               function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Apply Winsorization to numeric columns to reduce the effect of extreme outliers

historic_property_data[numeric_cols] <- lapply(historic_property_data[numeric_cols], 
                                               function(x) Winsorize(x, probs = c(0.01, 0.99)))

# Filtering out records where 'ind_arms_length' is FALSE as these transactions may not represent market value and could bias the model.

historic_property_data <- historic_property_data %>%
  filter(ind_arms_length == TRUE)

# Correcting any negative values in 'sale_price' by setting them to zero, assuming that sale prices cannot be negative.

historic_property_data$sale_price <- pmax(historic_property_data$sale_price, 0, na.rm = TRUE)

# Displaying the ordered sale prices to inspect their distribution, which can provide insights into the range and spread of property values.

ordered_sale_price <- historic_property_data$sale_price[order(historic_property_data$sale_price)]
print(ordered_sale_price)

# Counting the occurrences of zero values in 'sale_price' after correction, which can indicate data issues or entry errors

num_zero_sale_price <- sum(historic_property_data$sale_price == 0, na.rm = TRUE)
print(num_zero_sale_price)

# Dropping specific columns having a high number of missing values, as they may not be useful for analysis or modeling due to insufficient data.

columnsToDrop <- c("char_apts", "char_porch", "char_attic_fnsh", "char_tp_dsgn")
historic_property_data <- historic_property_data[, !names(historic_property_data) %in% columnsToDrop]

# Eliminating rows with missing values in specific school district columns to improve data quality, as these values are crucial and cannot be accurately imputed.

historic_property_data <- historic_property_data %>%
  filter(!is.na(geo_school_elem_district) & !is.na(geo_school_hs_district))

# After all data transformations, recalculate the number of missing values to verify that all intended imputations and removals have been successfully applied.

missing_values <- colSums(is.na(historic_property_data))
missing_df <- data.frame(column_name = names(missing_values), missing_values = missing_values)
missing_df <- missing_df[order(-missing_df$missing_values),]
print(missing_df)

# Visualizing the distribution of 'sale_price' using a histogram to identify the shape, spread, and any potential skewness in property sale prices.

ggplot(historic_property_data, aes(x = sale_price)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Sale Price", x = "Sale Price (in USD)", y = "Frequency")

summary(historic_property_data["sale_price"])


#STAGE 2: Selecting Predictors


# Select the specified predictors and sale_price
selected_predictors <- c('meta_town_code', 'meta_nbhd', 'geo_school_elem_district', 
                         'geo_school_hs_district', 'char_hd_sf', 'char_bldg_sf', 
                         'char_beds', 'char_fbath', 'char_hbath', 'sale_price','geo_fs_flood_factor','geo_fs_flood_risk_direction')

data_selected <- select(historic_property_data, all_of(selected_predictors))

# Calculate the correlation matrix
numeric_data_selected <- select(data_selected, where(is.numeric))

cor_matrix <- cor(numeric_data_selected, use = "complete.obs")

corrplot(cor_matrix, method = "circle")


# Check for multicollinearity
# Fit a linear model
model <- lm(sale_price ~ ., data = numeric_data_selected)

# Calculate Variance Inflation Factor (VIF) for each predictor
vif_results <- vif(model)
print(vif_results)

# Remove one of the highly collinear variables - 'meta_town_code'
data_selected_reduced <- numeric_data_selected
data_selected_reduced$meta_town_code <- NULL

# Check to ensure the column is removed
names(data_selected_reduced)

# Fit a new model with the reduced set of predictors
model_reduced <- lm(sale_price ~ ., data = data_selected_reduced)

# Calculate VIF again
vif_results_reduced <- vif(model_reduced)
print(vif_results_reduced)




#STAGE 3: Fitting Predictive Algorithms

set.seed(123) # Set a seed for reproducibility

# Split the data into training and testing sets
index <- createDataPartition(data_selected_reduced$sale_price, p = 0.8, list = FALSE)
train_data <- data_selected_reduced[index, ]
test_data <- data_selected_reduced[-index, ]


# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)


# Fit a linear regression model
lm_model <- lm(sale_price ~ ., data = train_data)
lm_model <- train(
  sale_price ~ ., 
  data = train_data,
  method = "lm",
  trControl = train_control)

# Make predictions on the test set
lm_predictions <- predict(lm_model, test_data)

# Evaluate the model performance
lm_rmse <- RMSE(lm_predictions, test_data$sale_price)

# Calculate the MSE for each set of predictions
lm_mse <- MSE(lm_predictions, test_data$sale_price)

print(list(lm_rmse = lm_rmse, lm_mse = lm_mse))




# Fit a Random Forest model
set.seed(789) # Random seed for reproducibility
rf_model <- train(
  sale_price ~ .,
  data = train_data,
  method = "rf",
  trControl = train_control,
  tuneGrid = expand.grid(mtry = c(2, sqrt(ncol(train_data) - 1))), # Simplified grid
  ntree = 100 # Set a fixed (and potentially lower) number of trees
)

# Stop the parallel processing after the model is trained
stopImplicitCluster()

# Make predictions on the test set
rf_predictions <- predict(rf_model, test_data)

# Evaluate the model performance
rf_rmse <- RMSE(rf_predictions, test_data$sale_price)

# Calculate the MSE for each set of predictions
rf_mse <- MSE(rf_predictions, test_data$sale_price)

print(list(rf_rmse = rf_rmse, rf_mse = rf_mse))


# Fit a LASSO regression model
set.seed(456)
lasso_model <- train(
  sale_price ~ .,
  data = train_data,
  method = "glmnet",
  trControl = train_control
)

# Make predictions on the test set
lasso_predictions <- predict(lasso_model, test_data)

# Evaluate the model performance
lasso_rmse <- RMSE(lasso_predictions, test_data$sale_price)

# Calculate the MSE for each set of predictions
lasso_mse <- MSE(lasso_predictions, test_data$sale_price)

print(list(lasso_rmse = lasso_rmse, lasso_mse = lasso_mse))


# Combine predictions (e.g., averaging the predictions)
combined_predictions <- (lm_predictions + rf_predictions + lasso_predictions) / 3

# Evaluate the combined model performance
combined_rmse <- RMSE(combined_predictions, test_data$sale_price)

# Print RMSE results
print(list(combined_rmse = combined_rmse))

# Calculate the MSE for each set of predictions
combined_mse <- MSE(combined_predictions, test_data$sale_price)

# Print MSE results
print(list(combined_mse = combined_mse))


# After evaluating the best model approach, predict on the new data
new_data <- predict_property_data

new_data_cols <- c('meta_nbhd', 'char_hd_sf', 'char_bldg_sf', 'char_beds', 'char_fbath', 'char_hbath','geo_fs_flood_factor','geo_fs_flood_risk_direction')

new_data_prepared <- select(new_data, all_of(new_data_cols))
# Predicting values using the best model or combined approach
final_predictions <- (predict(lm_model, new_data_prepared) +
                        predict(rf_model, new_data_prepared) +
                        predict(lasso_model, new_data_prepared)) / 3

# Combine final predictions with the pid column from new data
assessed_values <- data.frame(pid = new_data$pid, assessed_value = final_predictions)

# Ensure that all pid and assessed_value values are non-missing and non-negative
assessed_values <- assessed_values %>%
  filter(!is.na(assessed_value) & assessed_value >= 0)

# Write the assessed values to a CSV file
write_csv(assessed_values, "assessed_value.csv")

summary(assessed_values["assessed_value"])

# Visualizing the distribution of 'assessed_value' using a histogram to the distribution.

ggplot(assessed_values, aes(x = assessed_value)) +
  geom_histogram(binwidth = 50000, fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Market Value", x = "Market Value (in USD)", y = "Frequency")



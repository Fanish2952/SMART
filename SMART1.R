# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(caret)
# Load dataset
student_data <- read.csv("D:/SMART PROJECT/SMART DATASET.csv")
View(student_data)
str(student_data)
summary(student_data)

# ========================
# SMART PROJECT - DATA PREPROCESSING
# ========================

# 1. Remove duplicate rows (if any)
student_data <- student_data %>% distinct()

# 2. Handle missing values by removing rows with NAs (you can also choose to impute)
student_data <- student_data %>% na.omit()

# 3. Convert categorical columns to factors
student_data$Student_Behavior <- as.factor(student_data$Student_Behavior)
student_data$Risky_Behaviors_Engaged <- as.factor(student_data$Risky_Behaviors_Engaged)
student_data$Counseling_Need <- as.factor(student_data$Counseling_Need)
student_data$Placement_Status <- as.factor(student_data$Placement_Status)

# 4. Encode factors to numeric for modeling (temporary encoded dataframe for correlation & regression)
student_data_encoded <- student_data %>%
  mutate(
    Student_Behavior = as.numeric(Student_Behavior),
    Risky_Behaviors_Engaged = as.numeric(Risky_Behaviors_Engaged),
    Counseling_Need = as.numeric(Counseling_Need),
    Placement_Status = as.numeric(Placement_Status)
  )

# Note: Use 'student_data' for plots, and 'student_data_encoded' for correlations/models



# 1. Factors contributing most to overall stress
stress_factors <- student_data %>% select(Total_Stress_Level, Academic_Stress_Level, Financial_Stress_Level, Emotional_Stress_Level)
cor_matrix <- cor(stress_factors, use = "complete.obs")
corrplot(cor_matrix, method = "ellipse", title = "Correlation between Total Stress and Stress Factors")
stress_model <- lm(Total_Stress_Level ~ Academic_Stress_Level + Financial_Stress_Level + Emotional_Stress_Level, data = student_data)
summary(stress_model)

# 2. Influence of family support and parental income
ggplot(student_data, aes(x = Family_Support, y = Total_Stress_Level, color = Parental_Income)) +
  geom_point() +
  labs(title = "Influence of Family Support and Parental Income on Total Stress Level") +
  theme_minimal()
support_income_model <- lm(Total_Stress_Level ~ Family_Support * Parental_Income, data = student_data)
summary(support_income_model)

# 3. Insufficient sleep effects on stress levels
sleep_correlation <- student_data %>% select(Sleep_Hours, Academic_Stress_Level, Emotional_Stress_Level, Total_Stress_Level)
cor_matrix_sleep <- cor(sleep_correlation, use = "complete.obs")
corrplot(cor_matrix_sleep, method = "ellipse", title = "Correlation between Sleep and Stress Levels")
ggplot(student_data, aes(x = Sleep_Hours, y = Total_Stress_Level)) +
  geom_point(color = "blue") +
  labs(title = "Sleep Hours vs. Total Stress Level") +
  theme_minimal()

# 4. CGPA, academic stress, and counseling needs
ggplot(student_data, aes(x = CGPA, y = Academic_Stress_Level)) +
  geom_point(color = "purple") +
  labs(title = "CGPA vs. Academic Stress") +
  theme_minimal()
cgpa_stress_model <- lm(Academic_Stress_Level ~ CGPA, data = student_data)
summary(cgpa_stress_model)
ggplot(student_data, aes(x = CGPA, fill = Counseling_Need)) +
  geom_histogram(bins = 10, alpha = 0.7) +
  labs(title = "Counseling Needs Distribution by CGPA") +
  theme_minimal()

# 5. Behavior patterns and risky behaviors
behavior_data$Student_Behavior <- as.numeric(as.factor(behavior_data$Student_Behavior))
behavior_data$Risky_Behaviors_Engaged <- as.numeric(as.factor(behavior_data$Risky_Behaviors_Engaged))

# Now, calculate the correlation matrix
cor_behavior <- cor(behavior_data, use = "complete.obs")
corrplot(cor_behavior, method = "ellipse", title = "Correlation between Stress and Behaviors")





# Select behavior-related columns and check for non-numeric data types
behavior_data <- student_data %>% select(Total_Stress_Level, Student_Behavior, Risky_Behaviors_Engaged)
str(behavior_data) # Check the structure to identify non-numeric columns

# Convert categorical columns to numeric if they are factors or characters
# Assuming 'Student_Behavior' and 'Risky_Behaviors_Engaged' are categorical, we convert them to numeric
behavior_data$Student_Behavior <- as.numeric(as.factor(behavior_data$Student_Behavior))
behavior_data$Risky_Behaviors_Engaged <- as.numeric(as.factor(behavior_data$Risky_Behaviors_Engaged))

# Now proceed with correlation
cor_behavior <- cor(behavior_data, use = "complete.obs")
corrplot(cor_behavior, method = "ellipse", title = "Correlation between Stress and Behaviors")

# Regression model for predicting risky behaviors
# Make sure the column names in the model match those in the dataset
risky_behavior_model <- lm(Risky_Behaviors_Engaged ~ Total_Stress_Level + Student_Behavior, data = behavior_data)
summary(risky_behavior_model)






# 6. Placement status and financial/academic stress
ggplot(student_data, aes(x = Placement_Status, y = Financial_Stress_Level, fill = Placement_Status)) +
  geom_boxplot() +
  labs(title = "Financial Stress by Placement Status") +
  theme_minimal()

ggplot(student_data, aes(x = Placement_Status, y = Academic_Stress_Level, fill = Placement_Status)) +
  geom_boxplot() +
  labs(title = "Academic Stress by Placement Status") +
  theme_minimal()




##############################################################################
predictions <- predict(stress_model, student_data)

actuals <- student_data$Total_Stress_Level

# Calculate RMSE, MAE, and R-squared
rmse <- sqrt(mean((predictions - actuals)^2))
mae <- mean(abs(predictions - actuals))
r_squared <- summary(stress_model)$r.squared

# Print accuracy metrics
cat("Model Accuracy Metrics:\n")
cat("R-squared:", round(r_squared, 3), "\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("MAE:", round(mae, 3), "\n")




# Train/Test Split
set.seed(123)
train_index <- createDataPartition(student_data$Total_Stress_Level, p = 0.7, list = FALSE)
train_data <- student_data[train_index, ]
test_data <- student_data[-train_index, ]

# Train the model
stress_model <- lm(Total_Stress_Level ~ Academic_Stress_Level + Financial_Stress_Level + Emotional_Stress_Level, data = train_data)

# Make predictions on the test set
test_predictions <- predict(stress_model, test_data)

# Actual values for the test set
test_actuals <- test_data$Total_Stress_Level

# Calculate RMSE, MAE, and R-squared for the test set
rmse_test <- sqrt(mean((test_predictions - test_actuals)^2)) 
mae_test <- mean(abs(test_predictions - test_actuals)) 
r_squared_test <- summary(stress_model)$r.squared

# Print evaluation metrics for the test set
cat("Test Model Accuracy Metrics:\n")
cat("R-squared:", round(r_squared_test, 3), "\n")
cat("RMSE:", round(rmse_test, 3), "\n")
cat("MAE:", round(mae_test, 3), "\n")



# Set a threshold, here using the median of the predictions
threshold <- median(predictions)

# Convert predictions and actual values into binary classes
predictions_class <- ifelse(predictions > threshold, 1, 0)
actuals_class <- ifelse(actuals > threshold, 1, 0)

# Check if both classes are present
if(length(unique(predictions_class)) > 1 & length(unique(actuals_class)) > 1) {
  # Confusion Matrix
  conf_matrix <- confusionMatrix(as.factor(predictions_class), as.factor(actuals_class))
  
  # F1 Score calculation
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  cat("F1 Score: ", round(f1_score, 3))
} else {
  cat("Error: Only one class detected in predictions or actuals. Try adjusting the threshold or check data.")
}

# ========================================
# ADDITIONAL ML MODELS (Random Forest, SVM)
# ========================================

# Load required libraries
library(randomForest)
library(e1071)

# -----------------------------
# Random Forest Regression
# -----------------------------
set.seed(123)
rf_model <- randomForest(
  Total_Stress_Level ~ Academic_Stress_Level + Financial_Stress_Level + Emotional_Stress_Level,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

rf_predictions <- predict(rf_model, test_data)

# Evaluate Random Forest Model
rf_rmse <- sqrt(mean((rf_predictions - test_actuals)^2))
rf_mae <- mean(abs(rf_predictions - test_actuals))
rf_r2 <- 1 - sum((rf_predictions - test_actuals)^2) / sum((test_actuals - mean(test_actuals))^2)

cat("\nRandom Forest Regression:\n")
cat("R-squared:", round(rf_r2, 3), "\n")
cat("RMSE:", round(rf_rmse, 3), "\n")
cat("MAE:", round(rf_mae, 3), "\n")

# -----------------------------
# Support Vector Machine (SVM)
# -----------------------------
set.seed(123)
svm_model <- svm(
  Total_Stress_Level ~ Academic_Stress_Level + Financial_Stress_Level + Emotional_Stress_Level,
  data = train_data
)

svm_predictions <- predict(svm_model, test_data)

# Evaluate SVM Model
svm_rmse <- sqrt(mean((svm_predictions - test_actuals)^2))
svm_mae <- mean(abs(svm_predictions - test_actuals))
svm_r2 <- 1 - sum((svm_predictions - test_actuals)^2) / sum((test_actuals - mean(test_actuals))^2)

cat("\nSupport Vector Machine (SVM) Regression:\n")
cat("R-squared:", round(svm_r2, 3), "\n")
cat("RMSE:", round(svm_rmse, 3), "\n")
cat("MAE:", round(svm_mae, 3), "\n")


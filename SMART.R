# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)

# Load dataset
student_data <- read.csv(file.choose())
View(student_data)
str(student_data)
summary(student_data)
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
ggplot(student_data, aes(x = CGPA, y = Academic_Stress)) +
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
behavior_data <- student_data %>% select(Total_Stress_Level, Student_Behavior, Risky_Behaviors_Engaged)
cor_behavior <- cor(behavior_data, use = "complete.obs")
corrplot(cor_behavior, method = "ellipse", title = "Correlation between Stress and Behaviors")
risky_behavior_model <- lm(Risky_Behaviors ~ Total_Stress_Level + Aggression_Level + Reactive_Behavior, data = student_data)
summary(risky_behavior_model)





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

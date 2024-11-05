# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(ggpubr)


setwd("C:/Users/Cherryl/Desktop/rstudio projj")
# Load the dataset as a data frame
dataset <- as.data.frame(read.csv("Human_Resources.csv"))

# Data Cleaning: Convert Attrition and OverTime to binary factors
dataset$Attrition <- ifelse(dataset$Attrition == "Yes", 1, 0)
dataset$OverTime <- ifelse(dataset$OverTime == "Yes", 1, 0)

# Handling missing values (if any) using median imputation for numeric variables
numeric_vars <- c("JobSatisfaction", "YearsAtCompany", "TotalWorkingYears", 
                  "YearsInCurrentRole", "YearsSinceLastPromotion", "WorkLifeBalance", 
                  "TrainingTimesLastYear", "MonthlyIncome")

dataset[numeric_vars] <- dataset[numeric_vars] %>%
  lapply(function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Impute missing values with median
for (var in numeric_vars) {
  dataset[[var]][is.na(dataset[[var]])] <- median(dataset[[var]], na.rm = TRUE)
}

# Remove outliers for specific numeric variables (example: capping MonthlyIncome to 95th percentile)
dataset$MonthlyIncome <- pmin(dataset$MonthlyIncome, quantile(dataset$MonthlyIncome, 0.95, na.rm = TRUE))

# Display the number of records and the specific fields used
cat("Number of records:", nrow(dataset), "\n")
cat("Fields used:", paste(numeric_vars, collapse = ", "), "\n")

# Generate summary statistics for key variables
summary_stats <- dataset %>%
  summarise(across(all_of(numeric_vars), 
                   list(mean = mean, median = median, sd = sd, IQR = IQR), 
                   na.rm = TRUE))

# Print summary statistics
print(summary_stats)

# Create a table of descriptive statistics using ggplot2
# Convert summary statistics into a data frame for visualization
summary_df <- as.data.frame(t(summary_stats))


# Specific fields used
fields_used <- c("Attrition", "OverTime", "JobSatisfaction", "YearsAtCompany", 
                 "TotalWorkingYears", "YearsInCurrentRole", "YearsSinceLastPromotion", 
                 "WorkLifeBalance", "TrainingTimesLastYear", "MonthlyIncome")
dataset <- dataset %>% select(all_of(fields_used))
###############################################################


# EDA Step 3: Relationship Analysis
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
dataset <- read.csv("Human_Resources.csv", stringsAsFactors = FALSE)

# Data Cleaning: Convert Attrition and OverTime to binary factors
dataset$Attrition <- ifelse(dataset$Attrition == "Yes", 1, 0)
dataset$OverTime <- ifelse(dataset$OverTime == "Yes", 1, 0)

# Handling missing values using median imputation for key numeric variables
numeric_vars <- c("JobSatisfaction", "YearsAtCompany", "TotalWorkingYears", 
                  "YearsInCurrentRole", "YearsSinceLastPromotion", "WorkLifeBalance", 
                  "TrainingTimesLastYear", "MonthlyIncome")

dataset[numeric_vars] <- dataset[numeric_vars] %>%
  lapply(function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Relationship Analysis: Visualizations
# 1. Job Satisfaction vs Attrition (Side-by-side Bars with Trend Lines)
# Corrected trend data preparation for Job Satisfaction
attrition_trend_1 <- dataset %>%
  group_by(JobSatisfaction) %>%
  summarize(AttritionRate = mean(Attrition), NonAttritionRate = mean(1 - Attrition))

# Plot
ggplot(dataset, aes(x = factor(JobSatisfaction), fill = factor(Attrition))) +
  geom_bar(position = "dodge") +
  # Ensure trend data is only using attrition_trend_1
  geom_line(data = attrition_trend_1, aes(x = factor(JobSatisfaction), y = AttritionRate * max(table(dataset$JobSatisfaction)), group = 1), 
            color = "red", size = 1.2, inherit.aes = FALSE) +
  geom_line(data = attrition_trend_1, aes(x = factor(JobSatisfaction), y = NonAttritionRate * max(table(dataset$JobSatisfaction)), group = 1),
            
            
            color = "blue", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = attrition_trend_1, aes(x = factor(JobSatisfaction), y = AttritionRate * max(table(dataset$JobSatisfaction))), 
             color = "red", size = 3, inherit.aes = FALSE) +
  geom_point(data = attrition_trend_1, aes(x = factor(JobSatisfaction), y = NonAttritionRate * max(table(dataset$JobSatisfaction))), 
             color = "blue", size = 3, inherit.aes = FALSE) +
  labs(title = "Attrition by Job Satisfaction", x = "Job Satisfaction Level", y = "Count", fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(table(dataset$JobSatisfaction)), name = "Rate (Attrition vs Non-Attrition)"))

# 2. Years At Company vs Attrition (Side-by-side Bars with Dual Trend Lines)
# Calculate attrition and non-attrition trends for Years At Company
years_company_trend <- dataset %>%
  group_by(YearsAtCompany) %>%
  summarise(AttritionCount = sum(Attrition),
            NonAttritionCount = sum(1 - Attrition),
            TotalCount = n()) %>%
  mutate(AttritionRate = AttritionCount / TotalCount,
         NonAttritionRate = NonAttritionCount / TotalCount)

# Create bar chart with dual trend lines for Years At Company
ggplot() +
  geom_bar(data = dataset, aes(x = factor(YearsAtCompany), fill = factor(Attrition)), 
           position = "dodge", stat = "count") +
  geom_line(data = years_company_trend, aes(x = factor(YearsAtCompany), 
                                            y = AttritionRate * max(TotalCount), 
                                            group = 1), color = "red", linewidth = 1.2) +
  geom_line(data = years_company_trend, aes(x = factor(YearsAtCompany), 
                                            y = NonAttritionRate * max(TotalCount), 
                                            group = 2), color = "blue", linewidth = 1.2) +
  geom_point(data = years_company_trend, aes(x = factor(YearsAtCompany), 
                                             y = AttritionRate * max(TotalCount)), 
             color = "red", size = 3) +
  geom_point(data = years_company_trend, aes(x = factor(YearsAtCompany), 
                                             y = NonAttritionRate * max(TotalCount)), 
             color = "blue", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(years_company_trend$TotalCount), 
                                         name = "Rate (Attrition vs. Non-Attrition)")) +
  labs(title = "Attrition by Years at Company", 
       x = "Years at Company", 
       y = "Count", 
       fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3. Total Working Years vs Attrition (Side-by-side Bars with Dual Trend Lines)
# Calculate attrition and non-attrition trends for Total Working Years
working_years_trend <- dataset %>%
  group_by(TotalWorkingYears) %>%
  summarise(AttritionCount = sum(Attrition),
            NonAttritionCount = sum(1 - Attrition),
            TotalCount = n()) %>%
  mutate(AttritionRate = AttritionCount / TotalCount,
         NonAttritionRate = NonAttritionCount / TotalCount)

# Create bar chart with dual trend lines for Total Working Years
ggplot() +
  geom_bar(data = dataset, aes(x = factor(TotalWorkingYears), fill = factor(Attrition)), 
           position = "dodge", stat = "count") +
  geom_line(data = working_years_trend, aes(x = factor(TotalWorkingYears), 
                                            y = AttritionRate * max(TotalCount), 
                                            group = 1), color = "red", linewidth = 1.2) +
  geom_line(data = working_years_trend, aes(x = factor(TotalWorkingYears), 
                                            y = NonAttritionRate * max(TotalCount), 
                                            group = 2), color = "blue", linewidth = 1.2) +
  
  geom_point(data = working_years_trend, aes(x = factor(TotalWorkingYears), 
                                             y = AttritionRate * max(TotalCount)), 
             color = "red", size = 3) +
  geom_point(data = working_years_trend, aes(x = factor(TotalWorkingYears), 
                                             y = NonAttritionRate * max(TotalCount)), 
             color = "blue", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(working_years_trend$TotalCount), 
                                         name = "Rate (Attrition vs. Non-Attrition)")) +
  labs(title = "Attrition by Total Working Years", 
       x = "Total Working Years", 
       y = "Count", 
       fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 4. Years in Current Role vs Attrition (Side-by-side Bars with Dual Trend Lines)
# Calculate attrition and non-attrition trends for Years in Current Role
current_role_trend <- dataset %>%
  group_by(YearsInCurrentRole) %>%
  summarise(AttritionCount = sum(Attrition),
            NonAttritionCount = sum(1 - Attrition),
            TotalCount = n()) %>%
  mutate(AttritionRate = AttritionCount / TotalCount,
         NonAttritionRate = NonAttritionCount / TotalCount)

# Create bar chart with dual trend lines for Years in Current Role
ggplot() +
  geom_bar(data = dataset, aes(x = factor(YearsInCurrentRole), fill = factor(Attrition)), 
           position = "dodge", stat = "count") +
  geom_line(data = current_role_trend, aes(x = factor(YearsInCurrentRole), 
                                           y = AttritionRate * max(TotalCount), 
                                           group = 1), color = "red", linewidth = 1.2) +
  geom_line(data = current_role_trend, aes(x = factor(YearsInCurrentRole), 
                                           y = NonAttritionRate * max(TotalCount), 
                                           group = 2), color = "blue", linewidth = 1.2) +
  geom_point(data = current_role_trend, aes(x = factor(YearsInCurrentRole), 
                                            y = AttritionRate * max(TotalCount)), 
             color = "red", size = 3) +
  geom_point(data = current_role_trend, aes(x = factor(YearsInCurrentRole), 
                                            y = NonAttritionRate * max(TotalCount)), 
             color = "blue", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(current_role_trend$TotalCount), 
                                         name = "Rate (Attrition vs. Non-Attrition)")) +
  labs(title = "Attrition by Years in Current Role", 
       x = "Years in Current Role", 
       y = "Count", 
       fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 5. Calculate the counts for attrition and non-attrition to derive their rates
attrition_trend <- dataset %>%
  group_by(YearsSinceLastPromotion) %>%
  summarise(AttritionCount = sum(Attrition),
            NonAttritionCount = sum(1 - Attrition),
            TotalCount = n()) %>%
  mutate(AttritionRate = AttritionCount / TotalCount,
         NonAttritionRate = NonAttritionCount / TotalCount)

# Create the bar chart with dual trend lines for attrition and non-attrition
ggplot() +
  geom_bar(data = dataset, aes(x = factor(YearsSinceLastPromotion), fill = factor(Attrition)), 
           position = "dodge", stat = "count") +
  geom_line(data = attrition_trend, aes(x = factor(YearsSinceLastPromotion), 
                                        y = AttritionRate * max(TotalCount), 
                                        group = 1), color = "red", linewidth = 1.2) +
  geom_line(data = attrition_trend, aes(x = factor(YearsSinceLastPromotion), 
                                        y = NonAttritionRate * max(TotalCount),
                                        
                                        group = 2), color = "blue", linewidth = 1.2) +
  geom_point(data = attrition_trend, aes(x = factor(YearsSinceLastPromotion), 
                                         y = AttritionRate * max(TotalCount)), 
             color = "red", size = 3) +
  geom_point(data = attrition_trend, aes(x = factor(YearsSinceLastPromotion), 
                                         y = NonAttritionRate * max(TotalCount)), 
             color = "blue", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(attrition_trend$TotalCount), 
                                         name = "Rate (Attrition vs. Non-Attrition)")) +
  labs(title = "Attrition and Non-Attrition Trends by Years Since Last Promotion", 
       x = "Years Since Last Promotion", 
       y = "Count", 
       fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Work-Life Balance vs Attrition with Trend Lines
# Corrected trend data preparation for Work-Life Balance
attrition_trend_6 <- dataset %>%
  group_by(WorkLifeBalance) %>%
  summarize(AttritionRate = mean(Attrition), NonAttritionRate = mean(1 - Attrition))

# Plot
ggplot(dataset, aes(x = factor(WorkLifeBalance), fill = factor(Attrition))) +
  geom_bar(position = "dodge") +
  geom_line(data = attrition_trend_6, aes(x = factor(WorkLifeBalance), y = AttritionRate * max(table(dataset$WorkLifeBalance)), group = 1), 
            color = "red", size = 1.2, inherit.aes = FALSE) +
  geom_line(data = attrition_trend_6, aes(x = factor(WorkLifeBalance), y = NonAttritionRate * max(table(dataset$WorkLifeBalance)), group = 1), 
            color = "blue", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = attrition_trend_6, aes(x = factor(WorkLifeBalance), y = AttritionRate * max(table(dataset$WorkLifeBalance))), 
             color = "red", size = 3, inherit.aes = FALSE) +
  geom_point(data = attrition_trend_6, aes(x = factor(WorkLifeBalance), y = NonAttritionRate * max(table(dataset$WorkLifeBalance))), 
             color = "blue", size = 3, inherit.aes = FALSE) +
  labs(title = "Attrition by Work-Life Balance", x = "Work-Life Balance Score", y = "Count", fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(table(dataset$WorkLifeBalance)), name = "Rate (Attrition vs Non-Attrition)"))


# 7. Training Times Last Year vs Attrition (Side-by-side Bars with Dual Trend Lines)
# Calculate attrition and non-attrition trends for Training Times Last Year
training_trend <- dataset %>%
  group_by(TrainingTimesLastYear) %>%
  summarise(AttritionCount = sum(Attrition),
            NonAttritionCount = sum(1 - Attrition),
            TotalCount = n()) %>%
  mutate(AttritionRate = AttritionCount / TotalCount,
         NonAttritionRate = NonAttritionCount / TotalCount)

# Create bar chart with dual trend lines for Training Times Last Year
ggplot() +
  geom_bar(data = dataset, aes(x = factor(TrainingTimesLastYear), fill = factor(Attrition)), 
           position = "dodge", stat = "count") +
  geom_line(data = training_trend, aes(x = factor(TrainingTimesLastYear), 
                                       y = AttritionRate * max(TotalCount), 
                                       group = 1), color = "red", linewidth = 1.2) +
  geom_line(data = training_trend, aes(x = factor(TrainingTimesLastYear), 
                                       y = NonAttritionRate * max(TotalCount), 
                                       group = 2), color = "blue", linewidth = 1.2) +
  geom_point(data = training_trend, aes(x = factor(TrainingTimesLastYear), 
                                        y = AttritionRate * max(TotalCount)), 
             color = "red", size = 3) +
  geom_point(data = training_trend, aes(x = factor(TrainingTimesLastYear), 
                                        y = NonAttritionRate * max(TotalCount)),
             
         
             color = "blue", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(training_trend$TotalCount), 
                                         name = "Rate (Attrition vs. Non-Attrition)")) +
  labs(title = "Attrition by Training Times Last Year", 
       x = "Training Times Last Year", 
       y = "Count", 
       fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 8. Monthly Income vs Attrition with Trend Lines
# Corrected trend data preparation for Monthly Income
dataset$IncomeRange <- cut(dataset$MonthlyIncome, breaks = 10, labels = c("0-2k", "2k-4k", "4k-6k", "6k-8k", "8k-10k", "10k-12k", "12k-14k", "14k-16k", "16k-18k", "18k+"))
attrition_trend_8 <- dataset %>%
  group_by(IncomeRange) %>%
  summarize(AttritionRate = mean(Attrition), NonAttritionRate = mean(1 - Attrition))

# Plot
ggplot(dataset, aes(x = IncomeRange, fill = factor(Attrition))) +
  geom_bar(position = "dodge") +
  geom_line(data = attrition_trend_8, aes(x = IncomeRange, y = AttritionRate * max(table(dataset$IncomeRange)), group = 1), 
            color = "red", size = 1.2, inherit.aes = FALSE) +
  geom_line(data = attrition_trend_8, aes(x = IncomeRange, y = NonAttritionRate * max(table(dataset$IncomeRange)), group = 1), 
            color = "blue", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = attrition_trend_8, aes(x = IncomeRange, y = AttritionRate * max(table(dataset$IncomeRange))), 
             color = "red", size = 3, inherit.aes = FALSE) +
  geom_point(data = attrition_trend_8, aes(x = IncomeRange, y = NonAttritionRate * max(table(dataset$IncomeRange))), 
             color = "blue", size = 3, inherit.aes = FALSE) +
  labs(title = "Attrition by Monthly Income", x = "Monthly Income", y = "Count", fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(table(dataset$IncomeRange)), name = "Rate (Attrition vs Non-Attrition)"))

# 9. OverTime vs Attrition with Trend Lines
# Corrected trend data preparation for OverTime
attrition_trend_9 <- dataset %>%
  group_by(OverTime) %>%
  summarize(AttritionRate = mean(Attrition), NonAttritionRate = mean(1 - Attrition))

# Plot
ggplot(dataset, aes(x = factor(OverTime), fill = factor(Attrition))) +
  geom_bar(position = "dodge") +
  geom_line(data = attrition_trend_9, aes(x = factor(OverTime), y = AttritionRate * max(table(dataset$OverTime)), group = 1), 
            color = "red", size = 1.2, inherit.aes = FALSE) +
  geom_line(data = attrition_trend_9, aes(x = factor(OverTime), y = NonAttritionRate * max(table(dataset$OverTime)), group = 1), 
            color = "blue", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = attrition_trend_9, aes(x = factor(OverTime), y = AttritionRate * max(table(dataset$OverTime))), 
             color = "red", size = 3, inherit.aes = FALSE) +
  geom_point(data = attrition_trend_9, aes(x = factor(OverTime), y = NonAttritionRate * max(table(dataset$OverTime))), 
             color = "blue", size = 3, inherit.aes = FALSE) +
  labs(title = "Attrition by OverTime", x = "OverTime (0 = No, 1 = Yes)", y = "Count", fill = "Attrition") +
  scale_fill_manual(values = c("skyblue", "red")) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(table(dataset$OverTime)), name = "Rate (Attrition vs Non-Attrition)"))

######################################################################

# Logistic Regression Model
logistic_model <- glm(Attrition ~ JobSatisfaction + YearsAtCompany + TotalWorkingYears + 
                        YearsInCurrentRole + YearsSinceLastPromotion + WorkLifeBalance + 
                        TrainingTimesLastYear + MonthlyIncome + OverTime,
                      data = dataset, family = binomial)

# Summary of the logistic regression model
summary(logistic_model)

# Extract odds ratios
odds_ratios <- exp(coef(logistic_model))
print(odds_ratios)

# Plot logistic regression coefficients
library(broom)
coeffs <- tidy(logistic_model)
ggplot(coeffs, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Logistic Regression Coefficients", x = "Variables", y = "Coefficient Estimate")


# Fit a CART model using rpart
cart_model <- rpart(Attrition ~ JobSatisfaction + YearsAtCompany + TotalWorkingYears + 
                      YearsInCurrentRole + YearsSinceLastPromotion + WorkLifeBalance + 
                      TrainingTimesLastYear + MonthlyIncome + OverTime,
                    data = dataset, method = "class")

# Plot the decision tree with enhanced readability
rpart.plot(cart_model, 
           main = "Decision Tree for Resignation Prediction",
           type = 2,               # Show split labels beneath the node
           extra = 104,            # Display probability of class and percent of observations
           under = TRUE,           # Place the text under the split label
           fallen.leaves = TRUE,   # Put leaves at the bottom of the plot
           cex = 0.8,              # Control font size
           tweak = 0.7,            # Adjust text size for better readability
           box.palette = "RdYlGn", # Color boxes based on the predicted class
           shadow.col = "gray",    # Add shadow for a 3D effect
           nn = TRUE)              # Display node numbers
# Feature importance
importance <- cart_model$variable.importance
importance_df <- data.frame(Variable = names(importance), Importance = importance)

# Plot feature importance
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance in Decision Tree", x = "Variables", y = "Importance Score")


# Predict probabilities of resignation
dataset$churn_risk_score <- predict(logistic_model, type = "response")

# Categorize risk into high, medium, low based on quantiles
dataset <- dataset %>%
  mutate(risk_category = cut(churn_risk_score, breaks = quantile(churn_risk_score, probs = c(0, 0.3, 0.7, 1)), 
                             labels = c("Low", "Medium", "High"), include.lowest = TRUE))

# Distribution of risk scores
ggplot(dataset, aes(x = churn_risk_score)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Churn Risk Scores", x = "Churn Risk Score", y = "Count")

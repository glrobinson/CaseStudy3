library(ggplot2)
library(dplyr)
library(ROCR)
library(visreg)

# Load the dataset
dataset <- read.csv("restaurant_customer_satisfaction.csv")

# Add binary columns for ratings
dataset <- dataset %>%
  mutate(
    ServiceRatingBinary = ifelse(ServiceRating >= 5, 1, 0),
    FoodRatingBinary = ifelse(FoodRating >= 5, 1, 0),
    AmbianceRatingBinary = ifelse(AmbianceRating >= 5, 1, 0)
  ) %>%
  filter(!is.na(Age), !is.na(Income), !is.na(HighSatisfaction))

dataset <- dataset %>% filter(Age <= 70)

set.seed(123)
dataset_sample <- dataset

# Exploratory Plot: Age vs High Satisfaction
# This plot shows the relationship between customer age and high satisfaction. Overall, there is a smaller amount of individuals that are satisfied across all age groups. 
# High satisfaction values are binary (0 or 1).
ggplot(dataset_sample, aes(x = Age, y = HighSatisfaction)) +
  geom_jitter(width = 0.5, height = 0.05, alpha = 0.5, color = "#3366FF") +
  labs(title = "Age vs High Satisfaction",
       x = "Age", y = "High Satisfaction (Binary)") +
  theme_minimal()

# Conditional Density Plot: Satisfaction vs Age
# This plot shows the probability distribution of satisfaction levels across age groups. The relative density of satisfied vs. unsatisfied customers indicates that there is a slow increase in satisfaction as the age increases.
ggplot(dataset_sample, aes(x = Age, fill = factor(HighSatisfaction))) +
  geom_density(alpha = 0.4, position = "fill") +
  labs(title = "Conditional Density of Age by Satisfaction Level",
       x = "Age", y = "Probability", fill = "High Satisfaction") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean Satisfaction vs Age Group
# This plot shows the average satisfaction level for different age groups. Trends in this plot indicate that older vs. younger customers tend to be more satisfied.
dataset_sample <- dataset_sample %>%
  mutate(AgeGroup = cut(Age, breaks = seq(0, 100, by = 10)))

# Calculate mean satisfaction
mean_satisfaction <- dataset_sample %>%
  group_by(AgeGroup) %>%
  summarise(mean_satisfaction = mean(HighSatisfaction, na.rm = TRUE))

ggplot(mean_satisfaction, aes(x = AgeGroup, y = mean_satisfaction)) +
  geom_point(size = 3) +
  labs(title = "Mean Satisfaction vs Age Group",
       x = "Age Group", y = "Mean Satisfaction") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Mean Satisfaction vs Income Range
# This plot shows the average satisfaction level across different income ranges. This shows that satisfaction increases with income in lower ranges but fluctuates in higher income brackets which suggests diverse expectations.
dataset_sample <- dataset_sample %>%
  mutate(IncomeRange = cut(Income, breaks = seq(0, max(Income, na.rm = TRUE), by = 10000)))

mean_satisfaction_by_income <- dataset_sample %>%
  group_by(IncomeRange) %>%
  summarise(mean_satisfaction = mean(HighSatisfaction, na.rm = TRUE))

ggplot(mean_satisfaction_by_income, aes(x = IncomeRange, y = mean_satisfaction)) +
  geom_point(size = 3, color = "#AA336A") +
  geom_line(group = 1, color = "#AA336A") +
  labs(title = "Mean Satisfaction vs Income Range",
       x = "Income Range", y = "Mean Satisfaction") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter Plot: Age vs Income colored by Satisfaction
# This scatter plot highlights clusters of customers based on age, income, and satisfaction levels. It reveals no strong trends between satisfaction, age, and income which indicates that other factors may play a larger role.
ggplot(dataset_sample, aes(x = Age, y = Income, color = factor(HighSatisfaction))) +
  geom_point(alpha = 0.6) +
  labs(title = "Age vs Income Colored by Satisfaction",
       x = "Age", y = "Income", color = "Satisfaction") +
  theme_minimal()

# Logistic Regression: Predict High Satisfaction
# Logistic regression provides insights into the impact of age, service rating, food rating, and ambiance rating on satisfaction. The coefficients from the model reveal the relative importance of each predictor.
log_reg <- glm(
  HighSatisfaction ~ Age + ServiceRating + FoodRating + AmbianceRating,
  data = dataset_sample,
  family = binomial(link = "logit")
)

# Confidence Intervals
confint(log_reg)

# Model Evaluation: ROC Curve
# This evaluates the logistic regression model using only Age as a predictor. The ROC curve shows the trade off between true positives and false positives.
log_reg_age <- glm(HighSatisfaction ~ Age, data = dataset_sample, family = binomial(link = "logit"))
pred <- predict(log_reg_age, dataset_sample, type = "response")
roc_pred <- prediction(pred, dataset_sample$HighSatisfaction)
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_value <- performance(roc_pred, measure = "auc")@y.values[[1]]

plot(roc_perf, main = paste("ROC Curve - AUC:", round(auc_value, 4)), col = "#2E86C1", lwd = 2)
abline(a = 0, b = 1, col = "gray", lty = 2)

# Random Model for Comparison
# This generates a random baseline model by assigning random satisfaction levels (0 or 1) to each customer. Comparing this to the model AUC indicates that the two models preform similar.
set.seed(1235)
dataset_sample$RandomSatisfaction <- sample(c(0, 1), size = nrow(dataset_sample), replace = TRUE)

random_model <- glm(RandomSatisfaction ~ Age, data = dataset_sample, family = binomial(link = "logit"))
rand_pred <- predict(random_model, dataset_sample, type = "response")
rand_roc <- prediction(rand_pred, dataset_sample$RandomSatisfaction)
rand_perf <- performance(rand_roc, measure = "tpr", x.measure = "fpr")
rand_auc <- performance(rand_roc, measure = "auc")@y.values[[1]]

plot(rand_perf, main = paste("Random Model ROC Curve - AUC:", round(rand_auc, 4)), col = "purple", lwd = 2)
abline(a = 0, b = 1, col = "gray", lty = 2)

# Statistical Tests
# This is a t-test between high satisfaction rates and low-income and high-income groups. It is revealed that that because the p-value is not significant, income does not play a role in customer satisfaction.
low_income_group <- dataset_sample %>% filter(Income <= median(Income, na.rm = TRUE))
high_income_group <- dataset_sample %>% filter(Income > median(Income, na.rm = TRUE))

t_test_income <- t.test(low_income_group$HighSatisfaction, high_income_group$HighSatisfaction)
print(t_test_income)

# Summary Statistics
# Provide high-level insights into satisfaction, age, and income trends.
summary_stats <- dataset_sample %>%
  summarise(
    MeanSatisfaction = mean(HighSatisfaction, na.rm = TRUE),
    MeanAge = mean(Age, na.rm = TRUE),
    MeanIncome = mean(Income, na.rm = TRUE),
    CorrelationAgeIncome = cor(Age, Income, use = "complete.obs")
  )
print(summary_stats)

### Vienna Module ("Data Analytics in R") Pre-Work
### Author: Lance Owen
### Date: 2 December 2025
### Track: Master
### Note: I opted for the Master track as Advanced seemed way too easy for me, 
### but I found this quite challenging. I felt it was better to aim high and 
### miss the mark a bit rather than doing something easy that was not a challenge.

### LOAD LIBRARIES AND IMPORT DATA

library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(vtreat)
library(MLmetrics)
library(rpart.plot)
library(randomForest)
library(pROC)
library(gbm)

df <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/WA_Fn-UseC_-Telco-Customer-Churn.csv')

### QUICK INITAL REVIEW OF OF STRUCTURE, DATA TYPES, ETC.

View(df)
str(df)
glimpse(df)
colnames(df)

### EXPLORATORY DATA ANALYSIS

#Look for missing data
colSums(is.na(df))
#confirm rows with NA values
df_na <- df[rowSums(is.na(df)) > 0, ]
View(df_na)
#drop those 11 rows
df <- na.omit(df)
#check to see new df structure to ensure 11 rows were dropped (from 7043 to 7032)
str(df)

#check numerical columns for outliers using summary stats
df_num <- df %>% select(tenure, MonthlyCharges, TotalCharges)
summary(df_num)

#check histograms
df_num %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "#b0e0e6", color = "darkgrey") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Tenure, Monthly Charges, and Total Charges")

#check again with boxplots
num_long <- df_num %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

ggplot(num_long, aes(x = "", y = value)) +  # x="" to create a single box per variable
  geom_boxplot(fill = "#b0e0e6", color = "darkgrey", outlier.color = "#FCC981", outlier.shape = 8) +
  facet_wrap(~variable, scales = "free") +  # one boxplot per variable
  theme_minimal() +
  labs(title = "Boxplots of Tenure, Monthly Charges, and Total Charges",
       x = "",
       y = "Value")
#from this, I don't really see any outliers in numerical values

#convert character columns (except customerID) to factors
df <- df %>%
  mutate(across(where(is.character) & !customerID, as.factor))
#check
str(df)
summary(df[, sapply(df, is.factor)])

#quick correlation plots for numeric columns
df %>% select(tenure, MonthlyCharges, TotalCharges) %>% pairs()
#Better correlation plots :-)
df_corr <- df %>% select(tenure, MonthlyCharges, TotalCharges)
GGally::ggpairs(df_corr %>% sample_n(min(750, nrow(df_corr))),
                mapping = aes(alpha = 0.4))

# Inspect distribution of churn (our dependent variable)
ggplot(df, aes(x = Churn, fill = Churn)) +
  geom_bar(color = "darkgrey", width = 0.6) +        
  scale_fill_manual(values = c("#017075","#F48D79")) + 
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5, size = 4, color = "#36454F") +
  scale_y_continuous(limits = c(0, 6000)) +
  labs(
    title = "Churn Distribution in Raw Data",
    x = "Churn",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, hjust = 0)
  )

# View distribution of tenure by churn
ggplot(df, aes(x = tenure, color = Churn, fill = Churn)) +
  geom_density(alpha = 0.2) + labs(title = "Tenure Density by Churn")

# View Boxplots for Monthly Charges by Churn
ggplot(df, aes(x = Churn, y = MonthlyCharges)) +
  geom_boxplot() + labs(title = "Monthly Charges by Churn")
# So seems that low tenure (ie new-ish and short-term customers) churn more, as
# do customers who have higher monthly charges

# Look at Contract Type vs Churn in %
df %>%
  group_by(Contract, Churn) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = Contract, y = pct, fill = Churn)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#017075","#F48D79"))+
  labs(title = "Churn Proportion by Contract Type")
# Seems that month-to-month contracts result in higher churn rate than 
# longer-term contracts

str(df)
View(df)

# Note that I used Claude to help with this next section. 
# Convert churn to binary 1/0
df$Churn_numeric <- ifelse(df$Churn == "Yes", 1, 0)

numeric_cols <- c('tenure', 'MonthlyCharges', 'TotalCharges', 'SeniorCitizen')

# Calculate correlations with Churn
correlations <- sapply(df[numeric_cols], function(x) {
  cor(x, df$Churn_numeric, use = "complete.obs")
})

# Get absolute correlations and sort
abs_correlations <- abs(correlations)
sorted_correlations <- sort(abs_correlations, decreasing = TRUE)

# Print results
cat("Correlations with Churn:\n")
print(sorted_correlations)
cat("\nMost correlated variable:", names(sorted_correlations)[1], 
    "with correlation:", sorted_correlations[1], "\n")

# Get the most correlated variable
most_correlated_var <- names(sorted_correlations)[1]

# Create scatterplot jitter and color
ggplot(df, aes_string(x = most_correlated_var, y = "Churn_numeric", color = "Churn")) +
  geom_jitter(alpha = 0.4, height = 0.05, width = 0) +
  scale_color_manual(values = c("No" = "#017075", "Yes" = "#F48D79")) +
  labs(
    x = most_correlated_var,
    y = "Churn",
    title = paste("Scatterplot: Churn vs Tenure"),
    subtitle = paste("Correlation:", round(sorted_correlations[1], 4))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Alternative: Boxplot for better visualization of binary outcome
ggplot(df, aes_string(x = "Churn", y = most_correlated_var, fill = "Churn")) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("No" = "#017075", "Yes" = "#F48D79")) +
  labs(
    x = "Churn",
    y = most_correlated_var,
    title = paste("Distribution of Tenure by Churn Status"),
    subtitle = paste("Correlation:", round(sorted_correlations[1], 4))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

#partition data for models
df_model <- df %>% select(-customerID)
set.seed(123)
trainIndex <- createDataPartition(df_model$Churn, p = 0.7, list = FALSE)
trainDat <- df_model[trainIndex, ]
testDat  <- df_model[-trainIndex, ]

### MODELING (3 TYPES: logistic regression, decision tree, random forest)

# I.Logistic Regression

#define variables
informativeVars <- colnames(df_model)[1:19]
targetVar <- colnames(df_model)[20] 
successClass <- 'Yes' 

# Design "C"ategorical variable plan
plan <- designTreatmentsC(trainDat,
                          informativeVars,
                          targetVar, "Yes")

# Apply to informative variables
treatedX <- prepare(plan, trainDat)

# Fit a logistic regression model
fit <- glm(Churn ~., data = treatedX, family ='binomial')
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
bestFit <- step(fit, direction='backward')
summary(bestFit)

# Model shows better fit and only 15 (of 66) predictors. 
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
treatedTest <- prepare(plan, testDat)
churnPred <- predict(bestFit, treatedTest, type = 'response')
head(churnPred)

# Classify and get confusion matrix
cutoff <- 0.5
churnClasses <- ifelse(churnPred >= cutoff, "Yes", "No")
churnClasses <- factor(churnClasses, levels = c("No", "Yes"))
actualChurn <- factor(testDat$Churn, levels = c("No", "Yes"))
confMat <- confusionMatrix(churnClasses, actualChurn)
confMat

logit_roc <- roc(ifelse(testDat$Churn == "Yes", 1, 0), churnPred)

# II. Decision Tree
## The first time I ran this I got .808 AUC, but for some reason I could not replicate it.
## I'm reimporting the df here in the script, as this seems to fix the issue.

url <- "https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/WA_Fn-UseC_-Telco-Customer-Churn.csv"
df <- read.csv(url)

df_dt <- df %>%
  select(-customerID) %>%
  mutate(across(where(is.character), as.factor)) %>%
  na.omit()

set.seed(123)
trainIndex <- createDataPartition(df_dt$Churn, p = 0.7, list = FALSE)
train <- df_dt[trainIndex, ]
test  <- df_dt[-trainIndex, ]

dec_tree <- rpart(Churn ~ ., data = train, method = "class", control = rpart.control(cp = 0.01))
rpart.plot(dec_tree, type = 3, extra = 104, fallen.leaves = TRUE, cex = 0.6)

# Look at training probabilities
trainProbs <- predict(dec_tree, trainDat) 
head(trainProbs, 10)

# Get the final class and actuals
trainClass <- data.frame(class = colnames(trainProbs)[max.col(trainProbs)],
                         actual = trainDat$Churn)
head(trainClass, 10)

# Confusion Matrix
confMat <- table(trainClass$class,trainClass$actual)
confMat

# Accuracy
sum(diag(confMat))/sum(confMat)

# Now predict on the test set
testProbs <- predict(dec_tree, testDat)

# Get the final class and actuals
testClass<-data.frame(class  = colnames(testProbs)[max.col(testProbs)],
                      actual = testDat$Churn)

# Confusion Matrix
confMat <- table(testClass$class,testClass$actual)
confMat

# Accuracy
sum(diag(confMat))/sum(confMat)

tree_probs <- predict(tree_model, test, type = "prob")[, "Yes"]
tree_class <- predict(tree_model, test, type = "class")
tree_conf <- confusionMatrix(tree_class, test$Churn)
tree_conf

dt_roc <- roc(test$Churn, tree_probs)
dt_roc

# III. Random Forest

url <- "https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/WA_Fn-UseC_-Telco-Customer-Churn.csv"
df <- read.csv(url)

df_rf <- df %>%
  select(-customerID) %>%
  mutate(across(where(is.character), as.factor)) %>%
  na.omit()

set.seed(123)
trainIndex <- createDataPartition(df_rf$Churn, p = 0.7, list = FALSE)
train_rf <- df_rf[trainIndex, ]
test_rf  <- df_rf[-trainIndex, ]

set.seed(123)
rf <- randomForest(Churn ~ ., data = train_rf, ntree = 500,
                         mtry = floor(sqrt(ncol(train)-1)), importance = TRUE,
                         proximity = TRUE)
print(rf)

# Variable importance plot
varImpPlot(rf, main="Random Forest // Variable Importance")

# Predict on test set
rf_probs <- predict(rf, test_rf, type = "prob")[, "Yes"] 
rf_class <- predict(rf, test_rf, type = "class")

# Confusion matrix
confMat <- confusionMatrix(rf_class, test_rf$Churn)
print(confMat)

# ROC curve and AUC
actual <- ifelse(test_rf$Churn == "Yes", 1, 0)
roc_obj <- roc(actual, rf_probs)
auc_val <- auc(roc_obj)
cat("AUC:", round(auc_val,3), "\n")

rf_roc <- roc(ifelse(test_rf$Churn == "Yes", 1, 0), rf_probs)
rf_roc

# Try and Tune Random Forest with caret
rf_grid <- expand.grid(mtry = c(3, 5, 7, 10))
set.seed(123)
rf_caret <- train(Churn ~ ., data = train_rf, method = "rf",
                  tuneGrid = rf_grid,
                  ntree = 500,
                  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary),
                  metric = "ROC")
print(rf_caret)

# Predict on test set
rf_probs2 <- predict(rf_caret, test_rf, type = "prob")[, "Yes"]  # probabilities for "Yes"
rf_class2 <- predict(rf_caret, test_rf, type = "raw")

# Confusion matrix
confMat2 <- confusionMatrix(rf_class2, test_rf$Churn)
print(confMat2)

# roc
rf2_roc <- roc(ifelse(testDat$Churn == "Yes", 1, 0), rf_probs2)
rf2_roc

# Comparing the four ROC curves in a plot
plot(logit_roc, col = "#017075", lwd = 2, main = "ROC Curve Comparison")
plot(dt_roc, col = "#F48D79", lwd = 2, add = TRUE)
plot(rf_roc, col = "#FCC981", lwd = 2, add = TRUE)
plot(rf2_roc, col = "navy", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Logistic", "Decision Tree", "Random Forest", "Random Forest Tuned"),
       col = c("#017075", "#F48D79", "#FCC981", "navy"), lwd = 2)


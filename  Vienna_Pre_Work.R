### Vienna Module ("Data Analytics in R") Pre-Work
### Author: Lance Owen
### Date: 2 December 2025
### Track: Advanced

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

logit_roc <- roc(ifelse(testDat$Churn == "Yes", 1, 0), logit_probs)


# II. Decision Tree

dec_tree <- rpart(Churn ~ ., data = trainDat, method = "class", 
                 minsplit = 1, minbucket = 1, cp=-1, control = rpart.control(cp = 0.01))
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

dt_roc <- roc(ifelse(testDat$Churn == "Yes", 1, 0), tree_probs)


# III. Random Forest

set.seed(123)
rf <- randomForest(Churn ~ ., data = trainDat, ntree = 500,
                         mtry = floor(sqrt(ncol(train)-1)), importance = TRUE,
                         proximity = TRUE)
print(rf)

# Variable importance plot
varImpPlot(rf, main="Random Forest // Variable Importance")

# Predict on test set
rf_probs <- predict(rf, testDat, type = "prob")[, "Yes"] 
rf_class <- predict(rf, testDat, type = "class")

# Confusion matrix
confMat <- confusionMatrix(rf_class, testDat$Churn)
print(confMat)

# ROC curve and AUC
actual <- ifelse(testDat$Churn == "Yes", 1, 0)
roc_obj <- roc(actual, pred_probs)
auc_val <- auc(roc_obj)
cat("AUC:", round(auc_val,3), "\n")

rf_roc <- roc(ifelse(testDat$Churn == "Yes", 1, 0), rf_probs)

# Try and Tune Random Forest with caret
rf_grid <- expand.grid(mtry = c(3, 5, 7, 10))
set.seed(123)
rf_caret <- train(Churn ~ ., data = trainDat, method = "rf",
                  tuneGrid = rf_grid,
                  ntree = 500,
                  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary),
                  metric = "ROC")
print(rf_caret)

# Predict on test set
rf_probs2 <- predict(rf_caret, testDat, type = "prob")[, "Yes"]  # probabilities for "Yes"
rf_class2 <- predict(rf_caret, testDat, type = "raw")

# Confusion matrix
confMat2 <- confusionMatrix(pred_class2, testDat$Churn)
print(confMat2)

# roc
rf2_roc <- roc(ifelse(testDat$Churn == "Yes", 1, 0), rf_probs2)

# Comparing the four ROC curves in a plot
plot(logit_roc, col = "#017075", lwd = 2, main = "ROC Curve Comparison")
plot(dt_roc, col = "#F48D79", lwd = 2, add = TRUE)
plot(rf_roc, col = "#FCC981", lwd = 2, add = TRUE)
plot(rf2_roc, col = "navy", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Logistic", "Decision Tree", "Random Forest", "Random Forest Tuned"),
       col = c("#017075", "#F48D79", "#FCC981", "navy"), lwd = 2)


# Compare 

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

fit_logit <- train(Churn ~ ., data = trainDat, 
                   method = "glm", trControl = ctrl, metric = "ROC")

fit_gbm <- train(Churn ~ ., data = trainDat, 
                 method = "gbm", trControl = ctrl, metric = "ROC")

fit_rf <- train(Churn ~ ., data = trainDat, 
                method = "rf", trControl = ctrl, metric = "ROC")

models <- resamples(list(
  Logit = fit_logit,
  GBM = fit_gbm,
  RF = fit_rf
))

#See comparisons
summary(models)
bwplot(models, metric = "ROC")
dotplot(models, metric = "ROC")

#Are ROC differences statistically significant?
differences <- diff(models)
summary(differences)



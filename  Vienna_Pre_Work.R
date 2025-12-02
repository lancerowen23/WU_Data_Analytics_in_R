### Vienna Module (Data Analytics in R) Pre-Work
### Author: Lance Owen
### Date: 2 December 2025
### Track: Advanced

library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)

# Import data
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
ggplot(df, aes(x = Churn)) +
  geom_bar() + labs(title = "Churn distribution in Raw Data")

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

# Look at Contract Type vs Churn in %
df %>%
  group_by(Contract, Churn) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = Contract, y = pct, fill = Churn)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#017075","#F48D79"))+
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Churn Proportion by Contract Type")

str(df)
View(df)


### MODELING

# I.Logistic Regression

#define variables
informativeFeatures <- colnames(df)[2:20] #removes CustomerID
targetVariable      <- colnames(df)[21] #churn!
successClass        <- 'Yes' #looking for what leads to a churn (departing customer)

#partition data
splitPercent <- round(nrow(df) %*% .9)
totalRecords <- 1:nrow(df)
set.seed(1234)
df_part <- sample(totalRecords, splitPercent)
trainDat <- df[df_part,]
testDat  <- df[-df_part,]

ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(123)
model_logit <- train(
  Churn ~ ., 
  data = trainDat,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

print(model_logit)



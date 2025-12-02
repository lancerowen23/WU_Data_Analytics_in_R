### Vienna Module (Data Analytics in R) Pre-Work
### Author: Lance Owen
### Date: 2 December 2025
### Track: Advanced

library(ggplot2)
library(dplyr)
library(tidyverse)

# Import data
df <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/WA_Fn-UseC_-Telco-Customer-Churn.csv')

### QUICK REVIEW OF OF STRUCTURE, DATA TYPES, ETC.

View(df)
str(df)
glimpse(df)
colnames(df)

### EXPLORATORY DATA ANALYSIS

#quick correlation plots for numeric columns
df %>% select(tenure, MonthlyCharges, TotalCharges) %>% pairs()

#Better correlation plots :-)
df_corr <- df %>% select(tenure, MonthlyCharges, TotalCharges)
GGally::ggpairs(df_corr %>% sample_n(min(750, nrow(df_corr))),
                mapping = aes(alpha = 0.4))

# Inspect distribution of churn (our dependent variable)
ggplot(df, aes(x = Churn)) +
  geom_bar() + labs(title = "Churn distribution in Raw Data")

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
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Churn Proportion by Contract Type")

### INSPECT MISSING DATA 
colSums(is.na(df))

#drop NA rows (see 11 in TotalCharges)

#see rows with NA values
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
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Tenure, Monthly Charges, and Total Charges")

#check again with boxplots
num_long <- df_num %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")
ggplot(num_long, aes(x = "", y = value)) +  # x="" to create a single box per variable
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 8) +
  facet_wrap(~variable, scales = "free") +  # one boxplot per variable
  theme_minimal() +
  labs(title = "Boxplots of Tenure, Monthly Charges, and Total Charges",
       x = "",
       y = "Value")
#from this, I don't really see any outliers in numerical values

# Get tallys for character and categorical columns
tally_cols <- names(df)[sapply(df, is.character)]

for (col in tally_cols) {
  cat("------------\n")
  cat("Column:", col, "\n")
  print(table(df[[col]], useNA = "ifany"))
}


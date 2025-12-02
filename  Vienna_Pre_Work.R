### Vienna R Module Pre-Work
### Author: Lance Owen
### Track: Advanced

# Import data
df <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/WA_Fn-UseC_-Telco-Customer-Churn.csv')

### Quick view of df structure

str(df)
glimpse(df)
colnames(df)

### EDA of df

#quick correlation plots
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

### Inspect Missing Data
colSums(is.na(df))

# Get tallys for character and categorical columns
tally_cols <- names(df)[sapply(df, is.character)]

for (col in tally_cols) {
  cat("------------\n")
  cat("Column:", col, "\n")
  print(table(df[[col]], useNA = "ifany"))
}


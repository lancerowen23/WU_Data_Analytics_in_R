### Vienna Module ("Advanced Data Analytics") Pre-Work
### Author: Lance Owen
### Date: 2 December 2025

### LOAD LIBRARIES AND IMPORT DATA
# Museum and Galleries Monthly Visits: January 2019 to September 2025 
# Department for Culture, Media & Sport, United Kingdom
# Source: https://www.gov.uk/government/statistical-data-sets/museums-and-galleries-monthly-visits

library(dplyr)
library(scales)
options(scipen = 999)

uk <- read.csv('https://raw.githubusercontent.com/lancerowen23/WU_Data_Analytics_in_R/refs/heads/main/UK_Museum_Monthly_Visits.csv')

str(uk)
View(uk)
colnames(uk)

# Drop extra Museum name/group columns
uk <- uk %>% select(-Museum.Group, -Coverage)

# Count NAs
colSums(is.na(uk))

# Drop last column (where NAs are)
uk <- uk[ , -ncol(uk)]
colSums(is.na(uk))

# Get top musuems by total visitor volume
uk$VisitorTotal <- rowSums(uk[ , -1], na.rm = TRUE)
uk %>% select(Museum.Name, VisitorTotal) %>% 
  arrange(desc(VisitorTotal))

# Remove rows that are not for individual museum properties
uk <- uk %>%
  filter(!grepl("Total", Museum.Name, ignore.case = TRUE))
uk %>% select(Museum.Name, VisitorTotal) %>% 
  arrange(desc(VisitorTotal))

uk_top5 <- uk %>% arrange(desc(VisitorTotal)) %>% 
                            slice_head(n = 5) %>% 
                            select(-VisitorTotal)

uk_long <- uk_top5 %>%
  pivot_longer(
    cols = -Museum.Name,       # all month columns
    names_to = "Month",        # new column for month names
    values_to = "Visits"       # new column for visit counts
  ) %>%
  mutate(
    # Convert month names like "January.2019" to Date
    Month = as.Date(paste0("01-", Month), format = "%d-%B.%Y")
  )


# Filter for British Museum
british_museum <- uk_long %>%
  filter(Museum.Name == "British Museum") %>%
  arrange(Month)

# Create a ts object (monthly data)
bm_ts <- ts(british_museum$Visits, 
            start = c(2019, 1),  # year, month of first observation
            frequency = 12)      # 12 months per year

bm_ts

plot(bm_ts, 
     main = "British Museum Monthly Visits",
     ylab = "Visitors",
     xlab = "Time")


library(ggplot2)
library(scales)  # for comma formatting

# Optional: turn off scientific notation
options(scipen = 999)

# Clean, pretty plot of all museums
top_5 <- ggplot(uk_long, aes(x = Month, y = Visits, color = Museum.Name)) +
  geom_line(size = 1.2) +                    # thicker lines
  geom_point(size = 1.5, alpha = 0.7) +     # optional points
  scale_y_continuous(labels = scales::comma) +      # format y-axis with commas
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +  # nice x-axis
  labs(
    title = "Monthly Visits to 5 Busiest UK Museums",
    subtitle = "January 2019 to September 2025",
    x = "",
    y = "Visitors",
    color = "Museum",
    caption = "Data Source: UK Department for Culture, Media & Sport"
  ) +
  theme_minimal(base_size = 14) +     
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank() 
  )


# Just British Museum

bm <- uk_long %>% filter(Museum.Name == "British Museum")

bm_visits <- ggplot(bm, aes(x = Month, y = Visits, color = Museum.Name)) +
  geom_line(size = 1.2) +                    # thicker lines
  geom_point(size = 1.5, alpha = 0.7) +     # optional points
  scale_y_continuous(labels = scales::comma) +      # format y-axis with commas
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +  # nice x-axis
  labs(
    title = "Monthly Visits to The British Museum",
    subtitle = "January 2019 to September 2025",
    x = "",
    y = "Visitors",
    color = "Museum",
    caption = "Data Source: UK Department for Culture, Media & Sport"
  ) +
  theme_minimal(base_size = 14) +     
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    legend.title = element_blank() 
  )

# Add annotations
bm_visits_annot <- ggplot(bm, aes(x = Month, y = Visits, color = Museum.Name)) +
geom_line(size = 1.2) +                    # thicker lines
  geom_point(size = 1.5, alpha = 0.7) +     # optional points
  scale_y_continuous(labels = scales::comma) +      # format y-axis with commas
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +  # nice x-axis
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", color = "#1C1C1C", size = .5) +
  annotate("text", x = as.Date("2020-03-23"), y = max(uk_long$Visits)*0.99, 
           label = "1st COVID Lockdown", color = "#1C1C1C", angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(xintercept = as.Date("2020-11-05"), linetype = "dashed", color = "#1C1C1C", size = .5) +
  annotate("text", x = as.Date("2020-11-05"), y = max(uk_long$Visits)*0.99, 
           label = "2nd COVID Lockdown", color = "#1C1C1C", angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(xintercept = as.Date("2021-01-04"), linetype = "dashed", color = "#1C1C1C", size = .5) +
  annotate("text", x = as.Date("2021-01-04"), y = max(uk_long$Visits)*0.99, 
           label = "3rd COVID Lockdown", color = "#1C1C1C", angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(xintercept = as.Date("2023-07-01"), linetype = "dashed", color = "#1C1C1C", size = .5) +
  annotate("text", x = as.Date("2023-07-01"), y = max(uk_long$Visits)*0.25, 
           label = "Summer Tourism Peak", color = "#1C1C1C", angle = 90, vjust = -0.5, hjust = .25) +
  geom_vline(xintercept = as.Date("2024-07-01"), linetype = "dashed", color = "#1C1C1C", size = .5) +
  annotate("text", x = as.Date("2024-07-01"), y = max(uk_long$Visits)*0.25, 
           label = "Summer Tourism Peak", color = "#1C1C1C", angle = 90, vjust = -0.5, hjust = .25) +
  geom_vline(xintercept = as.Date("2025-07-01"), linetype = "dashed", color = "#1C1C1C", size = .5) +
  annotate("text", x = as.Date("2025-07-01"), y = max(uk_long$Visits)*0.25, 
           label = "Summer Tourism Pea", color = "#1C1C1C", angle = 90, vjust = -0.5, hjust = .25)+
  labs(
    title = "Monthly Visits to The British Museum",
    subtitle = "January 2019 to September 2025",
    x = "",
    y = "Visitors",
    color = "Museum",
    caption = "Data Source: UK Department for Culture, Media & Sport"
  ) +
  theme_minimal(base_size = 14) +     
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    legend.title = element_blank() 
  )


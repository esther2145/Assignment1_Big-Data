# ESTHER KICA
# B27293
# ASSIGNMENT 1 DESCRIPTIVE ANALTICS


library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)

#importing dataset
df <- read_csv("C:/Users/USER/OneDrive/Documents/Big_data/ida_credits_to_uganda_09-20-2025.csv")
View(df)

#data exploration
glimpse(df)
summary(df)
sum(is.na(df))#54 missing values

# ======
# QUESTION 1: TIME-SERIES ANALYSIS OF DISBURSED AMOUNT
# ======
#cleaning and converting last disbursement date
df <- df %>%
  mutate(
    last_disb_date = as.Date(`Last Disbursement Date`, format = "%m/%d/%Y"),
    year = year(last_disb_date)
  )
#filling missing values with the mean
sum(is.na(df$`Last Disbursement Date`)) #43 missing values

df <- df %>%
  mutate(
    last_disb_date = ifelse(
      is.na(last_disb_date),
      as.Date(`Agreement Signing Date`, format = "%m/%d/%Y"),
      last_disb_date
    ),
    last_disb_date = as.Date(last_disb_date, origin = "1970-01-01"),
    year = year(last_disb_date)
  )

#aggregating by year
disb_by_year <- df %>%
  group_by(year) %>%
  summarise(total_disbursed = sum(`Disbursed Amount (US$)`, na.rm = TRUE)) %>%
  arrange(year)
disb_by_year

#plotting the time series
ggplot(disb_by_year, aes(x = year, y = total_disbursed)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "World Bank Disbursements to Uganda",
       x = "Year", y = "Total Disbursed (US$)") +
  theme_minimal()


# Plot time-series with 3-year moving average and linear trend
disb_by_year$ma3 <- stats::filter(disb_by_year$total_disbursed,
                                  filter = rep(1/3, 3), 
                                  sides = 2)

p <- ggplot(disb_by_year, aes(x = year, y = total_disbursed)) +
  geom_line() + geom_point() +
  geom_line(aes(y = ma3), linetype = "dashed") +
  labs(title = "World Bank Disbursed Amounts to Uganda by Year",
       x = "Year", y = "Disbursed Amount (US$)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
print(p)

# Linear trend coefficients
lm_fit <- lm(total_disbursed ~ year, data = disb_by_year)
cat("Linear trend slope (USD/year):", coef(lm_fit)[2], "\n")
#On average, World Bank disbursements to Uganda increase by about 8 million USD every year.



# ======
# QUESTION 2 : CREDIT STATUS 
# ======

# Credit Status summary
table(df$`Credit Status`)
#Fully Repaid → 96 projects
#Repaying → 65 projects
#Disbursing → 10 projects
#Disbursing & Repaying → 8 projects
#Others (Cancelled, Terminated, Approved, Effective) → <10 
#most credits in the dataset are either already fully repaid or are in the process of being repaid..

# Analyze credit status distribution
credit_status_summary <- data %>%
  group_by(`Credit Status`) %>%
  summarise(
    count = n(),
    total_amount = sum(`Disbursed Amount (US$)`, na.rm = TRUE),
    avg_amount = mean(`Disbursed Amount (US$)`, na.rm = TRUE),
    percentage = n() / nrow(data) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(count))

print(credit_status_summary)

# Plot Credit Status Distribution by Count
p2 <- ggplot(credit_status_summary, aes(x = reorder(`Credit Status`, count), y = count)) +
  geom_col(fill = "#3498DB", alpha = 0.8) +
  geom_text(aes(label = count), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Distribution of Credit Status",
    subtitle = "Number of Credits by Status",
    x = "Credit Status",
    y = "Number of Credits"
  ) +
  theme_minimal()

print(p2)


# Plot Credit Status by Total Amount
p3 <- ggplot(credit_status_summary, aes(x = reorder(`Credit Status`, total_amount), y = total_amount)) +
  geom_col(fill = "#E74C3C", alpha = 0.8) +
  geom_text(aes(label = paste0("$", round(total_amount/1e6, 1), "M")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0("$", x/1e6, "M")) +
  labs(
    title = "Total Disbursed Amount by Credit Status",
    x = "Credit Status",
    y = "Total Disbursed Amount (US$ Millions)"
  ) +
  theme_minimal()

print(p3)

# Credit status over time
status_over_time <- df %>%
  group_by(year, `Credit Status`) %>%
  summarise(
    count = n(),
    total_amount = sum(`Disbursed Amount (US$)`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(year))

# Plot Credit Status Evolution Over Time
p4 <- ggplot(status_over_time, aes(x = year, y = count, fill = `Credit Status`)) +
  geom_area(alpha = 0.7) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "Evolution of Credit Status Over Time",
    x = "Year",
    y = "Number of Credits",
    fill = "Credit Status"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p4)
# ======
# QUESTION 3: ORIGINAL PRINCIPAL AMOUNT ANALYSIS
# ======

#Original principal analysis

yearly_principal <- df %>%
  group_by(year) %>%
  summarise(
    total_principal = sum(`Original Principal Amount (US$)`, na.rm = TRUE),
    avg_principal = mean(`Original Principal Amount (US$)`, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(year))
colnames(df)
# Plot 6: Original Principal Amount Trends
p5 <- ggplot(yearly_principal, aes(x = year)) +
  geom_col(aes(y = total_principal), fill = "#16A085", alpha = 0.7) +
  geom_line(aes(y = avg_principal * 10), color = "#E74C3C", size = 1.5) +
  scale_y_continuous(
    name = "Total Principal Amount (US$ Millions)",
    labels = function(x) paste0("$", x/1e6, "M"),
    sec.axis = sec_axis(~ . / 10, 
                        name = "Average Principal per Credit (US$ Millions)",
                        labels = function(x) paste0("$", x/1e6, "M"))
  ) +
  labs(
    title = "Original Principal Amount Analysis Over Time",
    subtitle = "Bars: Total Principal | Line: Average Principal per Credit",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "#16A085"),
    axis.title.y.right = element_text(color = "#E74C3C")
  )

print(p5)

# Plot 7: Distribution of Principal Amounts
p6 <- ggplot(df, aes(x = `Original Principal Amount (US$)`)) +
  geom_histogram(bins = 30, fill = "#9B59B6", alpha = 0.7, color = "white") +
  scale_x_continuous(labels = function(x) paste0("$", x/1e6, "M")) +
  labs(
    title = "Distribution of Original Principal Amounts",
    x = "Original Principal Amount (US$ Millions)",
    y = "Frequency"
  ) +
  theme_minimal()

print(p6)


# Summary statistics
total_original <- sum(df$`Original Principal Amount (US$)`, na.rm = TRUE)#the total original principal across all records 11,879,931,052.66
mean_orig <- mean(df$`Original Principal Amount (US$)`, na.rm = TRUE)#The mean original principal per record is 62,198,591.90
median_orig <- median(df$`Original Principal Amount (US$)`, na.rm = TRUE)#the median is 29,000,000.00 indicating a right-skewed distribution (a small number of very large loans raise the mean above the median).
cat("Total original principal:", scales::comma(total_original), "\n")
cat("Mean:", scales::comma(mean_orig), "Median:", scales::comma(median_orig), "\n")


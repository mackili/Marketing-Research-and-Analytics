library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(lubridate)


# Task 1
dataset <- read.csv("Assignments/Assignment 2/dataset.csv")

# Overview of the data structure
summary(dataset)

## No nulls, need to transform dates to date
## Flier to be transformed as factor

dataset$Date = parse_date(dataset$Date) #Convert Date to date format
dataset$flyer = as.factor(dataset$flyer) #Convert Date to date format
summary(dataset)

# No need to analyze the price as it is a dimension, not a measure

hist(dataset$items_sold)
boxplot(dataset$items_sold) # Just number of items sold
boxplot(dataset$items_sold ~ dataset$flyer) # Including the flyer as factor

# Create a new column for quarter and year
dataset <- dataset %>%
  mutate(Quarter_Year = paste0(year(Date), " Q", quarter(Date)))

dataset$Month_Year <- paste0(year(dataset$Date), '-', month(dataset$Date))

# Aggregate sales by quarter and year
quarterly_sales <- dataset %>%
  group_by(Quarter_Year) %>%
  summarise(total_items_sold = sum(items_sold))

monthly_sales <- dataset %>%
  group_by(Month_Year) %>%
  summarise(total_items_sold = sum(items_sold))

weekday_sales <- dataset %>%
  group_by(weekday) %>%
  summarise(total_items_sold = mean(items_sold)) 

weekday_sales$day_number <- recode(weekday_sales$weekday, 
       "Montag"=0,
       "Dienstag"=1,
       "Mittwoch"=2,
       "Donnerstag"=3,
       "Freitag"=4,
       "Samstag"=5,
       "Sonntag"=6)

weekday_sales <- weekday_sales %>% arrange(day_number)

# Convert Quarter_Year to a factor with the correct order
quarterly_sales <- quarterly_sales %>%
  mutate(Quarter_Year = factor(Quarter_Year, levels = sort(unique(Quarter_Year))))
# Convert Month_Year to a factor with the correct order
monthly_sales <- monthly_sales %>%
  mutate(Month_Year = factor(Month_Year, levels = sort(unique(Month_Year))))

# Plot the aggregated sales
plot_quarterly <- ggplot(quarterly_sales, aes(x = Quarter_Year, y = total_items_sold)) +
  geom_line(group = 1) +
  geom_point() +
  labs(x = "Quarter", y = "Total Items Sold", title = "Quarterly Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_quarterly

plot_quarterly_col <- ggplot(quarterly_sales, aes(x = Quarter_Year, y = total_items_sold)) +
  geom_col()
  labs(x = "Quarter", y = "Total Items Sold", title = "Quarterly Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_quarterly_col

## There is some seasonal decrease in Q4, overall trend is decreasing

plot_monthly <- ggplot(monthly_sales, aes(x = Month_Year, y = total_items_sold)) +
  geom_line(group = 1) +
  geom_point() +
  labs(x = "Month", y = "Total Items Sold", title = "Monthly Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_monthly

plot_monthly_col <- ggplot(monthly_sales, aes(x = Month_Year, y = total_items_sold)) +
  geom_col() +
  labs(x = "Month", y = "Total Items Sold", title = "Monthly Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_monthly_col

## Hard to see the decreasing trend here, but quarters suggest so

plot_weekday_col <- ggplot(weekday_sales, aes(x = weekday, y = total_items_sold)) +
  geom_col() + 
  labs(x = "Weekday", y = "Average Items Sold", title = "Weekday Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_weekday_col

## Days seem to be quite equal, with Wednesdays being the peak days in terms of average sales volume

## Frequency of price changes

price_change_plot <- ggplot(dataset, aes(x = Date, y = price)) +
    geom_line() +
    labs(x = 'Date', y = 'Price', title = 'Price change over time')

price_change_plot

# Calculate percentage price change from the previous price
dataset <- dataset %>%
  arrange(Date) %>%
  mutate(price_change_pct = (price - lag(price)) / lag(price) * 100)

price_changes_only <- dataset %>%
    filter(price_change_pct != 0)

price_change_percent_hist <- ggplot(price_changes_only, aes(x = price_change_pct)) +
    geom_histogram() + stat_bin(bins = 10) +
    labs(x = 'Percentage change', y = 'Occurence', title = 'Price change percent frequency')

price_change_percent_hist
mean_price_change <- mean(abs(price_changes_only$price_change_pct))
## Prices change 12% on average

average_price_duration <- nrow(dataset) / nrow(price_changes_only)

## We assume that promotions progression = frequency of flyers at a time
flyer_days_per_month <- dataset %>%
    group_by(year = year(Date), month = month(Date)) %>% 
    summarise(days_with_flyer = sum(flyer == 1)) %>%
    arrange(year, month)

flyer_days_per_month_plot <- ggplot(flyer_days_per_month, aes(x = paste0(year, '-', month), y = days_with_flyer, group =1)) +
    geom_line() +
    labs(x = 'Month', y = 'Number of flyers', title = 'Pprogression of promotions over time')

flyer_days_per_month_plot
## No pattern visible

flyer_days_per_quarter <- dataset %>%
    group_by(year = year(Date), quarter = quarter(Date)) %>% 
    summarise(days_with_flyer = sum(flyer == 1)) %>%
    arrange(year, quarter)

flyer_days_per_quarter_plot <- ggplot(flyer_days_per_quarter, aes(x = paste0(year, '-', quarter), y = days_with_flyer, group =1)) +
    geom_line() +
    labs(x = 'Quarter', y = 'Number of flyers', title = 'Pprogression of promotions over time')

flyer_days_per_quarter_plot
## A clear decreasing trend visible, with a slight increase in Q3 2021. 

# Regression analysis
dataset <- read.csv("Assignments/Assignment 2/dataset.csv")
dataset$Date = parse_date(dataset$Date) #Convert Date to date format
dataset$flyer = as.factor(dataset$flyer) #Convert Date to date format

dataset_reg <- dataset %>%
    mutate(month = month(Date), quarter = quarter(Date)) %>%
    mutate(weekday = as.factor(weekday))

sales_reg = lm(items_sold ~ weekday + month + quarter + price + flyer, dataset_reg)
summary(sales_reg)

## The model has an "ok" R^2 of .5344. And a very low p-value close to 0.00
plot(sales_reg$residuals)
sd(sales_reg$residuals)
## Most of variables are insignificant, hence we will start removing them
## Factors for weekdays are largely insignificant. We can leave wednesday as it has the only acceptable significance level

sales_reg2 = lm(items_sold ~ as.factor(weekday == 'Mittwoch') + month + quarter + price + flyer, dataset_reg)
summary(sales_reg2)

## The model has improved. We have a very similar performance in terms of R^2
## We can proceed with variable removal. Let's start with month, as it is also insignificant.

sales_reg3 = lm(items_sold ~ as.factor(weekday == 'Mittwoch') + quarter + price + flyer, dataset_reg)
summary(sales_reg3)
## Now all variables are significant, the R^2 is still high at .5338
## Model's p-value is low and acceptable

dataset_reg$regression_result <- predict(sales_reg3, dataset_reg)

# Plot the actual items_sold on x and regression_result on y as points with different colors
plot_actual_vs_predicted <- ggplot(dataset_reg, aes(x = items_sold, y = regression_result)) +
  geom_point() +
  labs(x = "Actual Items Sold", y = "Predicted Items Sold", title = "Actual vs Predicted Items Sold") +
  theme_minimal()

plot_actual_vs_predicted


# Group comparisons
t.test(dataset[dataset$flyer==1,]$items_sold, 
    dataset[dataset$flyer==0,]$items_sold)
## The t-test rejects the null hypothesis with p-value < 0.05
## meaning that we can assume that there is a difference in sample means
boxplot_flyer <- ggplot(dataset, aes(x = as.factor(flyer), y = items_sold)) +
  geom_boxplot() +
  labs(x = "Flyer", y = "Items Sold", title = "Items Sold with and without Flyer") +
  scale_x_discrete(labels = c("0" = "No Flyer", "1" = "Flyer"))

boxplot_flyer


## anova test
anova_test <- aov(items_sold ~ weekday, data = dataset)
summary(anova_test)
## p-value > 0.05, hence we reject the null hypothesis
boxplot_weekday <- ggplot(dataset, aes(x = as.factor(weekdays(Date)), y = items_sold)) +
  geom_boxplot() +
  labs(x = "Weekday", y = "Items Sold", title = "Items Sold on Weekdays") +
  scale_x_discrete()

boxplot_weekday

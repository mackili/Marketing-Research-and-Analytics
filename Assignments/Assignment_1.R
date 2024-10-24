library("tidyverse")
library("ggplot2")
library("readr")
library("lubridate")
library("dplyr")
library("ggplot2")

# Part 1: Data Loading and Inspection -------------------------------------

# 1. Load the Data

customer_sales <- read_csv("customer_sales.csv")
customer_info <- read_csv("customer_info.csv")
product_info <- read_csv("product_info.csv")
supplier_info <- read_csv("supplier_info.csv")

# 2. Inspect the Data

glimpse(customer_sales)
summary(customer_sales)

glimpse(customer_info)
summary(customer_info)

glimpse(product_info)
summary(product_info)

glimpse(supplier_info)
summary(supplier_info)


# Part 2: Data Cleaning and Tidying ---------------------------------------

# 3. Handle Missing Values in customer_sales

# Identify rows with missing Quantity, UnitPrice, or TotalPrice

customer_sales |> 
  filter(is.na(Quantity) | is.na(UnitPrice) | is.na(TotalPrice))

# Identifying rows with available Quantity, UnitPrice, but no TotalPrice

customer_sales |> 
  filter(!is.na(Quantity) & !is.na(UnitPrice) & is.na(TotalPrice))
# There are 5 such values

# Correcting these values
customer_sales <- customer_sales |> 
  mutate(TotalPrice = ifelse(!is.na(Quantity) & !is.na(UnitPrice & is.na(TotalPrice)), 
                             Quantity * UnitPrice, 
                             TotalPrice))

# Filter rows where Quantity and TotalPrice are available, UnitPrice is missing

customer_sales |> 
  filter(!is.na(Quantity) & is.na(UnitPrice) & !is.na(TotalPrice))
# There are no such rows

# Filter rows where Unit Price and TotalPrice are available, Quantity is missing

customer_sales |> 
  filter(is.na(Quantity) & !is.na(UnitPrice) & !is.na(TotalPrice))
# There are no such rows

# The rows with remaining NA values in either of the 3 columns are removed since their values cannot be inferred

customer_sales <- customer_sales |> 
  filter(!(is.na(Quantity) | is.na(UnitPrice) | is.na(TotalPrice)))
# Only 70 rows are kept from the original 80, as 10 have 2 out of 3 crucial values missing


# 4. Standardize Date Formats

# Substituting values which parse_date_time() doesn't recognize
customer_sales$Date <- gsub("Jän", "Jan", customer_sales$Date)

# Parsing the date column
customer_sales$Date <- parse_date_time(
  customer_sales$Date,
  orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY", "dMY"),
  locale = "de_DE" # This helps parse German month abbreviations
)

# Correcting the dates which weren't interpreted for 2023 but should've been
# e.g. "30-03-23" was parsed as 2030-03-23 instead of 2023-03-30

# Splitting the df into 2 - 2023 and non-2023 (wrong) dates
# Filter observations where Date is for 2023 or not available
df_2023 <- customer_sales |> 
  filter(year(Date) == 2023 | is.na(Date))

# Filter observations where Date is NOT for 2023
df_filtered <- customer_sales |> 
  filter(year(Date) != 2023)

# Converting dates from YYYY-MM-DD to YY-MM-DD
df_filtered <- df_filtered |> 
  mutate(Date = format(Date, "%y-%m-%d"))

# Converting all non-2023 values to 2023 (Switching YY with DD)
df_filtered <- df_filtered |> 
  mutate(Date = dmy(Date))

# Converting them back to standard format (YYYY-MM-DD)

# Combining the 2 data frames back together
customer_sales <- bind_rows(df_2023, df_filtered)

# Checking if all dates are for 2023
customer_sales |>
  filter(year(Date) != 2023)

# We can remove the 2 separate parts as we don't need them anymore
rm(df_2023, df_filtered)

# 5. Correct data types

# Converting OrderID, Quantity, UnitPrice and TotalPrice columns from dbl to int

customer_sales$OrderID <- as.integer(customer_sales$OrderID)
customer_sales$Date <- as.Date(customer_sales$Date)
customer_sales$Quantity <- as.integer(customer_sales$Quantity)
customer_sales$UnitPrice <- as.integer(customer_sales$UnitPrice)
customer_sales$TotalPrice <- as.integer(customer_sales$TotalPrice)

# 6. Resolve inconsistencies

# Correct any discrepancies found
# Correcting negative values

customer_sales$UnitPrice <- abs(customer_sales$UnitPrice)
customer_sales$TotalPrice <- abs(customer_sales$TotalPrice)

# Verify that TotalPrice equals Quantity * UnitPrice for all rows

customer_sales$TotalPrice == customer_sales$Quantity * customer_sales$UnitPrice

# customer_sales$TotalPrice <- customer_sales$Quantity * customer_sales$UnitPrice

all(customer_sales$TotalPrice == customer_sales$Quantity * customer_sales$UnitPrice)

# All values correspond with each other


# Part 3: Data Integration with Joins -------------------------------------

# Perform a left_join() of customer_sales with customer_info on CustomerID

customer_sales_enriched <- left_join(customer_sales, customer_info, by = "CustomerID")

# Identify how many rows have missing customer information after the join

customer_sales_enriched |> 
  filter(is.na(Name))
# There are 3 observations with missing customer information

# 8. Inner join

customer_sales_enriched <- inner_join(
  customer_sales_enriched, product_info, by = "ProductID"
  )

# Determine the number of rows in the resulting dataset

glimpse(customer_sales_enriched)

# 67 rows instead of 70 in the previous dataset

# Right join
# Perform a right_join() of product_info with customer_sales on ProductID

product_info_enriched <- right_join(product_info, customer_sales, by = "ProductID")

# Identify products that have no sales records

product_info_enriched |> 
  filter(is.na(OrderID))
# There is no such product

# 10. Full Join

customer_info_enriched <- full_join(customer_info, customer_sales, by = "CustomerID")

# Identify customers who have not made any purchases

customer_info_enriched |> 
  filter(is.na(OrderID))
# 1 customer hasn't made any purchases: Oscar Jones


# Part 4: Data Manipulation with dplyr ------------------------------------

# 11. Create a Profit and an AgeGroup column

customer_sales_enriched <- customer_sales_enriched |> 
  mutate(Profit = TotalPrice * 0.25,
         AgeGroup = case_when(
           Age < 30 ~ "Under 30",
           Age >= 30 & Age <= 40 ~ "30-40",
           Age > 40 ~ "Over 40"
         ))

# 12. Filter and select data

customer_sales_enriched |> 
  filter(Category == "Electronics") |> 
  select(OrderID,
         Date,
         CustomerID,
         Name,
         Product,
         Quantity,
         UnitPrice,
         TotalPrice,
         Profit,
         Region.y, # assuming we want to get info about the customer region, not the sales region
         AgeGroup)
  
# 13. Arrange Data
  
customer_sales_enriched |>
  arrange(desc(TotalPrice))


# Part 5: Data Aggregation ------------------------------------------------

# 14. Group and Summarize

customer_sales_enriched |> 
  group_by(Region.x, Category) |> # Assuming we're looking for sales regions here
  summarise(TotalQuantity = sum(Quantity),
            TotalPrice = sum(TotalPrice),
            AverageProfit = mean(Profit))

# 15. Compute cumulative metrics

customer_sales_enriched |> 
  group_by(Region.x) |> 
  summarise(TotalPrice = sum(TotalPrice))


# Part 6: Data Tidying with tidyr -----------------------------------------

# 16. Reshape the data

# Creating a wide-format table showing TotalPrice for each Product across different Regions
wide <- customer_sales_enriched |>
  group_by(ProductID, Region.x) |>
  summarise(TotalPrice = sum(TotalPrice)) |>
  pivot_wider(names_from = Region.x, values_from = TotalPrice)

# Converting it back to long format
long <- wide |> 
  pivot_longer(
    cols = c("East", "North", "South", "West"),
    names_to = 'Region',
    values_to = 'TotalPrice'
  ) |> 
  filter(!is.na(TotalPrice))

# Part 7: Data Visualization with ggplot2 ---------------------------------

# 17. Create a bar chart

# Plotting total price for each category

customer_sales_enriched |> 
  group_by(Category) |> 
  summarise(TotalPrice = sum(TotalPrice)) |> 
  ggplot(mapping = aes(x = Category)) + 
    geom_bar(aes(weight = TotalPrice))

# 18. Create a time series plot

customer_sales_enriched |> 
  ggplot(mapping = aes(x = Date, y = TotalPrice, color = Region.x)) +
  geom_line() +
  labs(x = "Date", y = "Total Price", color = "Region", title = "Total Price over Time by Region") +
  theme_minimal()

# 19. Create a scatterplot

customer_sales_enriched |> 
  ggplot(mapping = aes(x = UnitPrice, y = Quantity, size = Profit)) +
  geom_point(alpha = 0.6, color = "lightblue") +  # Use alpha to make points semi-transparent for better visibility
  facet_wrap(~ Category) +   # Facet by Category
  labs(x = "Unit Price", y = "Quantity", size = "Profit", title = "Unit Price vs. Quantity by Category") +
  theme_minimal()


# Part 8: Advanced visualization ------------------------------------------

# 20. Faceting and Themes

# Use facet_wrap() to create separate plots for each Region
# Apply a custom theme to enhance visual appeal

customer_sales_enriched |> 
  ggplot(mapping = aes(x = Date, y = TotalPrice)) +
  geom_line() +
  facet_wrap(~ Region.x) +
  theme_minimal()

# 21. Customize Scales and Guides
# Customizing color scales and legends
# Adding informative axis labels and a descriptive title

customer_sales_enriched |> 
  ggplot(mapping = aes(x = Date, y = TotalPrice, color = Category)) +
  geom_line() +
  facet_wrap(~ Region.x) +
  labs(x = "Date", y = "Total Price", color = "Category", title = "Total Price over Time by Category for each Region") +
  theme_minimal()


# Part 9: Additional Analysis with Joins ----------------------------------

# 22. Identify unmatched records
# After joins, identify any OrderID s without matching CustomerID s or ProductID s.

customer_sales_enriched |> 
  filter(is.na(CustomerID) | is.na(ProductID))

# There are 3 rows with missing CustomerID

# Discuss possible reasons:
  # failure to capture customer information upon sale
  # customer purchasing online without registering a personal account
  # data handling mistakes, wrong / missing data entry
# Potential implications:
  # missing information on customers - less information to base decisions on
  # not being able do identify loyal customers - missing opportunity to reward / incentivize
  # losing the capability of personalizing and targeting promotions to the individual customer

# 23. Compare Different Joins

# Join customer_sales and customer_info using left_join

left <- left_join(customer_sales, customer_info, by = 'CustomerID')

# inner_join

inner <- inner_join(customer_sales, customer_info, by = 'CustomerID')

#right_join

right <- right_join(customer_sales, customer_info, by = 'CustomerID')

#full_join

full <- full_join(customer_sales, customer_info, by = 'CustomerID')

# Create 4 datasets and compare the number of rows

nrow(left)
nrow(inner)
nrow(right)
nrow(full)

# Discuss the differences and when each join type is appropriate

  # left: left join takes the available values in the left df and looks for matching values in the right
    # it is appropriate when we want to take the left's values as a basis and look up matching values for it

  # inner: inner join only takes values from the 2 dfs where there are matching pairs
    # it is appropriate when we only want the intersection of the 2 dfs

  # right: the inverse of left: it takes the available values in the right df and looks for matching values in the left
    # it is appropriate when we want to take the right's values as a basis and look up matching values for it in the left

  # full: full join joins the 2 dfs regardless of matching pairs. The resulting df includes all of the 2 df's values
    # it is appropriate when we want to combine the values from 2 tables without leaving out any of the values in the join



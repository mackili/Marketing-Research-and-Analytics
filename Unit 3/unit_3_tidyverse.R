# Load the Tidyverse
install.packages("tidyverse")
library(tidyverse)

# Sample Data
data("mtcars")

### Data Manipulation with dplyr

# filter(): Subset rows based on conditions
mtcars_filtered <- mtcars %>% filter(mpg > 20)

mtcars_filtered <- mtcars %>% filter(mpg > 20 & cyl == 6)
#mtcars[mtcars[,1]>20 & mtcars[,2]==6,]

# select(): Select columns by names or conditions
mtcars_selected <- mtcars %>% select(mpg, cyl, hp)

# mutate(): Create new columns or modify existing ones
mtcars_mutated <- mtcars %>% mutate(hp_per_cyl = hp / cyl)

# arrange(): Sort rows by columns
mtcars_arranged <- mtcars %>% arrange(desc(mpg))

# summarize() and group_by(): Aggregate data and compute summaries
mtcars_summary <- mtcars %>% group_by(cyl) %>% summarize(mean_mpg = mean(mpg), count = n())

### Data Tidying with tidyR

# Example messy data
messy_data <- tibble(
  id = 1:4,
  treatment_a = c(3, 4, 6, 8),
  treatment_b = c(5, 6, 7, 9)
)

# gather(): Convert wide data to long format
tidy_data <- messy_data %>% gather(key = "treatment", value = "response", treatment_a:treatment_b)

# spread(): Convert long data to wide format
wide_data <- tidy_data %>% spread(key = treatment, value = response)

# separate(): Split a single column into multiple columns
separated_data <- tidy_data %>% separate(treatment, into = c("treatment", "type"), sep = "_")

# unite(): Combine multiple columns into one
united_data <- separated_data %>% unite(treatment_type, treatment, type, sep = "_")


# Advanced dplyr Techniques

# Joins: left_join(), inner_join(), right_join(), full_join()
df1 <- tibble(id = 1:3, value1 = c("A", "B", "C"))
df2 <- tibble(id = 2:4, value2 = c("D", "E", "F"))

left_join(df1, df2, by = "id")
inner_join(df1, df2, by = "id")
right_join(df1, df2, by = "id")
full_join(df1, df2, by = "id")

# Window functions: lead(), lag(), cumsum(), etc.
mtcars %>% mutate(lag_mpg = lag(mpg), cumsum_mpg = cumsum(mpg))

### Data Visualization with ggplot2

#stepwise ggplot2

ggplot(data = mtcars)

ggplot(mtcars, mapping = aes(x = hp, y = mpg))

ggplot(mtcars, mapping = aes(x = hp, y = mpg)) +
  # to create a scatterplot
  geom_point()

ggplot(mtcars, mapping = aes(x = hp, y = mpg)) +
  # to create a scatterplot
  geom_point() +
  # to fit and overlay a trendline
  geom_smooth(formula = y ~ x, method = "lm") +
  # to add a title and name the axes
  labs(title = "Scatter plot of MPG vs HP", x = "HP", y = "Miles per Gallon")

ggplot(mtcars, mapping = aes(x = hp, y = mpg, color = factor(cyl))) +
  geom_point() +
  scale_colour_viridis_d()

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "green", "blue")) +
  theme_classic()

# Faceting: facet_wrap(), facet_grid()
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  facet_wrap(~ cyl)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  facet_grid(vs ~ am)




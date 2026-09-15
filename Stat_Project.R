getwd()
install.packages("tidyverse")
library(tidyverse)              

# Data Exploration 

amazon_dataset <- read.csv("amazon_ecommerce_1M.csv")

head(amazon_dataset)

# Data Cleaning

# https://www.youtube.com/watch?v=sV5lwAJ7vnQ 

glimpse(amazon_dataset)

class(amazon_dataset$category)

# Check Duplicate Values

sum(duplicated(amazon_dataset))

# Check Null Values

colSums(is.na(amazon_dataset))

# Check Data Types

str(amazon_dataset)

# Change purchase_data column from character data type to date data type

amazon_dataset$purchase_date <- as.Date(amazon_dataset$purchase_date)

class(amazon_dataset$purchase_date)

range(amazon_dataset$purchase_date)

# Change is_returned col from character data type to boolean data type

unique(amazon_dataset$is_returned)

amazon_dataset$is_returned <- amazon_dataset$is_returned == "True"

class(amazon_dataset$is_returned)

table(amazon_dataset$is_returned)

# Check the IQR and Range for all the numerical statistics

summary(amazon_dataset[ c(
  "price",
  "discount",
  "final_price",
  "rating",
  "review_count",
  "stock",
  "seller_rating",
  "shipping_time_days"
)])

# Check Numerical Variables

sum(amazon_dataset$price < 0, na.rm = TRUE)
sum(amazon_dataset$price == 0, na.rm = TRUE)
sum(amazon_dataset$discount < 0, na.rm = TRUE)
sum(amazon_dataset$discount > 100, na.rm = TRUE)
sum(amazon_dataset$rating < 1, na.rm = TRUE)
sum(amazon_dataset$rating > 5, na.rm = TRUE)
sum(amazon_dataset$review_count < 0, na.rm = TRUE)
sum(amazon_dataset$stock < 0, na.rm = TRUE)
sum(amazon_dataset$shipping_time_days <= 0, na.rm = TRUE)

# Check ID Formating 

sum(!grepl("^U[0-9]+$", amazon_dataset$user_id))
sum(!grepl("^P[0-9]+$", amazon_dataset$product_id))
sum(!grepl("^S[0-9]+$", amazon_dataset$seller_id))

# Check delivery_Status vs is_returned

table(
  amazon_dataset$is_returned,
  amazon_dataset$delivery_status
)
  
# Hypothesis Testing 

# H0; Discount factor has no relationship with the final print
# H1; Discount factor has a significant relationship with the final price

cor.test(
  amazon_dataset$discount,
  amazon_dataset$final_price,
  method = "pearson"
) 

  
# Data Manipulation


# Data Visualization

amazon_dataset %>%
  group_by(category) %>%
  summarise(revenue = sum(final_price)) %>%
  ggplot(aes(x = reorder(category, -revenue), y = revenue)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Revenue by Category", x = NULL, y = "Revenue")


ggplot(amazon_dataset, aes(x = discount, y = rating)) +
  geom_hex(bins = 40) +
  labs(title = "Discount vs. Product Rating")

# Data Analysis 
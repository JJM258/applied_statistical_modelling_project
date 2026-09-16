# Applied Statistical Modelling 
# Module (B105A)

# Exploration of Amazon E-commerce Network 

# Business Questions
## - Does the final price change between different products within various categories?
## - Which products have the most sales revenue and highest ratings, and how does the customer return rate impacts the total sales revenue?
## - What is the correlation between discount factor and final price, and its effect on net sales revenue?

# Data set Source
# https://www.kaggle.com/datasets/sharmajicoder/amazon-e-commerce

getwd()
install.packages("tidyverse")
install.packages("caret")
install.packages("ploty")
library(caret)
library(tidyverse)
library(plotly)

# Data Exploration 

amazon_dataset <- read.csv("amazon_ecommerce_1M.csv")

head(amazon_dataset)

# Data Cleaning

# https://www.youtube.com/watch?v=sV5lwAJ7vnQ - Used this Youtube Video to aid in the Data Cleaning Process

glimpse(amazon_dataset)

## the glimpse function checks the data type of each column/feature in the dataset

class(amazon_dataset$category)

## the class function is if you need to check the data type of a specific column, for example, I checked the data type for the column "category", whcih reutrned the "chr" data type. 

# Check Duplicate Values

sum(duplicated(amazon_dataset))

## the sum() of duplicated() checks how many rows are duplicated in the data set

# Check Null Values

colSums(is.na(amazon_dataset))

## colSum() gives the each column sum of rows. It could have been used in the previous step as well. The is.na() gives the total amount of null values in the data set. 

# Check Data Types

str(amazon_dataset)

## the str() is similar to the glimpise() in which it gives the amount of rows and columns, as well as the data type and some data points. 

# Change purchase_data column from character data type to date data type

amazon_dataset$purchase_date <- as.Date(amazon_dataset$purchase_date)

class(amazon_dataset$purchase_date)

range(amazon_dataset$purchase_date) 

## as.Date() just converts the column from chr to date
## class() to confirm the change of data type
## the range() just shows the lowest and hgihest value, in terms of purchase date it shows the earlist date and lastest date of purchase. 

# Change is_returned col from character data type to boolean data type

unique(amazon_dataset$is_returned)

amazon_dataset$is_returned <- amazon_dataset$is_returned == "True"

class(amazon_dataset$is_returned)

table(amazon_dataset$is_returned)

## the unique() gives all the uniquq data points in that column, for example, in the column "is_returned" the output gave True and False.
## Then the "== "True", changes the data type from chr to boolean/binary.
## class() to check
## table is used to put all the true or false answers into a concise table. 

# Check Numerical Variables

sum(amazon_dataset$price < 0)
sum(amazon_dataset$price == 0)
sum(amazon_dataset$discount < 0)
sum(amazon_dataset$discount > 100)
sum(amazon_dataset$rating < 1)
sum(amazon_dataset$rating > 5)
sum(amazon_dataset$review_count < 0)
sum(amazon_dataset$stock < 0)
sum(amazon_dataset$shipping_time_days <= 0)

## the sum() fuction checks the total amount of invalid data points in the specfic column
## For example the discount percentage cannot be more that 100%

# Check ID Formatting 

sum(!grepl("^U[0-9]+$", amazon_dataset$user_id))
sum(!grepl("^P[0-9]+$", amazon_dataset$product_id))
sum(!grepl("^S[0-9]+$", amazon_dataset$seller_id))

## the grepl() checks the ID format
## for example in for user_id the expected format is U, and the numbers that follow are any anumber between 0 - 9

# Check the delivery_Status and is_returned columns together

table(
  amazon_dataset$is_returned,
  amazon_dataset$delivery_status
)

## this just puts the is_returned and delivery_status column together in a table format

# Data Sampling 

strat_sample <- amazon_dataset %>%
  group_by(category) %>%
  slice_sample(prop = 0.1) %>%
  ungroup()
  
glimpse(strat_sample)

## Used for the price and rating comparison
## Out of a 1,000,000 rows, 100,000 was sampled, based on the stratified sampling technique
## In the stratified sample 10% is taken each equally based on the category

# Descriptive Statistics 

# Checks the IQR and Range for all the numerical statistics

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

# Checks the standard deviation for all the numerical statistics 

sapply(amazon_dataset[ c(
  "price",
  "discount",
  "final_price",
  "rating",
  "review_count",
  "stock",
  "seller_rating",
  "shipping_time_days"
)], sd)

# Checks the variance for all the numerical statistics

sapply(amazon_dataset[ c(
  "price",
  "discount",
  "final_price",
  "rating",
  "review_count",
  "stock",
  "seller_rating",
  "shipping_time_days"
)], var)

# Hypothesis Testing (Inferential Statistics)

# Pearson Correlation (Linear Regression)

# Sources were used in the creation for the linear regression
## https://www.youtube.com/watch?v=-mGXnm0fHtI
## https://www.youtube.com/watch?v=wsi0jg_gH28


## H0; Discount factor has no relationship with the final price
## H1; Discount factor has a significant relationship with the final price

cor.test(
  amazon_dataset$discount,
  amazon_dataset$final_price,
  method = "pearson"
) 

## For this Hypothesis Test Accepted the Alternate Hypothesis, since p-value is less than 0.05 alpha level (significance-value)

# CHI-Squared Test

## H0; Orders are not dependent on the category (this suggest that given a different category the amount of products sell the same.)
## H1; Orders are dependent on the category (this suggest that when changing the category the products sold differs substantially.)

chisq.test(table(amazon_dataset$category))

# the p-value is more than the significance level, therefore there is not sufficient evidence to support the alternate hypothesis, and hence the null hypothesis is selected.

## ANOVA Test

# Sources were used in the creation of this ANOVA graphs
## https://www.youtube.com/watch?v=ITf4vHhyGpc
## https://www.youtube.com/watch?v=CS_BKChyPuc&pp=ygUFYW5vdmE%3D

## H0; There is no difference between the different categories and the final price. 
## H1; There is a difference between the different categories and the final price. 

anova_diagram <- aov(final_price ~ category, data = strat_sample)

plot(anova_diagram, which = 1)

plot(anova_diagram, which = 2)

summary(anova_diagram)

## the aov() tests a one-way ANOVA model, testing final price againist different categories
## the which = 1, produces a graph that shows the homogeneity of variance (compares the variance between all the different categories)
## the which = 2, produces a graph that looks at the normality of residuals (sees whether the categories are normally distributed)

# Logistic Regression

# Sources were used in the creation of this logistic model
## https://www.youtube.com/watch?v=Fcanny-v-7c 
## https://www.youtube.com/watch?v=AQRr2XwfmlY
## https://www.youtube.com/watch?v=AVx7Wc1CQ7Y 
## https://www.youtube.com/watch?v=E7J3M1oYVlc
## https://www.youtube.com/watch?v=SnlOUfT55So&pp=ygUYYW5vdmEgb25lIHdheSB2cyB0d28gd2F5
## https://www.youtube.com/watch?v=C4N3_XJJ-jU
## https://stackoverflow.com/a/73208609

## H0; Final price, discount, rating, and shipping time dates does not influence whether a product is returned.
## H1; Final price, discount, rating, and shipping time dates does influence whether a product is returned.

strat_sample$is_returned_factor <- factor(
  ifelse(strat_sample$is_returned, "Returned", "NotReturned"),
  levels = c("Returned", "NotReturned")
)

set.seed(1)
training <- createDataPartition(strat_sample$is_returned_factor, p = 0.8, list = FALSE)
train_data <- strat_sample[training, ]
test_data <- strat_sample[-training, ]

log_model <- glm(
  is_returned_factor ~ final_price + discount + rating + shipping_time_days, 
  data = train_data,
  family = binomial
)

summary(log_model)

predict_probability <- predict(log_model, newdata = test_data, type = "response")

predict_classes <- factor(
  ifelse(predict_probability < 0.8, "Returned", "NotReturned"),
  levels = c("Returned", "NotReturned")
)

con_matrix <- confusionMatrix(
  data = predict_classes,
  reference = test_data$is_returned_factor,
  positive = "Returned"
)

print(con_matrix)

## The first part of the code creates a factor column from the original column "is_returned", using an if else function, if its TRUE label it as "Returned" and else"Not Returned".
## The set.seed() makes the data reproducible when ran different times. 
## The next function "createDataPartition()" splits the data into 80% training and 20% testing. 
## The glm() function with family = binomial, creates the binomial logistic model based on the training data.
## The summary() function shows the model descriptive statistics 
## predict() with type = "response", gives the predicted probabilities on the test data
## THe confusionMatrix() functions compares predicted probabilities from the test data against actual test values.

# Data Visualization

# Sources used to create graphs
## https://www.youtube.com/watch?v=4KMrOZ_BbKE
## https://www.youtube.com/watch?v=QEjFUQluMWI

# Histogram showing the the Total Revenue for each category

G1 <- amazon_dataset %>%
  group_by(category) %>%
  summarise(revenue = sum(final_price)) %>%
  ggplot(aes(x = reorder(category, - revenue), y = revenue)) +
  geom_col(fill = "coral", width = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Revenue by Category", x = "Category", y = "Revenue")

ggplotly(G1)

## Revenue to given as the sum of all the prices for each category
## It was reordered from highest to lowest (- revenue)
## The colour of the bars was set to "coral" and the width of the bars are "0.5"
## The ploty library was used to make the graphs interactive
## The grpah illustrates that electronics are the highest selling items on the Amazons E-commerce website, while clothing and beauty products are sold the least.
## It can by speculated that because users want to try clothes and beauty products they are relutant to buy these king of products online. 

# Hexbin/Scatter Plot used to show the correlation between discount and rating of the product

G2 <- ggplot(amazon_dataset, aes(x = discount, y = rating)) +
  geom_hex(bins = 25) +
  labs(title = "Correlation between Discount and Product Rating")

ggplotly(G2)

## the x axis is set to the discount factor 
## the y axis is set to the rating for item parameter
## the bins is set to 25, which means that there are 25 by 25 hexagons illustrating the correlation
## the results shows that around the 20% discount mark that product rating was higher (4.0 stars)
## Therefore, Amazon might want to use the 20 - 25% discount as a marketing strategy to sell more Amazon products. 

# Box-and-Whisker Plot that show the final price compared across different categories

G3 <- ggplot(strat_sample, aes(x = category, y = final_price, fill = category)) +
  geom_boxplot() +
  labs(title = "Final Price by Category", x = "Category", y = "Final Price")

ggplotly(G3)

## the x axis is set to category
## the y axis is set to final_price
## goem_boxplot is used to create box plot 
## the graph restates that the electronic products have the highest prices when compared to the other categories


# Data Analysis

# Linear Regression Graph

lin_reg <- lm(discount ~ rating, data = amazon_dataset)

summary(lin_reg)

ggplot(strat_sample, aes(x= rating, y = discount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(mehtod = "lm", col = "blue") +
  labs(title = "Linear Regression for Discount Factor and Rating",
       x = "Rating", y = "Discount") +
  theme_minimal()

## the lm() creates the regression model 
## X axis is set to rating
## Y axis is set to discount factor
## geom_smooth, using the lm to create the graph
## aplha is set to 0.5, which just shows the transparency of the line

## Return Rate of Devices 

amazon_dataset %>%
  group_by(device) %>%
  summarise(return_rate = mean(is_returned))

## calculates the mean rate of return for each device. 
## According to the results, tablets are returned the most at 11.7%, Mobiles at 11.6%, and 11.5% for Computers

## Return Rate based on Category

amazon_dataset %>%
  group_by(category) %>%
  summarise(return_rate = mean(is_returned))

## calculates the mean rate of return for each category.
## According to the results beauty products are returned the most at 11.8%.

## Revenue Per Year

amazon_dataset %>%
  mutate(year = format(purchase_date, "%Y-m")) %>%
  group_by(year) %>%
  summarise(revenue = sum(final_price)) %>%
  ggplot(aes(x = year, y = revenue, group = 1)) + geom_line()

## this shows year-by-year revenue shifts.
## the mutate() function is used to create a variable "year" which got from the purchase_date column, which we changed early from char to date data type ("%Y-m")
## The it is groups by year, and we use final_price summed by to create the price variable. 
## The results indicate that revenue decrease substantially from 2025 to 2026. 
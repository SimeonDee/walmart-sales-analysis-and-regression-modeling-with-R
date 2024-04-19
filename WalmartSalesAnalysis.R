## Clearing the memory data for a fresh run ##
rm(list = ls())

## Installing Libraries (NOTE: remove the comments and comment back once installed) ##
######################################################################################
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("RVAideMemoire")
# install.packages("glmnet") # for Ridge Regression Model

## Loading libraries into current environment ##
################################################
library(ggplot2)
library(dplyr)
#library(qqplotr)
library(tidyverse)
library(corrplot)
#library(RVAideMemoire)
library(stats)
# library(glmnet)

## Loading the Dataset ##
#########################
walmart = read.csv("Walmart.csv", header = TRUE)

View(walmart)
head(walmart)
tail(walmart)
colnames(walmart) # columns
length(walmart$Order.ID) # No of observations

## Data Statistical Summary ##
summary(walmart)

## Checking for null values ##
##############################
View(walmart[!is.na(walmart),])
# Observation: No column with missing Value

## EDA ##
#########

summary(walmart)

# Converting Order.Date and Ship.Date columns into dates
walmart$Order.Date <- as.POSIXct(walmart$Order.Date, format="%d-%m-%Y", tz='UTC')
walmart$Ship.Date <- as.POSIXct(walmart$Ship.Date, format="%d-%m-%Y", tz='UTC')

summary(walmart)


# Sales Distribution
ggplot() + geom_histogram(data = walmart, aes(
  x=Sales), fill="blue", color="black") + title(
    main = 'Sales Distribution')


# Profit Distribution
ggplot() + geom_histogram(data = walmart, aes(
  x=Profit), fill="blue",color="black", bins=50, binwidth = 200) + title(
    main = 'Profit Distribution')

# Quantity Distribution
ggplot() + geom_histogram(data = walmart, aes(x=Profit), fill="blue",color="black",
                          bins=50, binwidth=200) + title(main = 'Profit Distribution')

# Total Annual Sales
sales_by_order_date = walmart %>% select(c(Order.Date, Sales))
sales_by_order_date <- sales_by_order_date[order(sales_by_order_date$Order.Date),]
sales_by_order_date$Order.Year <- as.factor(format(sales_by_order_date$Order.Date, format="%Y"))
View(sales_by_order_date)

ggplot(sales_by_order_date, aes(x=Order.Year, y=Sales, fill=Order.Year),
       title(main = "Average Sales Per Year")) + stat_summary(
         fun=sum, geom="bar")
title(main = 'Average Sales Per Annum')


## Adding Order.Year and Ship.Year Columns for purpose of Analysis ##
walmart$Order.Year <- as.factor(format(walmart$Order.Date, format="%Y"))
walmart$Ship.Year <- as.factor(format(walmart$Ship.Date, format="%Y"))

## Total Sales by Category ##
ggplot(walmart, aes(x=Category, y=Sales, fill=Order.Year), color="rainbow",
       main = "Total Sales Per Product Category") + stat_summary(
         fun=sum, geom="bar") + theme(axis.text.x = element_text(
           face="bold", color="#993333", size=12, angle = 90))
title(main = 'Total Sales Per Product Category')


## Total Quantity Sold by Category ##
ggplot(walmart, aes(x=Category, y=Quantity, fill=Order.Year), color="rainbow",
       title(main = "Total Quantity Sold Per Product Category per Annum")) + stat_summary(
         fun=sum, geom="bar") + theme(axis.text.x = element_text(
           face="bold", color="#993333", size=12, angle = 90))
title(main = 'Total Sales Per Product Category')


## Analyzing Products by Category ##
####################################
category_data = walmart %>% group_by(Category) %>% summarise(
  Total.Sales = sum(Sales), Total.Profit=sum(Profit), Total.Quantity = sum(Quantity))
View(category_data)

## Sales by Category ##
sales_by_category = category_data[order(-category_data$Total.Sales),]
barplot(sales_by_category$Total.Sales, names.arg = sales_by_category$Category,
        main = 'Total Sales per Product Category',
        col = rainbow(length(sales_by_category$Category)),
        xlab = 'Product Category', ylab = 'Total Sales', las=2)


# ggplot(sales_by_category, aes(y=Total.Sales, x=Category), color="rainbow",
#        title(main = "Total Quantity Sold Per Product Category per Annum")) + stat_summary(
#          fun=sum, geom="bar") + theme(axis.text.x = element_text(
#            face="bold", color="#993333", size=12, angle = 90))


## Quantity Sold by Category ##
qty_by_category = category_data[order(-category_data$Total.Quantity),]
barplot(qty_by_category$Total.Quantity, names.arg = qty_by_category$Category,
        main = 'Total Quantity Sold per Product Category',
        col = rainbow(length(qty_by_category$Category)),
        xlab = 'Product Category', ylab = 'Total Quantity', las=2)


## Profit Made on Each Category ##
profit_by_category = category_data[order(-category_data$Total.Profit),]
barplot(profit_by_category$Total.Profit, names.arg = profit_by_category$Category,
        main = 'Total Profit Made per Product Category',
        col = rainbow(length(profit_by_category$Category)),
        xlab = 'Product Category', ylab = 'Total Profit', las=2, offset = 5)


## Analyzing Income on Unique Products ##
#########################################
product_data = walmart %>% group_by(Product.Name) %>% summarise(
  Total.Sales = sum(Sales), Total.Profit=sum(Profit), Total.Quantity = sum(Quantity))
View(product_data)

cat("Total Number of Unique Products: ", as.character(length(unique(product_data$Product.Name))))


## Top 15 Most-Selling Products ##
top_selling_products = product_data[order(-product_data$Total.Sales),][1:15,]
View(select(top_selling_products, c(Product.Name, Total.Sales)))

barplot(top_selling_products$Total.Sales, names.arg = top_selling_products$Product.Name,
        main = 'Top 15 Most-Selling Product',
        col = rainbow(length(top_selling_products$Product.Name)),
        xlab = 'Product Names', ylab = 'Total Sales', las=2,
        offset = 20)

## Top 15 Most-Profitable Products ##
top_profitable_products = product_data[order(-product_data$Total.Profit),][1:15,]
View(select(top_profitable_products, c(Product.Name, Total.Profit)))

barplot(top_profitable_products$Total.Profit, names.arg = paste(substr(
  top_profitable_products$Product.Name, start=1, stop=20), "..."),
        main = 'Top 15 Most-Profitable Products',
        col = rainbow(length(top_profitable_products$Product.Name)),
        xlab = 'Product Names', ylab = 'Total Profit', las=2,
        offset = 20)


## Top 15 Most-Quantity of Products Sold ##
top_quantity_products = product_data[order(-product_data$Total.Quantity),][1:15,]
View(select(top_quantity_products, c(Product.Name, Total.Quantity)))

barplot(top_quantity_products$Total.Quantity, names.arg = paste(substr(
  top_quantity_products$Product.Name, start=1, stop=20), "..."),
  main = 'Top 15 Products with Highest Quantity',
  col = rainbow(length(top_quantity_products$Product.Name)),
  xlab = 'Product Names', ylab = 'Total Quantity', las=2,
  offset = 20)


##  STATE ANALYSIS ##
#####################
state_data = walmart %>% group_by(State) %>% summarise(
  Total.Sales = sum(Sales), Total.Profit=sum(Profit), Total.Quantity = sum(Quantity))
View(state_data)
cat("Total State Coverage: ", as.character(length(state_data$State)), " states")

## Sales made per State ##
# line(state_data$Total.Sales, state_data$State)


## Analyzing the percentage of Sales Coming from Each State ##
#############################################################
percent <-round(100 * state_data$Total.Sales /
                  sum(state_data$Total.Sales), 1)
states_sales_ratio = data.frame(State=state_data$State, Sales.Percentage=percent,
                                Total.Sales=state_data$Total.Sales)
states_sales_ratio = states_sales_ratio[order(-states_sales_ratio$Total.Sales),]
View(states_sales_ratio)

barplot(states_sales_ratio$Sales.Percentage, names.arg = states_sales_ratio$State,
  main = 'Percentage of Total Sales Coming from Each State',
  col = "blue",#rainbow(length(states_sales_ratio$State)),
  xlab = 'States', ylab = 'Percentage of Total Sales', las=2,
  offset = 20)


pie(states_sales_ratio$Sales.Percentage, labels = states_sales_ratio$State, col = rainbow(
  length(states_sales_ratio$State)), las=2, radius=1.0, lwd=2,
    legend.text=percent, main="(%) Sales Coming In Per State")


## CITY ANALYSIS ##
###################
city_data = walmart %>% group_by(City) %>% summarise(
  Total.Sales = sum(Sales), Total.Profit=sum(Profit), Total.Quantity = sum(Quantity))
View(city_data)
cat("Total City Coverage: ", as.character(length(city_data$City)), " cities")

## Analyzing the top 15 Cities with the most Sales ##
percent <-round(100 * city_data$Total.Sales /
                  sum(city_data$Total.Sales), 1)
city_sales_ratio = data.frame(City=city_data$City, Sales.Percentage=percent,
                              Total.Sales=city_data$Total.Sales)
city_sales_ratio = city_sales_ratio[order(-city_sales_ratio$Total.Sales),]
View(city_sales_ratio)

## Top-15 Cities
top_15_perc_by_city = city_sales_ratio[order(-city_sales_ratio$Total.Sales),][1:15,]
View(top_15_perc_by_city)

barplot(top_15_perc_by_city$Total.Sales, names.arg = top_15_perc_by_city$City,
        main = 'Top-15 Cities with Highest Total Sales',
        col = "blue",#rainbow(length(top_15_perc_by_city$City)),
        xlab = 'Cities', ylab = 'Total Sales', las=2,
        offset = 20)

## Least 15 Cities
least_15_by_city = city_sales_ratio[order(city_sales_ratio$Total.Sales),][1:15,]
View(least_15_by_city)

barplot(least_15_by_city$Total.Sales, names.arg = least_15_by_city$City,
        main = 'Top-15 Cities with Highest Total Sales',
        col = "red",#rainbow(length(least_15_by_city$City)),
        xlab = 'Cities', ylab = 'Total Sales', las=2,
        offset = 20)


## CUSTOMER ANALYSIS ##
#######################
customer_data = walmart %>% group_by(Customer.Name) %>% summarise(
  Total.Sales = sum(Sales), Total.Profit=sum(Profit),
  Total.Quantity = sum(Quantity), Patronage.Count = n())
View(customer_data)
cat("Total Number of Unique Customers: ", as.character(length(customer_data$Customer.Name)), " customers")


## Top-15 Patronizing Customers ##
##################################
top_15_patronizing_customers = customer_data[order(-customer_data$Patronage.Count),][1:15,]
View(top_15_patronizing_customers)

barplot(top_15_patronizing_customers$Patronage.Count,
        names.arg = top_15_patronizing_customers$Customer.Name,
        main = 'Top-15 Patronizing Customers',
        col = "blue",#rainbow(length(top_15_patronizing_customers$Customer.Name)),
        xlab = 'Customers', ylab = 'Patronage Count', las=2,
        offset = 20)

## Least 15 Patronizing Customers (who have patronized more than once) ##
########################################################################
least_15_patronizing_customers = customer_data[order(customer_data$Patronage.Count),][1:15,]
View(least_15_patronizing_customers)
cat("Total Number of Customers who only patronized Once: ", as.character(
  length(customer_data[customer_data$Patronage.Count == 1,]$Customer.Name)), " customers")

barplot(least_15_patronizing_customers$Patronage.Count,
        names.arg = least_15_patronizing_customers$Customer.Name,
        main = 'Least-15 Patronizing Customers',
        col = "red",#rainbow(length(top_15_patronizing_customers$Customer.Name)),
        xlab = 'Customers', ylab = 'Patronage Count', las=2)


## Top-15 High Spending Customers ##
###################################
top_15_high_spenders = customer_data[order(-customer_data$Total.Sales),][1:15,]
View(top_15_high_spenders)

barplot(top_15_high_spenders$Total.Sales,
        names.arg = top_15_high_spenders$Customer.Name,
        main = 'Top-15 High Spending Customers',
        col = "blue",#rainbow(length(top_15_high_spenders$Customer.Name)),
        xlab = 'Customers', ylab = 'Total Spending', las=2,
        offset = 20)

## Least-15 Spending Customers ##
#################################
least_15_spenders = customer_data[order(customer_data$Total.Sales),][1:15,]
View(least_15_spenders)

barplot(least_15_spenders$Total.Sales,
        names.arg = least_15_spenders$Customer.Name,
        main = '15 Least Spending Customers',
        col = "red",#rainbow(length(least_15_spenders$Customer.Name)),
        xlab = 'Customers', ylab = 'Total Spending', las=2)


## DATA PREPROCESSING FOR REGRESSION ##
#######################################

## 1. Exploring Outliers ##
##########################

## SALES
ggplot() + geom_boxplot(data = walmart, aes(
  y=Sales), outlier.color = 'red', outlier.shape = 4)

# Violin plot
ggplot() + geom_violin(data = walmart, aes(x=State, y=Sales))


# Descriptive Stats
cat('Min: ', as.character(min(walmart$Sales)), "\n")
cat('Mean: ', as.character(mean(walmart$Sales)), "\n")
cat('Max: ', as.character(max(walmart$Sales)), "\n")
cat('median: ', as.character(median(walmart$Sales)), "\n")
cat('Variance: ', as.character(var(walmart$Sales)), "\n")
cat('Std Dev: ', as.character(sd(walmart$Sales)), "\n")
cat('Median Absolute Deviation: ', as.character(mad(walmart$Sales)), "\n")
cat('Inter-Quantile Range: ', as.character(IQR(walmart$Sales)), "\n")
print(quantile(walmart$Sales))

walmart[walmart$Sales > 500,-c(1:9)]
length(walmart[walmart$Sales > 500,-c(1:9)][,1])
walmart[walmart$Sales > 1000,-c(1:9)]
length(walmart[walmart$Sales > 1000,-c(1:9)][,1])
walmart[walmart$Sales > 1500,-c(1:9)]
length(walmart[walmart$Sales > 1500,-c(1:9)][,1])
walmart[walmart$Sales > 2000,-c(1:9)]
length(walmart[walmart$Sales > 2000,-c(1:9)][,1])
walmart[walmart$Sales > 2500,-c(1:9)]
length(walmart[walmart$Sales > 2500,-c(1:9)][,1])

# Observations and Decision made:
# Sales Price of # 1500 seems fairly, considering the
# small amount of dataset available so as not to loose too much data while
# the treating the outliers

# QUANTITY #
############
ggplot() + geom_boxplot(data = walmart, aes(
  y=Quantity), outlier.color = 'red', outlier.shape = 4)

# Violin plot
ggplot() + geom_violin(data = walmart, aes(x=State, y=Quantity))

# Descriptive Stats
cat('Min: ', as.character(min(walmart$Quantity)), "\n")
cat('Mean: ', as.character(mean(walmart$Quantity)), "\n")
cat('Max: ', as.character(max(walmart$Quantity)), "\n")
cat('median: ', as.character(median(walmart$Quantity)), "\n")
cat('Variance: ', as.character(var(walmart$Quantity)), "\n")
cat('Std Dev: ', as.character(sd(walmart$Quantity)), "\n")
cat('Median Absolute Deviation: ', as.character(mad(walmart$Quantity)), "\n")
cat('Inter-Quantile Range: ', as.character(IQR(walmart$Quantity)), "\n")
print(quantile(walmart$Quantity))

walmart[walmart$Quantity > 8,-c(1:9)]
length(walmart[walmart$Quantity > 8,-c(1:9)][,1])
walmart[walmart$Quantity > 10,-c(1:9)]
length(walmart[walmart$Quantity > 10,-c(1:9)][,1])


# Observations and Decision made:
# Quantity of # 10 seems good, considering the
# small amount of dataset available so as not to loose too much data while
# the treating the outliers


# PROFIT #
############
# walmart = walmart_copy
ggplot() + geom_boxplot(data = walmart, aes(x=State,
  y=Profit), outlier.color = 'red', outlier.shape = 4)

# Violin plot
ggplot() + geom_violin(data = walmart, aes(x=State, y=Profit))

# Descriptive Stats
cat('Min: ', as.character(min(walmart$Profit)), "\n")
cat('Mean: ', as.character(mean(walmart$Profit)), "\n")
cat('Max: ', as.character(max(walmart$Profit)), "\n")
cat('median: ', as.character(median(walmart$Profit)), "\n")
cat('Variance: ', as.character(var(walmart$Profit)), "\n")
cat('Std Dev: ', as.character(sd(walmart$Profit)), "\n")
cat('Median Absolute Deviation: ', as.character(mad(walmart$Profit)), "\n")
cat('Inter-Quantile Range: ', as.character(IQR(walmart$Profit)), "\n")
print(quantile(walmart$Profit))

walmart[walmart$Profit > 500,-c(1:9)]
length(walmart[walmart$Profit > 500,-c(1:9)][,1])
walmart[walmart$Profit < -500,-c(1:9)]
length(walmart[walmart$Profit < -500,-c(1:9)][,1])


# Observations and Decision made:
# Profit of # 10 seems good, considering the
# small amount of dataset available so as not to loose too much data while
# the treating the outliers


###########################
## Treating the Outliers ##
###########################

## SALES ##
# Regulating the outliers by setting all Sales above 1500 to 1500
walmart_copy = walmart
treat_sales_outliers <- function(x){
  if (x > 500){
    return (500.0)
  } else {
    return (x)
  }
}

walmart$Sales = unlist(lapply(walmart$Sales, treat_sales_outliers))

## QUANTITY ##
# Regulating the outliers by setting all Quantities above 10 to 10
treat_qty_outliers <- function(x){
  if (x > 10){
    return (10)
  } else {
    return (x)
  }
}

walmart$Quantity = unlist(lapply(walmart$Quantity, treat_qty_outliers))


## PROFIT ##
# Regulating the outliers by setting all Profits above 500 to 500 and
# and those below -500 to -500
treat_profit_outliers <- function(x){
  if (x > 500){
    return (500.0)
  } else if (x < -500) {
    return (-500.0)
  } else {
    return (x)
  }
}
walmart$Profit = unlist(lapply(walmart$Profit, treat_profit_outliers))


View(walmart)
View(walmart[walmart$Sales > 1300, ])


## 2. CATEGORICAL FEATURES ENCODING ##
#####################################
# Using Ordinal Encoding to reduce complexity
encode_ordinal <- function(x, order=unique(x)){
  x <- as.numeric(factor(x, levels=order, exclude=NULL))
  return (x)
}

sorted_uniq_cities = sort(unique(walmart$City))
sorted_uniq_cities
encoded_city = encode_ordinal(walmart$City, order = sorted_uniq_cities)
encoded_city = unlist(lapply(encoded_city, as.integer))

sorted_uniq_states = sort(unique(walmart$State))
sorted_uniq_states
encoded_state= encode_ordinal(walmart$State, order = sorted_uniq_states)
encoded_state = unlist(lapply(encoded_state, as.integer))
encoded_state

sorted_uniq_category = sort(unique(walmart$Category))
sorted_uniq_category
encoded_category= encode_ordinal(walmart$Category, order = sorted_uniq_category)
encoded_category = unlist(lapply(encoded_category, as.integer))
encoded_category

sorted_uniq_products = sort(unique(walmart$Product.Name))
sorted_uniq_products
encoded_products= encode_ordinal(walmart$Product.Name, order = sorted_uniq_products)
encoded_products = unlist(lapply(encoded_products, as.integer))
encoded_products

# dropping the Country columns as it contains only a single value for all observations
walmart = walmart %>% select(-c("Country"))
walmart$State = encoded_state
walmart$Category = encoded_category
walmart$Product.Name = encoded_products
walmart$City = encoded_city

View(walmart)

# Accessing Relationships among variables
plot(walmart[,5:11])

## Dataset Segmentation ##
#########################
training = slice_sample(walmart, prop = 0.7, replace = FALSE)
test_set = slice_sample(walmart, prop = 0.3, replace = FALSE)

View(training)
View(test_set)

## Fitting the linear regression model ##
#########################################

# LINEAR MODELS #

# Using all relevant variables to predict Sales
lm_model1 = lm(Sales ~ Quantity + Profit + City + State + Category + Product.Name,
           data = training)
summary(lm_model1)
plot(lm_model1)

# Using only Quantity and Profit variables to predict Sales
lm_model2 = lm(Sales ~ Quantity + Profit,
               data = training)
summary(lm_model2)
plot(lm_model2)


## Making Predictions and Evaluations ##
#######################################
X_test1 = test_set[, c("Quantity", "Profit", "City", "State", "Category", "Product.Name")]
X_test2 = test_set[, c("Quantity", "Profit")]
y_test = test_set[, c("Sales")]

View(X_test1)
View(X_test2)
View(y_test)

predicted_sales_lm1 = predict(lm_model1, X_test1)
predicted_sales_lm2 = predict(lm_model2, X_test2)

View(predicted_sales_lm1)
View(predicted_sales_lm2)

## EVALUATIONS ##
#################

## LINEAR MODEL 1 (ALL FEATURES) ##
mae1 = mean(y_test - predicted_sales_lm1)
mse1 = mean( (y_test - predicted_sales_lm1) ^ 2 )
rmse1 = sqrt(mean( (y_test - predicted_sales_lm1) ^ 2 ))

# R-Squared

sst = sum((y_test - mean(y_test)) ^ 2)
sse = sum((predicted_sales_lm1 - y_test) ^ 2)
r_squared1 = 1 - sse / sst

cat("LINEAR MODEL 1 (ALL FEATURES)")
cat("MAE: ", as.character(mae1))
cat("MSE: ", as.character(mse1))
cat("RMSE: ", as.character(rmse1))
cat("R-SQUARED: ", as.character(r_squared1))


## LINEAR MODEL 2 (Quantity & Profit Features only) ##
mae2 = mean(y_test - predicted_sales_lm2)
mse2 = mean( (y_test - predicted_sales_lm2) ^ 2 )
rmse2 = sqrt(mean( (y_test - predicted_sales_lm2) ^ 2 ))

# R-Squared
sst = sum((y_test - mean(y_test)) ^ 2)
sse = sum((predicted_sales_lm2 - y_test) ^ 2)
r_squared2 = 1 - sse / sst

cat("LINEAR MODEL 2 (Quantity & Profit Features only)")
cat("MAE: ", as.character(mae2))
cat("MSE: ", as.character(mse2))
cat("RMSE: ", as.character(rmse2))
cat("R-SQUARED: ", as.character(r_squared2))

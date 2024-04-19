# walmart-sales-anaylysis-and-regression-modeling-with-R
In this project, I used R-programming language to analyse Walmart Sales Dataset, preprocessed the dataset to built a linear regression model to predict sales and evaluated the model using  MAE, MSE, RMSE and R-Squared metrics.

## Tools used:
- `R Programming Language`

## Analysis
### Installed Packages/Libraries (NOTE: remove the comments and comment back once installed). Packages used include:
  - ggplot2
  - dplyr
  - qqplotr
  - tidyverse
  - corrplot
  - RVAideMemoire
  - stats
  - glmnet (for Ridge Regression Model)

- Loaded the Dataset

### Exploratory Data Analysis (EDA)
- Statistical Summary
- Checked for null values (missing values)
- Converted Order.Date and Ship.Date columns into dates
- Assessed Sales, Profit and Quantity Distribution (with ggplot)

### Data Manipulation/Wrangling
- Computed Total Sales per annum
- Added Order.Year and Ship.Year Columns for purpose of Analysis

### Sales Analysis
- Analyzed Total Sales by Category
- Analyzed Total Quantity Sold by Category
- Analyzed Products by Category
- Sales made per Category
- Quantity Sold per Category
- Profit Made on Each Category
- Analyzed Income made on each Unique Product
- Top 15 Most-Selling Products
- Top 15 Most-Profitable Products
- Top 15 Products Most Sold (Quantity)

#### Demographic Analysis
- Sales made per State
- Percentage of Sales Coming from each State
- Top 15 Cities with the most Sales
- Percentage Sales for Top-15 Cities
- Least 15 Cities with low sales (possible use targeted promotions and adverts)

### Customer Analysis
- Top-15 Patronizing Customers
- Least 15 Patronizing Customers (who have patronized more than once)
- Top-15 High Spending Customers
- Bottom-15 Least spending Customers


## DATA PREPROCESSING FOR LINEAR REGRESSION MODELING
- Outlier assessment on Sales, quantity, profit 
- Descriptive Stats (min, max, mean, median, mode, std, var, MAD (mean absolute deviation), interquartile range)
- Treated the outliers

### Categorical Variables encoding
- Encoded categorical variables "cities", "state", "category", "products"
- Dropped "country" column (as it contains only a single value for all observations)
- Explored the Relationships among the variables

### Data Segmentation
- 70% for training and 30% for testing (7:3)

### Fitting the Linear Regression model
- Using all relevant variables to predict Sales
```code
lm_model1 = lm(Sales ~ Quantity + Profit + City + State + Category + Product.Name, data = training)
summary(lm_model1)
plot(lm_model1)
```

- Using only Quantity and Profit variables to predict Sales
```code
lm_model2 = lm(Sales ~ Quantity + Profit, data = training)
summary(lm_model2)
plot(lm_model2)
```

### Predictions and Evaluations
- Predictions
```code
predicted_sales_lm1 = predict(lm_model1, X_test1)
predicted_sales_lm2 = predict(lm_model2, X_test2)

View(predicted_sales_lm1)
View(predicted_sales_lm2)
```

- Evaluations
```code
## LINEAR MODEL 1 (ALL FEATURES) ##
mae1 = mean(y_test - predicted_sales_lm1)
mse1 = mean( (y_test - predicted_sales_lm1) ^ 2 )
rmse1 = sqrt(mean( (y_test - predicted_sales_lm1) ^ 2 ))

# R-Squared
sst = sum((y_test - mean(y_test)) ^ 2)
sse = sum((predicted_sales_lm1 - y_test) ^ 2)
r_squared1 = 1 - sse / sst


## LINEAR MODEL 2 (Quantity & Profit Features only) ##
mae2 = mean(y_test - predicted_sales_lm2)
mse2 = mean( (y_test - predicted_sales_lm2) ^ 2 )
rmse2 = sqrt(mean( (y_test - predicted_sales_lm2) ^ 2 ))

# R-Squared
sst = sum((y_test - mean(y_test)) ^ 2)
sse = sum((predicted_sales_lm2 - y_test) ^ 2)
r_squared2 = 1 - sse / sst
```




# Clear global environment
rm(list = ls())
# Import required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(dummies)

# Set working directory
setwd("...")
# Upload raw data
sales_data <- read.csv("sales_data1.csv")
# Observe the structure of the dataset
str(sales_data)
# Also the summary
summary(sales_data)

# Explore the seasonality of sales in cases by month
monthly_sales <- sales_data[c("Sales.in.Cases", "Month")] %>% group_by(Month) %>% summarise_at(vars(Sales.in.Cases), sum)
ggplot(data = monthly_sales, aes(x = factor(Month, level = c("January", "February", "March", "April", "May", "June", "July", 
                                                          "August", "September", "October", "November", "December")), 
                                 y = Sales.in.Cases, group = 1)) + geom_line() + geom_point() + ggtitle("Monthly seasonality of sales") + xlab("Month") + ylab("Sales in cases") + scale_y_continuous(labels = comma) 

# Plot the data
ggplot(data = sales_data, aes(x = PeriodNumber, y = Sales.in.Cases, group = 1)) + geom_line() + geom_point() + ggtitle("Evolution of sales in cases") + xlab("Period number") + ylab("Sales in cases") + scale_y_continuous(labels = comma) 
ggplot(data = sales_data, aes(x = PeriodNumber, y = Consumer.Promotion, group = 1)) + geom_line() + geom_point() + ggtitle("Consumer Promotion") + xlab("Period number") + ylab("Consumer promotion ($)") + scale_y_continuous(labels = comma) 
ggplot(data = sales_data, aes(x = PeriodNumber, y = Trade.Promotion, group = 1)) + geom_line() + geom_point() + ggtitle("Trade Promotion") + xlab("Period number") + ylab("Trade promotion ($)") + scale_y_continuous(labels = comma) 

# Create modeling dataframe
sales_data2 <- sales_data[2:nrow(sales_data),]
row.names(sales_data2) <- 1:nrow(sales_data2)
sales_data2$Tlag1 <- sales_data$Trade.Promotion[-nrow(sales_data)]
sales_data2$Clag1 <- sales_data$Consumer.Promotion[-nrow(sales_data)]
sales_data2$Trend <- as.numeric(row.names(sales_data2))
# Create dummy variables for each month
month_dummy <- data.frame(dummy(sales_data2$Month))
# Keep K-1 dummy variables and bind it with sales data
model_data <- cbind(sales_data2, subset(month_dummy, select = -c(Month.December)))
# Observe data structure
str(model_data)

# Run a linear regression model
linear_model <- lm(formula = Sales.in.Cases ~ Tlag1 + Trade.Promotion + Clag1 
                   + Consumer.Promotion + Month.January + Month.February + Month.March 
                   + Month.April + Month.May + Month.June + Month.July + Month.August 
                   + Month.September + Month.October + Month.November + Trend, 
                   data = model_data)
# Review the linear model results
summary(linear_model)
# Tlag1 and Trend are not statistically significant, therefore we can run another model excluding them
# Run a second iteration of the linear regression model
linear_model_2 <- lm(formula = Sales.in.Cases ~ Trade.Promotion + Clag1 + Consumer.Promotion 
                     + Month.January + Month.February + Month.March + Month.April + Month.May 
                     + Month.June + Month.July + Month.August + Month.September + Month.October 
                     + Month.November, data = model_data)
# Review the linear model results
lm2_summary <- summary(linear_model_2)
lm2_summary

# Create a dataframe with the estimated coefficients
coeffs <- data.frame(Estimate = lm2_summary$coefficients[,1])
coeffs <- cbind(Variable = rownames(coeffs), coeffs)
row.names(coeffs) <- 1:nrow(coeffs)
coeffs

# Simulate the total sales for the case where the 
# brand manager offers the consumer promotion and
# the case where no promotion is offered in January
price_case <- 2.2*24
# Step 1: calculate the consumer promotion investment
coupons <- 2000000*4 # 2,000,000 coupons of $1 sent for each Sunday in January
redeemed_coupons <- 0.1*coupons # On average 10% are redeemed
dist_costs <- (coupons/1000)*20 
proc_costs <- 0.1*redeemed_coupons
total_costs <- redeemed_coupons + dist_costs + proc_costs # Consumer promotion investment

# Step 2: calculate both scenarios
# No consumer promotion was offered
jan_vars_nopromo <- c(1, sales_data$Trade.Promotion[sales_data$PeriodNum == 36], sales_data$Consumer.Promotion[sales_data$PeriodNum == 47], 0, 1, rep(c(0), times = 10))
jan_nonpromo_sales <- as.numeric(coeffs$Estimate%*%jan_vars_nopromo)  
feb_vars_nopromo <- c(1, sales_data$Trade.Promotion[sales_data$PeriodNum == 37], rep(c(0), times = 3), 1, rep(c(0), times = 9))
feb_nonpromo_sales <- as.numeric(coeffs$Estimate%*%feb_vars_nopromo)  
total_nonpromo_sales <- jan_nonpromo_sales + feb_nonpromo_sales
nonpromo_profits <- price_case * total_nonpromo_sales

# Consumer promotion was offered
jan_vars_promo <- c(1, sales_data$Trade.Promotion[sales_data$PeriodNum == 36], sales_data$Consumer.Promotion[sales_data$PeriodNum == 47], redeemed_coupons, 1, rep(c(0), times = 10))
jan_promo_sales <- as.numeric(coeffs$Estimate%*%jan_vars_promo)
feb_vars_promo <- c(1, sales_data$Trade.Promotion[sales_data$PeriodNum == 37], redeemed_coupons, rep(c(0), times = 2), 1, rep(c(0), times = 9))
feb_promo_sales <- as.numeric(coeffs$Estimate%*%feb_vars_promo)
total_promo_sales <- jan_promo_sales + feb_promo_sales
promo_gross_profits <- price_case * total_promo_sales
promo_net_profits <- promo_gross_profits - total_costs

# Profits difference between both scenarios
profits_diff <- promo_net_profits - nonpromo_profits
profits_diff
################################################################################################################
# Train/Test approach
library(caret)
# Set seed for reproducibility
set.seed(123) 
# Perform k-fold cross-validation to train the model on k=5 different subsets of the data 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
linear_model <- train(Sales.in.Cases ~ Tlag1 + Trade.Promotion + Clag1 + Consumer.Promotion + Month.January + Month.February + Month.March + Month.April + Month.May + Month.June + Month.July + Month.August + Month.September + Month.October + Month.November + Trend,
               data = model_data, method = "lm",
               trControl = train.control)
# Summarize the results
print(linear_model)
summary(linear_model)

# Train the second model
linear_model_2 <- train(Sales.in.Cases ~ Trade.Promotion + Clag1 + Consumer.Promotion 
                        + Month.January + Month.February + Month.March + Month.April 
                        + Month.May + Month.June + Month.July + Month.August + Month.September 
                        + Month.October + Month.November,
                      data = model_data, method = "lm",
                      trControl = train.control)
# Summarize the results
print(linear_model_2)
summary(linear_model_2)

# Random forest
library(randomForest)
rfFit <- train(Sales.in.Cases ~ Trade.Promotion + Clag1 + Consumer.Promotion + Month.January + Month.February + Month.March + Month.April + Month.May + Month.June + Month.July + Month.August + Month.September + Month.October + Month.November, 
               data = model_data,
         method = "rf",
         trControl = train.control,
         tuneLength = 5,
         metric = "Rsquared")
print(rfFit)
# Variable Importance
plot(varImp(rfFit)) 
rfFit$pred

##############################################################################################################################################
# Predict the last year's sales 
# Train model with the data of the first 3 years and test it with last year data to then plot Actual versuss Predicted Sales
train_rows <- nrow(model_data) - 12 # ~75%/25%
test_rows <- train_rows + 1
train_df <- model_data[1:train_rows, ] 
test_df  <- model_data[test_rows:nrow(model_data), ] 
# Fit linear model
linear_model_train <- lm(formula = Sales.in.Cases ~ Trade.Promotion + Clag1 + Consumer.Promotion 
                         + Month.January + Month.February + Month.March + Month.April + Month.May 
                         + Month.June + Month.July + Month.August + Month.September + Month.October 
                         + Month.November, data = train_df)

summary(linear_model_train)
# Predict sales
predictions <- predict(linear_model_train,test_df)
# Create dataframe to create a plot afterwards
actual_predicted <- data.frame(cbind(Period = c(1:12), ActualSales = test_df$Sales.in.Cases, PredictedSales = predictions))
# Plot actual and predicted sales
ggplot() + 
  geom_line(data = actual_predicted, aes(x = Period, y = ActualSales), color = "blue") + 
  geom_line(data = actual_predicted, aes(x = Period, y = PredictedSales), color = "red") +
  xlab('Period') +  ylab('Sales in Cases') + ggtitle("Actual Sales vs Predicted Sales") + scale_y_continuous(labels = comma) + scale_x_continuous(breaks = c(1:12)) 

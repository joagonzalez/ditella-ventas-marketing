'''
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    Conjoint Analysis
            Example: launch of a new cellphone
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A company is thinking of launching a new cellphone to the
market. It decided that the attributes of main interest 
are: PRICE, COLOR, SCREEN SIZE and CAMERAS AMOUNT.
Suppose that the company surveyed a group of 8 people to 
obtain their preferences for 20 different potential 
cellphone profiles to launch. Their preferences range 
from 0 to 10.

1) How does their utility change when attribute levels 
vary?

2) Which is the most relevant feature for the surveyed 
group?

3) If the company shows two more options:
   a) a color cellphone with standard screen size with
      two cameras at a low price, or
   b) a black cellphone with standard screen size with
      three cameras at a high price.
   which one will be prefered the most?

4) Assume that the low price is US$500, the medium 
price is US$1000 and the high price is US$1500. 
What would be the willingness to pay of changing
from one camera to three cameras?

'''
# Clear the global environment
rm(list = ls())

# Set the path where the raw files are at
setwd("/home/jgonzalez/dev/ventas-marketing/Conjoint/")

# Upload the cellphone's preference data
Cell_data <- read.csv("Data_Rating_Based.csv", sep = ",")

# Run a linear regression model of the preference as a function of the product's attributes levels
linear_model <- lm(formula = Preference ~ Low + Medium + Black + White + Standard + One + Two, data = Cell_data)
# Linear regression model results
lm_summary <- summary(linear_model)
# Show model results
lm_summary

# Question 1:
# To be able to plot the part-worth utilities, we will create a table with the relevant information
# We will keep the estimated coefficients (part-worth utilities)
coeffs <- as.vector(lm_summary$coefficients[,1])
# To graph the part-worth utilities we must add the reference levels for each attribute (which have a zero value)
pw_ut <- c(coeffs[1], coeffs[2:3], 0, coeffs[4:5], 0, coeffs[6], 0, coeffs[7:8], 0)
# Define the levels
levels <- c("Intercept", "Low", "Medium", "High", "Black", "White", "Color", "Standard", "Plus", "One", "Two", "Three")
# Define the attributes
attributes <- c("Intercept", rep(c("Price"), times = 3), rep(c("Color"), times = 3) , 
                rep(c("ScreenSize"), times = 2), rep(c("CamerasAmount"), times = 3))
# Create the part-worth utilities dataframe
pw_ut_df <- data.frame(Variable = attributes, Levels = levels, pw_ut)

# Import libraries to plot the part-worth utilities
library(ggplot2)
library(gridExtra)
# Get the subset of the dataframe for each attribue
cellPrice <- subset(pw_ut_df, pw_ut_df$Variable == "Price")
cellColor <- subset(pw_ut_df, pw_ut_df$Variable == "Color")
cellScreenSize <- subset(pw_ut_df, pw_ut_df$Variable == "ScreenSize")
cellCameras <- subset(pw_ut_df, pw_ut_df$Variable == "CamerasAmount")
# Define the plots for each attribute
gg1 <- ggplot(data = cellPrice, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Price") + ylab("Part-Worth Utilities")
gg2 <- ggplot(data = cellColor, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Color") + ylab("Part-Worth Utilities")
gg3 <- ggplot(data = cellScreenSize, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Screen size") + ylab("Part-Worth Utilities")
gg4 <- ggplot(data = cellCameras, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Cameras amount") + ylab("Part-Worth Utilities")
# Plot them together
grid.arrange(gg1, gg2, gg3, gg4)

# Question 2:
# Attributes importante calculation
# First, we must calculate the range of the part-worth utilities for each attribute and the total
cellPrice_range <- max(cellPrice$pw_ut) - min(cellPrice$pw_ut)
cellColor_range <- max(cellColor$pw_ut) - min(cellColor$pw_ut)
cellScreenSize_range <- max(cellScreenSize$pw_ut) - min(cellScreenSize$pw_ut)
cellCameras_range <- max(cellCameras$pw_ut) - min(cellCameras$pw_ut)
total_range <- sum(cellPrice_range + cellColor_range + cellScreenSize_range + cellCameras_range)
# Then, we can calculate the relative importance for each attribute
cellPrice_importance <- cellPrice_range/total_range
cellColor_importance <- cellColor_range/total_range
cellScreenSize_importance <- cellScreenSize_range/total_range
cellCameras_importance <- cellCameras_range/total_range
relative_importance <- data.frame(Attribute = c("Price", "Color", "Screen size", "Cameras amount"),
                                  Importance = c(cellPrice_importance, cellColor_importance, cellScreenSize_importance, cellCameras_importance))
# Finally, we can visualize the relative importances
ggplot(relative_importance, aes(x = Attribute, y = Importance)) + geom_bar(stat = "identity") + ggtitle("Relative importance of attributes")
# The most important attribute is Price (followed by the cameras amount, screen size and color).

# Question 3:
# The utility of a product is the sum of the utilities of its attributes levels
utility_a <- sum(pw_ut_df$pw_ut[pw_ut_df$Levels == "Intercept"] + pw_ut_df$pw_ut[pw_ut_df$Levels == "Low"] 
                 + pw_ut_df$pw_ut[pw_ut_df$Levels == "Color"] + pw_ut_df$pw_ut[pw_ut_df$Levels == "Standard"] 
                 + pw_ut_df$pw_ut[pw_ut_df$Levels == "Two"])

utility_b <-  sum(pw_ut_df$pw_ut[pw_ut_df$Levels == "Intercept"] + pw_ut_df$pw_ut[pw_ut_df$Levels == "High"] 
                  + pw_ut_df$pw_ut[pw_ut_df$Levels == "Black"] + pw_ut_df$pw_ut[pw_ut_df$Levels == "Standard"] 
                  + pw_ut_df$pw_ut[pw_ut_df$Levels == "Three"])

paste("The utility of option (a) is:", utility_a)
paste("The utility of option (b) is:", utility_b)
# Therefore, as the utility of the first option is greater than the second one, the surveyed group will prefer option (a)

# Question 4:
lowPrice <- 500
mediumPrice <- 1000
highPrice <- 1500
# The relative utility of changing a cellphone with one camera to a cellphone with three cameras is of
camera_ut_range <- cellCameras$pw_ut[cellCameras$Levels == "Three"] - cellCameras$pw_ut[cellCameras$Levels == "One"] # 2.088938
# The price range is
price_range <- highPrice - lowPrice
# The range of the relative utility for price is
price_ut_range <- cellPrice$pw_ut[cellPrice$Levels == "Low"] - cellPrice$pw_ut[cellPrice$Levels == "High"] # 5.936739
# The monetary value of one unit of utility is
mv <- price_range / price_ut_range
# Therefore, as changing from one camera to three provides a utility of 2.088938, the willingness to pay for it will be
mv*camera_ut_range # 351.8662 dollars


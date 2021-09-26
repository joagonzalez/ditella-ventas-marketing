# Choice-Based Conjoint Analysis
# Digital Cameras example 
# mixed logit model of digital cameras 
# wide format

# Clear the list of objects
rm(list=ls())					

# Set working directory
setwd("...")

#install.packages("openxlsx")
library(openxlsx)

# Read in the data
##complete data file loaded in a matrix
#install.packages("data.table")
library(data.table)

#install.packages("mlogit")
library(mlogit)

#format attributes for mlogit
#data have 5 attributes: Price, Resolution, Battery Life, Optical Zoom, Camera Size
#Price ($500, $400, $300, and $200), 
#Resolution (2, 3, 4, and 5 Megapixels), 
#Battery Life (150, 300, 450, and 600 pictures),
#Optical Zoom (2x, 3x, 4x, and 5x), 
#Camera Size (SLR, Medium, Pocket, and Ultra Compact).

# Upload dataset
data = fread(file="dataC.txt", sep="\t", header=FALSE, data.table=F)		
# Change column names
colnames(data)=c("id","question","alt","chosen_alt","choice","Price","Resolution","BL","OZ","CS")
# Change attribute levels
data$Price=ifelse(data$Price==1,500,ifelse(data$Price==2,400,ifelse(data$Price==3,300,200)))
data$Resolution=ifelse(data$Resolution==1,"2MP",ifelse(data$Resolution==2,"3MP",ifelse(data$Resolution==3,"4MP","5MP")))
data$BL=ifelse(data$BL==1,150,ifelse(data$BL==2,300,ifelse(data$BL==3,450,600)))
data$OZ=ifelse(data$OZ==1,"2X",ifelse(data$OZ==2,"3X",ifelse(data$OZ==3,"4X","5X")))
data$CS=ifelse(data$CS==1,"SLR",ifelse(data$CS==2,"Medium",ifelse(data$CS==3,"Pocket","UltraCompact")))

data$chosen_alt2=data$chosen_alt # to eliminate later
# Observe the structure of the data
str(data)
# Set attributes as factors
data[,6:10] = as.data.frame(sapply(data[,6:10], function(x) as.factor(x)))
# Sort Camera Size levels
data$CS <- factor(data$CS , levels = c("SLR", "Medium", "Pocket","UltraCompact"))
str(data)
# Create dataframe for modeling
data_tmp=as.data.frame(model.matrix(chosen_alt2 ~. -1, data = data))
colnames(data_tmp)
# Eliminate Price500
data=data_tmp[,-9] 
# Eliminate choice
data=data[,c(-5)] 

# Expand data in wide format
data_wide <- reshape(data=data,idvar=c("id","question"),
                     v.names = c("Price200","Price300","Price400","Resolution3MP","Resolution4MP","Resolution5MP","BL300","BL450","BL600","OZ3X","OZ4X","OZ5X","CSMedium","CSPocket","CSUltraCompact"),
                     timevar = "alt",
                     direction="wide")

# Format data in mlogit format
data.logit <- mlogit.data(data_wide, shape="wide", varying=4:63, sep="." ,choice="chosen_alt", id = "id")

# Fit homogeneous mlogit
plainlogit <- mlogit(chosen_alt ~ 0 + Price400 + Price300 + Price200 + Resolution3MP 
                     + Resolution4MP + Resolution5MP + BL300 + BL450 + BL600 + OZ3X 
                     + OZ4X + OZ5X + CSMedium + CSPocket + CSUltraCompact, 
                     data = data.logit)
# Show homogeneous mlogit results
summary(plainlogit)

# Create a dataframe with the estimated part worths
coeffs <- data.frame(plainlogit$coefficients)
coeffs <- `colnames<-`(coeffs, "Part-worth")
coeffs <- cbind(Attribute = rownames(coeffs), coeffs)
rownames(coeffs) <- 1:nrow(coeffs)
print(coeffs)
# Define attributes
attributes <- c(rep(c("Price"), times = 4), rep(c("Resolution"), times = 4), 
                rep(c("Battery Life"), times = 4), rep(c("Optical Zoom"), times = 4),
                rep(c("Camera Size"), times = 4))
# Define levels
levels <- c("$500", "$400", "$300", "$200", "2MP", "3MP", "4MP", "5MP", 
            "150", "300", "450", "600", "2x", "3x", "4x", "5x", 
            "SRL", "Medium", "Pocket", "UltraCompact")
# Keep coefficients and include zeros for base levels
pw_ut <- c(0, coeffs$`Part-worth`[1:3], 0, coeffs$`Part-worth`[4:6], 0, coeffs$`Part-worth`[7:9], 
           0, coeffs$`Part-worth`[10:12], 0, coeffs$`Part-worth`[13:15])
# Create final part-worth utilities dataframe
pw_ut_df <- data.frame(Variable = attributes, Levels = levels, pw_ut)

# Import libraries to plot the part-worth utilities
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra)
library(gridExtra)

# Get the subset of the dataframe for each attribue
Price <- subset(pw_ut_df, pw_ut_df$Variable == "Price")
Resolution <- subset(pw_ut_df, pw_ut_df$Variable == "Resolution")
BatteryLife <- subset(pw_ut_df, pw_ut_df$Variable == "Battery Life")
OpticalZoom <- subset(pw_ut_df, pw_ut_df$Variable == "Optical Zoom")
CameraSize <- subset(pw_ut_df, pw_ut_df$Variable == "Camera Size")

# Define the plots for each attribute
gg1 <- ggplot(data = Price, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Price") + ylab("Part-Worth Utilities")
gg2 <- ggplot(data = Resolution, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Resolution") + ylab("Part-Worth Utilities")
gg3 <- ggplot(data = BatteryLife, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Battery Life") + ylab("Part-Worth Utilities")
gg4 <- ggplot(data = OpticalZoom, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Optical Zoom") + ylab("Part-Worth Utilities")
gg5 <- ggplot(data = CameraSize, aes(x = Levels, y = pw_ut, group = 1)) + geom_line() + geom_point() + ggtitle("Camera Size") + ylab("Part-Worth Utilities")

# You can plot them together
grid.arrange(gg1, gg2, gg3, gg4, gg5)
# Or separately
#gg1
#gg2
#gg3
#gg4
#gg5

# Attributes importante calculation:
# First, we must calculate the range of the part-worth utilities for each attribute and the total
Price_range <- max(Price$pw_ut) - min(Price$pw_ut)
Resolution_range <- max(Resolution$pw_ut) - min(Resolution$pw_ut)
BatteryLife_range <- max(BatteryLife$pw_ut) - min(BatteryLife$pw_ut)
OpticalZoom_range <- max(OpticalZoom$pw_ut) - min(OpticalZoom$pw_ut)
CameraSize_range <- max(CameraSize$pw_ut) - min(CameraSize$pw_ut)
total_range <- sum(Price_range + Resolution_range + BatteryLife_range + OpticalZoom_range + CameraSize_range)

# Then, we can calculate the relative importance for each attribute
Price_importance <- Price_range/total_range
Resolution_importance <- Resolution_range/total_range
BatteryLife_importance <- BatteryLife_range/total_range
OpticalZoom_importance <- OpticalZoom_range/total_range
CameraSize_importance <- CameraSize_range/total_range
relative_importance <- data.frame(Attribute = unique(attributes),
                                  Importance = c(Price_importance, Resolution_importance, BatteryLife_importance, OpticalZoom_importance, CameraSize_importance))

# Finally, we can visualize these relative importances
ggplot(relative_importance, aes(x = Attribute, y = Importance)) + geom_bar(stat = "identity") + ggtitle("Relative importance of attributes")

# Calculate the WTP
# The utility of geting a camera with 5 megapixels relative to getting one with 2MP is:
Resolution_range # 1.484973
# The utility of paying a low price relative to a high price is:
Price_range # 2.162663
# The price range is:
priceRange <- 500 - 200 # 300
# The monetary value of one unit of utility is:
mv <- priceRange / Price_range # 138.7179
# Hence, the WTP is:
Resolution_range*mv # 205.9923 

# Including heterogeneity of consumers
# Assume that random coefficients are normally distributed across the respondents
print("mixed logit without correlation")
# Fit mixed logit model without correlations
mixedlogit <- mlogit(chosen_alt ~ 0 + Price400 + Price300 + Price200 + Resolution3MP 
                     + Resolution4MP + Resolution5MP + BL300 + BL450 + BL600 + OZ3X 
                     + OZ4X + OZ5X + CSMedium + CSPocket + CSUltraCompact, 
                     rpar = c(Price400 ="n", Price300 ="n", Price200 ="n", Resolution3MP ="n",
                              Resolution4MP ="n", Resolution5MP ="n", BL300 ="n", BL450 ="n", 
                              BL600 ="n", OZ3X ="n", OZ4X ="n", OZ5X ="n", CSMedium ="n", 
                              CSPocket ="n", CSUltraCompact ="n"),
                     correlation = FALSE, data = data.logit, panel = TRUE)
summary(mixedlogit)

print("mixed logit with correlation")
# Fit mixed logit model with corelations
mixedlogit2 <- mlogit(chosen_alt ~ 0 + Price400 + Price300 + Price200 + Resolution3MP 
                      + Resolution4MP + Resolution5MP + BL300 + BL450 + BL600 + OZ3X 
                      + OZ4X + OZ5X + CSMedium + CSPocket + CSUltraCompact, 
                      rpar = c(Price400 ="n", Price300 ="n", Price200 ="n", Resolution3MP ="n",
                               Resolution4MP ="n", Resolution5MP ="n", BL300 ="n", BL450 ="n", 
                               BL600 ="n", OZ3X ="n", OZ4X ="n", OZ5X ="n", CSMedium ="n", 
                               CSPocket ="n", CSUltraCompact ="n"),
                      correlation = TRUE, data = data.logit, panel = TRUE)
# Show mixed logit results
summary(mixedlogit2)
#rpar(mixedlogit2)

############ Shares for plain and mixed logit models ############ 
# Choice variable will be removed (do not consider it. It is needed to avoid format error afterwards)
camera_attributes <- list(Choice=rep(c(0),times=4),
                          Price = c("500", "400", "300", "200"),
                          Resolution = c("2MP", "3MP", "4MP", "5MP"),
                          BL = c("150", "300", "450", "600"),
                          OZ = c("2x", "3x", "4x", "5x"),
                          CS = c("SRL", "Medium", "Pocket", "UltraCompact"))
# Get some profiles
new_cam_data <- expand.grid(camera_attributes)[c(1967, 2645, 300, 2033, 3401),]
# Get dummies
model_data_tmp <- model.matrix(Choice ~ 0 +., data = new_cam_data)
# Drop Price500
model_data <- model_data_tmp[,-1]
# Homogeneous mlogit:
# Calculate utilities
options_utility <- model_data %*% plainlogit$coef
# Compute shares using the multinomial logit equation (choice probabilities)
share <- exp(options_utility) / sum(exp(options_utility))
# Merge profiles with its shares
share_data <- cbind(new_cam_data[,-1], share)
share_data

# Heterogeneous mlogit with correlations
library(MASS) # For mvrnorm
# Set a number of respondents
resp <- 100
# Get covariance matrix
cov_mlogit <- cov.mlogit(mixedlogit2)
# Get the mean of the random coefficients
coeff_mu <- mixedlogit2$coefficients[1:dim(cov_mlogit)[1]]
# Get radom sample 
draws <- mvrnorm(resp, coeff_mu, cov_mlogit)
# Create a matrix to keep the shares
cam_shares <- matrix(NA, nrow = resp, ncol = nrow(new_cam_data))
# Calculate the shares for each respondent
for (i in 1:resp){
  utility <- model_data%*%draws[i,]
  share <- exp(utility) / sum(exp(utility))
  cam_shares[i,] <- share 
}
# Calculate the mean of the shares for each profile
mean_shares <- colMeans(cam_shares)
# Merge profiles with its shares
share_data <- cbind(new_cam_data[,-1], mean_shares)
share_data

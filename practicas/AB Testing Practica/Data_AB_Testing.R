# load the library
#1. Prepare the dataset and load the tidyverse library which contains the relevant packages used for the analysis.


library(tidyverse)

# set up your own directory
setwd('/Users/seguram/Documents/AB Test')

# Using read.csv base import function
ABTest <- read.csv("Website Results.csv",
                   header = TRUE)

# save in your own directory
save(ABTest, file = "~rda\\ABTest.rda")

#Let’s filter conversions for variants A & B and compute their corresponding conversion rates
# Let's filter out conversions for variant_A
conversion_subset_A <- ABTest %>%
  filter(variant == "A" & converted == "TRUE")

# Total Number of Conversions for variant_A
conversions_A <- nrow(conversion_subset_A)

# Number of Visitors for variant_A
visitors_A <- nrow(ABTest %>%
                     filter(variant == "A"))

# Conversion_rate_A
conv_rate_A <- (conversions_A/visitors_A)
print(conv_rate_A) # 0.02773925

# Let's take a subset of conversions for variant_B
conversion_subset_B <- ABTest %>%
  filter(variant == "B" & converted == "TRUE")

# Number of Conversions for variant_B
conversions_B <- nrow(conversion_subset_B)

# Number of Visitors for variant_B
visitors_B <- nrow(ABTest %>%
                     filter(variant == "B"))

# Conversion_rate_B
conv_rate_B <- (conversions_B/visitors_B)
print(conv_rate_B) # 0.05068493

#Let’s compute the relative uplift using conversion rates A & B. The uplift is a percentage of the increase
uplift <- (conv_rate_B - conv_rate_A) / conv_rate_A * 100
uplift # 82.72%

#Let’s compute the pooled probability, standard error, the margin of error, and difference in proportion (point estimate) for variants A & B
# Pooled sample proportion for variants A & B
p_pool <- (conversions_A + conversions_B) / (visitors_A +
                                               visitors_B)
print(p_pool) # 0.03928325

# Let's compute Standard error for variants A & B (SE_pool)
SE_pool <- sqrt(p_pool * (1 - p_pool) * ((1 / visitors_A) +
                                           (1 / visitors_B)))
print(SE_pool) # 0.01020014

# Let's compute the margin of error for the pool
MOE <- SE_pool * qnorm(0.975)
print(MOE) # 0.0199919

# Point Estimate or Difference in proportion
d_hat <- conv_rate_B - conv_rate_A

#Let’s compute the z-score
# Compute the Z-score so we
# can determine the p-value
z_score <- d_hat / SE_pool
print(z_score) # 2.249546

#Using this z-score, we can quickly determine the p-value via a look-up table, or using the code below:
# Let's compute p_value
# using the z_score value
p_value <- pnorm(q = -z_score,
                 mean = 0,
                 sd = 1) * 2
print(p_value) # 0.02447777

#Let’s compute the confidence interval for the pool
# Let's compute Confidence interval for the
# pool using pre-calculated results
ci <- c(d_hat - MOE, d_hat + MOE)
ci # 0.002953777 0.042937584

# Using same steps as already shown,
# let's compute the confidence
# interval for variants A separately
X_hat_A <- conversions_A / visitors_A
se_hat_A <- sqrt(X_hat_A * (1 - X_hat_A) / visitors_A)
ci_A <- c(X_hat_A - qnorm(0.975) * se_hat_A, X_hat_A
          + qnorm(0.975) * se_hat_A)
print(ci_A) # 0.01575201 0.03972649

# Using same steps as already shown,
# let's compute the confidence
# interval for variants B separately								
X_hat_B <- conversions_B / visitors_B
se_hat_B <- sqrt(X_hat_B * (1 - X_hat_B) / visitors_B)
ci_B <- c(X_hat_B - qnorm(0.975) * se_hat_B,
          X_hat_B + qnorm(0.975) * se_hat_B)
print(ci_B) # 0.03477269 0.06659717

#Let’s visualize the results computed so far in a dataframe (table):
vis_result_pool <- data.frame(
  metric = c(
    'Estimated Difference',
    'Relative Uplift(%)',
    'pooled sample proportion',
    'Standard Error of Difference',
    'z_score',
    'p-value',
    'Margin of Error',
    'CI-lower',
    'CI-upper'),
  value = c(
    conv_rate_B - conv_rate_A,
    uplift,
    p_pool,
    SE_pool,
    z_score,
    p_value,
    MOE,
    ci_lower,
    ci_upper
  ))
print (vis_result_pool)


  

  




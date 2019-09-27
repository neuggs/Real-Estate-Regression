library(ggplot2)
library(Hmisc)
library(ggm)
library(dplyr)
library(corrplot)
library(pastecs)
library(readxl)
library(scales)
library(formattable)
library(data.table)

# Import data
housing_data <- read_excel("week-7-housing.xlsx")
#str(housing_data)
housing_subset <- housing_data[c('Sale Price', 'sq_ft_lot')]
count_all <- count(housing_subset)


# rename column
setnames(housing_subset, old = c('Sale Price', 'sq_ft_lot'), new = 
           c('sale_price', 'sq_ft_lot'))

splot <- ggplot(data = housing_subset, aes(x = sq_ft_lot, y = sale_price)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Sale Price affected by Lot Size')
suppressWarnings(print(plot))

ggplot(housing_subset, aes(x = sale_price)) +
  geom_histogram() + 
  labs(title = 'Histogram for Sale Price')
qplot(sample = housing_subset$sale_price, stat='qq')
desc_sale_price <- describe(housing_subset$sale_price)
desc_sale_price$skew

ggplot(housing_subset, aes(x = sq_ft_lot)) +
  geom_histogram() +
  labs(title = 'Histogram for Lot Size')
qplot(sample = housing_subset$sq_ft_lot, stat='qq')
desc_lot_size <- describe(housing_subset$sq_ft_lot)
desc_lot_size$skew

housing_subset$log_price <- log(housing_subset$sale_price)
ggplot(housing_subset, aes(x = log_price)) +
  geom_histogram() +
  labs(title = 'Histogram for Log Price')
qplot(sample = housing_subset$log_price, stat='qq')
log_price_desc <- describe(housing_subset$log_price)
log_price_desc$skew

housing_subset$log_size <- log(housing_subset$sq_ft_lot)
ggplot(housing_subset, aes(x = log_size)) +
  geom_histogram() +
  labs(title = 'Histogram for Log Size')
qplot(sample = housing_subset$log_size, stat='qq')
log_size_desc <- describe(housing_subset$log_size)
log_size_desc$skew


price_size_lm <- lm(sale_price ~ sq_ft_lot, data = housing_subset)
housing_subset$resid <- resid(price_size_lm)
housing_subset$stand_resid <- rstandard(price_size_lm)
housing_subset$stud_resid <- rstudent(price_size_lm)
housing_subset$cooks_dist <- cooks.distance(price_size_lm)
housing_subset$dfbeta <- dfbeta(price_size_lm)
housing_subset$dffit <- dffits(price_size_lm)
housing_subset$leverage <- hatvalues(price_size_lm)
housing_subset$covariance_ratios <- covratio(price_size_lm)
write.table(housing_subset, "Housing_With_Diagnostics.dat", sep="\t", row.names = FALSE)

housing_subset$large_residual <- housing_subset$stand_resid > 2 | housing_subset$stand_resid < -2
sum(housing_subset$large_residual)
housing_subset$large_res_high_cooks <- housing_subset$large_residual & housing_subset$cooks_dist > 1
sum(housing_subset$large_res_high_cooks)
housing_subset[housing_subset$large_residual, c('sale_price', 'sq_ft_lot', 'resid', 'stand_resid',
                                                'stud_resid', 'cooks_dist', 'dfbeta', 'dffit',
                                                'leverage', 'covariance_ratios')]
outside_limits <- 334/12865
outside_limits # 2.596%
housing_subset

housing_subset$far_outside_resid <- housing_subset$stand_resid > 2.5 | housing_subset$stand_resid < -2.5
tmp_some_val <- housing_subset[housing_subset$far_outside_resid, c('sale_price', 'sq_ft_lot', 'cooks_dist',
                                                'leverage', 'covariance_ratios','stand_resid')]

# Looking at cook's a leverage
housing_subset[housing_subset$large_residual, c('sale_price', 'sq_ft_lot', 'cooks_dist',
                                                'leverage', 'covariance_ratios')]
# Since cook's is < 1 for all cases, none of the cases is having an undue influence.

# leverage
# avg is number of predictors plus one divided by sample size
avg_leverage <- 1 / count_all
two_x <- avg_leverage * 2
three_x <- avg_leverage * 3
housing_subset$two_x_leverage <- housing_subset$leverage >= two_x & housing_subset$leverage < three_x
housing_subset$three_x_leverage <- housing_subset$leverage >= three_x

two_x_leverage_sum <- sum(housing_subset$two_x_leverage)
three_x_leverage_sum <- sum(housing_subset$three_x_leverage)
two_x_subset <- housing_subset[housing_subset$two_x_leverage, c('sale_price', 'sq_ft_lot', 'cooks_dist','leverage','covariance_ratios','stand_resid')]

# Covariance is last - boundaries are 


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
library(QuantPsyc)
library('psychometric')
library(lmtest)
library(car)

housing_data <- read_excel("week-7-housing.xlsx")
housing_subset <- housing_data[c('Sale Price', 'sq_ft_lot', 'square_feet_total_living', 'bedrooms',
                                 'bath_full_count', 'year_built')]
count_all <- count(housing_subset)
setnames(housing_subset, old = c('Sale Price', 'sq_ft_lot', 'square_feet_total_living', 'bedrooms',
                                 'bath_full_count', 'year_built'), new = c('sale_price', 'sq_ft_lot', 
                                                                           'square_feet_total_living',
                                                                           'bedrooms','bath_full_count', 
                                                                           'year_built'))

housing_subset$log_price <- log(housing_subset$sale_price)
housing_subset$log_size <- log(housing_subset$sq_ft_lot)

price_size_lm <- lm(log_price ~ log_size, data = housing_subset)
housing_subset$resid <- resid(price_size_lm)
housing_subset$stand_resid <- rstandard(price_size_lm)
housing_subset$stud_resid <- rstudent(price_size_lm)
housing_subset$cooks_dist <- cooks.distance(price_size_lm)
housing_subset$leverage <- hatvalues(price_size_lm)
housing_subset$covariance_ratios <- covratio(price_size_lm)

housing_subset$large_residual <- housing_subset$stand_resid > 2 | housing_subset$stand_resid < -2
large_resid_sum <- sum(housing_subset$large_residual)
housing_subset_large_resid <- housing_subset[housing_subset$large_residual, c('sale_price', 
   'sq_ft_lot', 'resid', 'stand_resid', 'cooks_dist', 'leverage', 'covariance_ratios')]

avg_leverage <- 0.000155460552
two_x <- avg_leverage * 2
three_x <- avg_leverage * 3
housing_subset$two_x_leverage <- housing_subset$leverage >= two_x & 
                            housing_subset$leverage < three_x
sum(housing_subset$large_residual)
sum(housing_subset$two_x_leverage)
sum((housing_subset$large_residual == TRUE) & (housing_subset$two_x_leverage == TRUE))
sum((housing_subset$large_residual == FALSE) | (housing_subset$two_x_leverage == FALSE))

final_data <- housing_subset[(housing_subset$two_x_leverage == FALSE) | 
                               (housing_subset$large_residual == FALSE), 
                             c('sale_price', 'sq_ft_lot', 
                               'square_feet_total_living',
                               'bedrooms','bath_full_count', 
                               'year_built')]

pairs(final_data)

lm_price_size <- lm(sale_price ~ sq_ft_lot, data = final_data)
lm_price_size_br_living <- lm(sale_price ~ sq_ft_lot + bedrooms + square_feet_total_living, data = final_data)

summary(lm_price_size)
summary(lm_price_size_br_living)

AIC_delta <- AIC(lm_price_size_br_living, k = 2) - AIC(lm_price_size, k = 2)
betas <- lm.beta(lm_price_size_br_living)
betas[1]
betas[2]
betas[3]

anova <- anova(lm_price_size, lm_price_size_br_living)

final_data$resid <- resid(lm_price_size_br_living)
final_data$stand_resid <- rstandard(lm_price_size_br_living)
final_data$stud_resid <- rstudent(lm_price_size_br_living)
final_data$cooks_dist <- cooks.distance(lm_price_size_br_living)
final_data$leverage <- hatvalues(lm_price_size_br_living)
final_data$covariance_ratios <- covratio(lm_price_size_br_living)

final_data[c('resid', 'stand_resid', 'stud_resid', 'cooks_dist', 'leverage', 'covariance_ratios')]

final_data$large_residual <- final_data$stand_resid > 2 | final_data$stand_resid < -2
# i. Use the appropriate function to show the sum of large residuals.
sum(final_data$large_residual)

final_data_large_resid <- final_data[final_data$large_residual, c('sale_price', 'sq_ft_lot', 'resid')]

cooks <- final_data[final_data$cooks_dist, c('sale_price', 'sq_ft_lot', 'resid')]

avg_leverage <- 3 / count(final_data)
final_data$two_x_leverage <- final_data$leverage >= 0.0004671078 & 
  final_data$leverage < 0.0007006617
final_data$three_x_leverage <- final_data$leverage >= 0.0007006617

bound_low <- 1 - 12/12845
bound_high <- 1 + 12/12845
covariance_deviation_sum <- sum(final_data$covariance_ratios 
                                <= bound_high & final_data$covariance_ratios >= bound_low)
covariance_deviation_sum

dwt <- durbinWatsonTest(lm_price_size_br_living)
dwt

vif <- vif(lm_price_size_br_living)
vif_tol <- 1 / vif
mean_vif <- mean(vif)

vif
vif_tol
mean_vif

final_data$fitted <- lm_price_size_br_living$fitted.values
histogram <- ggplot(final_data, aes(stud_resid)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'white') +
  labs(x = 'Studentized Residual', y = 'Density')
histogram + stat_function(fun = dnorm, args = list(mean = mean(final_data$stud_resid, na.rm = TRUE),
                                                   sd = sd(final_data$stud_resid, na.rm = TRUE)),
                          color = 'red', size = 1)

qplot(sample = final_data$stud_resid, stat = 'qq') + labs(x = 'Theoretical Values', y = 'Observed Values')

scatter <- ggplot(final_data, aes(fitted, stud_resid))
scatter + geom_point() + geom_smooth(method = 'lm', color = 'black') + labs(x = 'Fitted Values', 
   y = 'Studentized Residual')


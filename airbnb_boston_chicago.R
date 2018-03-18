
## DATASET http://tomslee.net/airbnb-data-collection-get-the-data

library(dplyr)
library(ggpubr)
library(gtools)
library(bootstrap)

boston_data <- read.csv("E:\\prob_models\\project\\boston_1bhk.csv")
chicago_data <- read.csv("E:\\prob_models\\project\\chicago_1bhk.csv")

boston_vec <- as.vector(boston_data$price)
chicago_vec <- as.vector(chicago_data$price)


my_data <- data.frame( 
  city = c( rep("Boston",length(boston_vec)),rep("Chicago",length(chicago_vec)) ),
  price = c(boston_vec,  chicago_vec)
)

head(my_data)

########################################################################################################################

##summary table

group_by(my_data, city) %>%
  summarise(
    count = n(),
    mean = mean(price, na.rm = TRUE),
    sd = sd(price, na.rm = TRUE)
  )


par(mfrow = c(1,2))
##Histogram and comparison plot
hist(boston_vec, breaks = 25,
main = "Distribution of 1bhk prices in Boston", xlab = "Price")

hist(chicago_vec, breaks = 25,
     main = "Distribution of 1bhk prices in Chicago", xlab = "Price")

ggboxplot(my_data, x = "city", y = "price", 
          color = "city", palette = c("#00AFBB", "#E7B800"),
          ylab = "City", xlab = "City", title = "Price of 1bhk accommodation")

##########################################################################################################################
#                                                  EMPIRICAL CDF ESTIMATION                                              #
##########################################################################################################################
par(mfrow = c(1,2))

# Determining Empirical Distribution of 1bhk accommodation in Boston
ecdf_B <- ecdf(boston_vec)
n <- length(boston_vec)
alpha <- 0.05

# Plotting ecdf, 95% confidence interval for ecdf and fitting normal curve
plot.ecdf(boston_vec)
Eps <- sqrt(log(2/alpha)/(2*n))
grid <- seq(floor(min(boston_vec)), ceiling(max(boston_vec)), length.out = 1000)
lines(grid, pmin(ecdf_B(grid) + Eps,1), col = "blue")
lines(grid, pmax(ecdf_B(grid) - Eps,0), col = "blue")
normal_pts <- seq(floor(min(boston_vec)), ceiling(max(boston_vec)), length.out = 10000)
lines(normal_pts, pnorm(normal_pts, mean = mean(boston_vec), sd = sd(boston_vec)), col = 'red', lwd = 3, lty = 3)

#---------------------------------------------------------------------------------------------------------------------------

# Determining Empirical Distribution of 1bhk accommodation in Chicago
ecdf_C <- ecdf(chicago_vec)
n <- length(chicago_vec)
alpha <- 0.05

# Plotting ecdf, 95% confidence interval for ecdf and fitting normal curve
plot.ecdf(chicago_vec)
Eps <- sqrt(log(2/alpha)/(2*n))
grid <- seq(floor(min(chicago_vec)), ceiling(max(chicago_vec)), length.out = 1000)
lines(grid, pmin(ecdf_C(grid) + Eps,1), col = "blue")
lines(grid, pmax(ecdf_C(grid) - Eps,0), col = "blue")
normal_pts <- seq(floor(min(chicago_vec)), ceiling(max(chicago_vec)), length.out = 10000)
lines(normal_pts, pnorm(normal_pts, mean = mean(chicago_vec), sd = sd(chicago_vec)), col = 'red', lwd = 3, lty = 3)

############################################################################################################################
#                                              NON PARAMETRIC BOOTSTRAP                                                    #
############################################################################################################################
par(mfrow = c(1,2))

##Boston
B <- 1000
n1 <- length(boston_vec)
bo_hat <- mean(boston_vec)

mean_boston.boot <- bootstrap(boston_vec, nboot = B, mean)
hist(mean_boston.boot$thetastar,main = "sampling distribution of means of prices in Boston",xlab = "mean")
se_boston_hat.boot <- sqrt(var(mean_boston.boot$thetastar))
bo_hat
se_boston_hat.boot
CI_95_boston.mean <- c(bo_hat - 2*se_boston_hat.boot , bo_hat + 2*se_boston_hat.boot)
CI_95_boston.mean

##Chicago
n2 <- length(chicago_vec)
ch_hat <- mean(chicago_vec)

mean_chicago.boot <- bootstrap(chicago_vec, nboot = B, mean)
hist(mean_chicago.boot$thetastar, main = "sampling distribution of means of prices in Chicago", xlab = "mean")
se_chicago_hat.boot <- sqrt(var(mean_chicago.boot$thetastar))

ch_hat
se_chicago_hat.boot
CI_95_chicago.mean <- c(ch_hat - 2*se_chicago_hat.boot , ch_hat + 2*se_chicago_hat.boot)
CI_95_chicago.mean

#############################################################################################################################
#                                                 SIGNIFICANCE TESTING                                                      #
#############################################################################################################################

## independent t test

# Shapiro-Wilk normality test for Boston's prices
with(my_data, shapiro.test(price[city == "Boston"]))
# Shapiro-Wilk normality test for Chicago's prices
with(my_data, shapiro.test(price[city == "Chicago"])) 

# From the output, the two p-values are less than the significance level 0.05 implying 
# that the distribution of the data in both the cases is significantly different from the normal distribution. In other words, 
# we can not assume normality. This implies failure of assumptions to perform an independent 2 sample t test.
# So, We shall use wilcoxan rank sum test to compare the prices of 1bhk accommodation between both the cities.


##2 sample Kolmogorov-Smirnov (KS) test

ks.test(boston_vec,chicago_vec)

##Mann-Whitney-Wilcoxan Test

wilcox.test(boston_vec, chicago_vec, alternative = "greater", conf.int = T)


#The p-value of the test is 6.662e-10 , which is less than the significance level alpha = 0.05. 
#We can conclude that the price of 1 bedroom accommodation in Boston is significantly higher than the price of 1 bedroom 
#accommodation in Chicago. 


## The Permutation test

data.vector <- c(boston_vec,chicago_vec)

t_obs <- abs(mean(boston_vec) - mean(chicago_vec))
perm.matrix <- t(replicate(100000,sample(649)))
perm.T <- apply(perm.matrix, 1, function(x) {abs(mean(data.vector[x[1:324]]) - mean(data.vector[x[325:649]]))})
p.value <- mean(perm.T > t_obs)

#################################################################################################################################


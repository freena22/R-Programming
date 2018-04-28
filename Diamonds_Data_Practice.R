##### Read in Data

data(diamonds)

str(diamonds)
# 53940 obs / 10 variables
summary(diamonds)
# Best color : D


##### 1. Price Histogram

summary(diamonds$price)
library(ggplot2)
qplot(x = price, data = diamonds)

summary(diamonds$price < 500)

summary(diamonds$price < 250)

summary(diamonds$price <= 15000)

# limiting the x-axis, altering the bin width and setting different breaks on the x-axis.
summary(diamonds$price)
library(ggplot2)
qplot(x = price, data = diamonds, binwidth = 100,
      color =  I('gray'), fill = I('#F79420'),
      xlab = 'Price',
      ylab = 'Count') +
  scale_x_continuous(limits = c(0, 15000), breaks = seq(0, 15000, 1000))
ggsave('priceHistogram.png')

# There are no diamonds that cost $1500.
# For that less than $2,000,the most common price of a diamond is around $700 with the mode being $605 (binwidth = 1)

##### 2. Price by Cut Histograms


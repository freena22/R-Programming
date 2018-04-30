##### Read in Data

library(ggplot2)
data(diamonds)

str(diamonds)
# 53940 obs / 10 variables
summary(diamonds)
# Best color : D

## Part I: One Varaible

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

qplot(x = price, data = diamonds) + facet_wrap(~cut)
by(diamonds$price, diamonds$cut, summary)


# add a parameter to facet_wrap for non-fixed y-axis in the histograms (y-axis to be different for each histogram).
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free")

##### 3. Price per Carat by Cut
# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.

# Adjust the bin width and transform the scale
# of the x-axis using log10.

qplot(x = price/carat, data = diamonds, binwidth = 0.03) +
  scale_x_log10() + facet_wrap(~cut)
# Result: normal-ish distributions and slightly bimodal distribution for Very Good cuts

##### 4. Price by color Box Plots

by(diamonds$price, diamonds$color, summary)
qplot(x = color, y = price/carat, 
      data = diamonds, geom = 'boxplot')
ggsave('PricePerCaratBoxPlots.png')

# check the IQR
IQR(subset(diamonds, color == 'D')$price)

##### 5. Carat Frequency Polygon
qplot(x = carat, data = diamonds, binwidth = 0.1, geom = 'freqpoly') +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.1))

table(diamonds$carat)

## Part II: Two Varaibles

##### 1. Scatterplot of price vs x.

library(ggplot2)
ggplot(aes(x = price, y = x), data = diamonds) +
  geom_point()

cor.test(diamonds$price, diamonds$x)
cor.test(diamonds$price, diamonds$y)
cor.test(diamonds$price, diamonds$z)

##### 2. Scatterplot of price vs depth
ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point(alpha = 1/100)


# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units.




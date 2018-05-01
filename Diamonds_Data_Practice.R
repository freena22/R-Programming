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

# Change the code to make the transparency of the points to be 1/100 
ggplot(aes(x = price, y = depth), data = diamonds) +
  geom_point(alpha = 1/100)

cor.test(diamonds$price, diamonds$depth)

##### 3. Scatterplot of price vc carat

#  omit the top 1% of price and carat values.

ggplot(aes(x = price, y = carat), data = diamonds) +
  geom_point() + 
  xlim(0, quantile(diamonds$price, 0.90))

##### 4. Scatterplot of price vs. volume (x * y * z)

# This is a very rough approximation for a diamond's volume.
# Create a new variable for volume in the diamonds data frame.
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
head(diamonds$volume)

ggplot(aes(x = price, y = volume), data = diamonds) +
  geom_point() +
  ylim(0,500)  

# The correlation of price and volume

# Exclude diamonds that have a volume of 0 or that are greater than or equal to 800
library(dplyr)
with(subset(diamonds, volume != 0 & volume < 800), cor.test(price, volume))

# Subset the data to exclude diamonds with a volume greater than or equal to 800 and equal to 0. 
# Adjust the transparency of thevpoints and add a linear model to the plot.

ggplot(aes(x = price, y = volume), 
             data = subset(diamonds, volume < 800 & volume != 0)) +
  geom_line(alpha = 1/3, position = position_jitter(h = 0)) +
  geom_smooth(method = "lm") 

##### 5. Create a new data frame diamondsByClarity
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
data(diamonds)

clarity_groups <- group_by(diamonds, clarity)

diamonds.diamondsByClarity <- summarise(clarity_groups,
                                        mean_price = mean(price),
                                        median_price = median(as.numeric(price)),
                                        min_price = min(price),
                                        max_price = max(price),
                                        n = n())
head(diamonds.diamondsByClarity, 20)

##### 6. Create summary data frames with the mean price by clarity and color
#####    And create two bar plots on one output image
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, 
                                    mean_price = mean(price))
head(diamonds_mp_by_clarity)

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color,
                                  mean_price = mean(price))
head(diamonds_mp_by_color)


p1 <- barplot(diamonds_mp_by_clarity$mean_price, 
              names.arg = diamonds_mp_by_clarity$clarity, 
              horiz = TRUE, border = NA, main = 'Diamond Price vs. Clarity')

p2 <- barplot(diamonds_mp_by_color$mean_price, 
              names.arg = diamonds_mp_by_color$color,
              horiz = TRUE, border = NA, main = 'Diamond Price vs. Color')

library(gridExtra)
grid.arrange(p1, p2, ncol = 1)



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

## Part III:  Diamonds & Price Predictions

# 1. price and carat relationship

qplot(data = diamonds, x= carat, y = price,
      xlim = c(0, quantile(diamonds$carat, 0.99)),
      ylim = c(0, quantile(diamonds$price, 0.99))) +
  # use quantile() to omit the top 1% of values 
  geom_point(fill = I('#F79420'), color = I('blue'), shape = 21)

# The plot shows two things: 1. nonlinear relationship, maybe exponential or else
# 2. dipersion or variance of the relationship also increases as carat size increases

# Another option with ggplot but with a smooth line
ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point(color = '#F79420', alpha = 1/4) +
  stat_smooth(method = 'lm') +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)))

# the trend line doesn't go through the center of tha data at some key places.
# If we use this as predictions, we might be off for some key places.

# 2. ggpair function

# load the ggplot graphics package and the others
library(ggplot2)
library(GGally)
library(scales)
library(memisc)
# sample 10,000 diamonds from the data set
set.seed(20022012)
diamond_sample <- diamonds[sample(1:length(diamonds$price), 10000),]
ggpairs(diamond_sample, 
        lower = list(continuous = wrap('points', shape = I('.'))),
        upper = list(combo = wrap('box', outlier.shape = I('.'))))


# log_transform the price

library(gridExtra)
plot1 <- qplot(x = price, data = diamonds, binwidth = 100) + 
  ggtitle('Price')
plot2 <- qplot( x = price, data = diamonds, binwidth = 0.01) +
  scale_x_log10() +
  ggtitle('Price (log10)')
grid.arrange(plot1, plot2, ncol = 2)

# plot1 shows a heavily skewed where plot2 get close to a bell curve of a normal distribution
# and even a bimodality on this log ten scale, which is consistent with two class rich buyer and poor buyer
qplot( x = price, data = diamonds, binwidth = 0.05) +
  scale_x_log10() +
  facet_wrap(~cut, scales = "free")
  ggtitle('Price by Cut (log10)')

# 3. replot the data
  
### original scatter plot of price and carat
qplot(carat, price, data = diamonds) +
  scale_y_continuous((trans = log10_trans())) +
  ggtitle('Price (log10) by Carat')
# it looks less dispersed at the high end of carat size and price
  
### Create a new function to transform the carat variable
cuberoot_trans = function() trans_new('cuberoot', transform = function(x) x^(1/3),
                                        inverse = function(x) x^3)

### Use the cuberoot_trans function
ggplot(aes(carat, price), data = diamonds) + 
    geom_point() + 
    scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                       breaks = c(0.2, 0.5, 1, 2, 3)) + 
    scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                       breaks = c(350, 1000, 5000, 10000, 15000)) +
    ggtitle('Price (log10) by Cube-Root of Carat')
# get our data in a noice scale, and looks almost linear

### Overplotting Revisited
head(sort(table(diamonds$carat), decreasing = T))
 
head(sort(table(diamonds$price), decreasing = T))

# outputs show very high numbers which will result in substantial amount of overplotting
# that can obsecure some of the density and the sparsity of the data
# it can deal with it by making points smaller by jittering and by adding transparency


ggplot(aes(carat, price), data = diamonds) + 
  geom_point(alpha = 0.1, size = 0.75, position = 'jitter') + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')

# 4. price vs. carat and clarity

# A layer called scale_color_brewer() has been added to adjust the legend and provide custom colors.

library(RColorBrewer)
# just add 'color = clarity' to ggplot(aes())
ggplot(aes(x = carat, y = price, color = clarity), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')

# clarity does seem to explain a lot of variance in price 
# -- the diamonds with lower clarity always cheaper than diamonds with better clarity

# 5. cut and price
ggplot(aes(x = carat, y = price, color = cut), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')
# according to Blue Nile, most diamonds are ideal cut

# 6. Price vs Carat and color
ggplot(aes(x = carat, y = price, color = color), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div', # remove the reverse parameter so the best color would be at the top on the list
                     guide = guide_legend(title = 'Color', reverse = FALSE, 
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')
# color does seem to explain some of the variance in price like clarity

# 7. Linear Model in R
# apply log transformation to long tail price variable and cube root of carat weight


### Building the Linear Model

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)
# Notice how adding cut to our model does not help explain much of the variance
# in the price of diamonds. This fits with out exploration earlier.

### Model probelms
# a. period from 2008 - 2014 b. inflation c. 2008 global recession (6% increasing rate after 2008)
# d. diamond market in China booming
# e. uneven recovery/price increase across differernt carat weight

# 8. A Bigger, Better Data Set

install.package('bitops')
install.packages('RCurl')
library('bitops')
library('RCurl')

diamondsurl = getBinaryURL("https://raw.github.com/solomonm/diamonds-data/master/BigDiamonds.Rda")
load(rawConnection(diamondsurl))




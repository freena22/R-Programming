getwd()
setwd('~/Desktop/R_Programming')
reddit <- read.csv('reddit.csv')
str(reddit)

table(reddit$employment.status)
table(reddit$education)

summary(reddit)

# age.range
levels(reddit$age.range)

# to visulize the result
library(ggplot2)
qplot(data = reddit, x= age.range)

# need to order the factor for more readble

# Solution 1: setting levels of ordered factors 

reddit$age.range <- ordered(reddit$age.range, 
                            levels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 or Above"))
                            
# Solution 2: 
reddit$age.range <- factor(reddit$age.range, 
                            levels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 or Above"),
                           ordered = T)
qplot(data = reddit, x= age.range)





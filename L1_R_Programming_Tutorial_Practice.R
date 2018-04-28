# < R Basic >

##### 1. Assign a value

math <- 88
art <- 98
writing <- 90
final_scores <- c(math, art, writing)

highest_score <- max(final_scores)
lowest_score <- min(final_scores)
num_classes <- length(final_scores)


##### 2. Data Type

# R has a variety of data types including scalars, vectors (numerical, character, logical), matrices, data frames, and lists.

a <- c(1,2,5.3,6,-2,4) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector

# Factor -- tell R that a variable is nominal by making it a factor, which stores the nominal values as a vector of integers.
# Factors are used to represent categorical data into several levels

majors <- c('Arts','Business','Computers & Mathematics', 'Engineering',
            'Health','Arts','Business','Social Science')

print('-----------------------------')
factor_majors <- factor(majors)  #
major_levels <- levels(factor_majors)

print(factor_majors) # Out: 8 majors
print(major_levels)  # Out: 6 majors



##### 3. Vector 
class(final_scores)  # Out: "numeric"  check the data type

class_names <- c("math","art","writing")
class(class_names)   # out: "character"

names(final_scores) <- class_names  # assign the labels(keys) to the values
named_final_scores <- final_scores  # math:88 art:98 writing:90 

# recycling rules in matching two vectors when the number of elements are not equal


# Combine vectors
harvard <- c(1,1,1,1,3)
MIT <- c(2,9,3,4,10)
uni_vector <- c(harvard, MIT)


##### 4. Matrix

# creating a matrix

uni_matrix <- matrix(uni_vector, nrow = 2, ncol = 5, byrow=TRUE)

rownames(uni_matrix) <- c(universities)
colnames(uni_matrix) <- c(categories)

dim(uni_matrix)  # Out: 2 5  -- dimension

# add vector to matrix
tuition <- c(43280,45000)
cbind(uni_matrix, tuition)  # add col
rbind(uni_vector, Duke)     # add row

matrix['rowname', 'colname']  # position
sort(matrix[, 'colname'], decreasing = FALSE)

head(matrix,2)

##### 5. Data Frame

# read in csv file
df <- read.csv('recent-grads.csv')
head(df)
tail(df)
str(df)  # check the internal structure like info()

# check integer
finance_med_salary <- 47000
fin_salary <- is.interger(finance_med_salary)  # Out: [1] FALSE

# rownames(df) <- df$Rowname

fin_salary <- df['FINANCE','Median']
above_50 <- df[df$Median > 50000,]
engineering <- df[df$Major == 'Engineering']
majors <- df[df$Median > 50000 & df$ShareWomen > 0.4]
major_choice <- majors[sort(majors$Unemployment_rate),]

##### 6. List: one dimensional data hold multiple data types

research <- list('Data Analyst', c(74000, 60000, 80000), 'Must have skills with R programming')
names(research) <- c('job_title', 'salaries','job_requirements')
named_research <- names(research)

# Another easier way

research <- list(job_title = "Data Analyst", salaries = c(74000,60000,80000),job_requirements = "Must have skills with R programming")
 
# add another colomn
research <- c(research, vacation = 21)

# positions
research[2]             # 74000 60000 80000
research[[2]]           # salaries = 74000 60000 80000
research$salaries       # 74000 60000 80000

# change the elements
research[[1]] <- c('Jr. Data Scientist')

combined <- c(data1, data2)

##### 7. Loop and IF

matches <- list(c(5,4),c(2,1),c(4,1),c(7,5),c(3,5),c(3,3),c(2,3),c(4,2))

get_results <- c()

for (match in matches){
    if (match[1] > match[2]){
        print("Win")
        get_results <- c(get_results, "Win")
    } else { 
        print("Lose")
        get_results <- c(get_results, "Lose")
        break
        } 
}

wins <- 0 
while (wins <= 15){
    if (wins == 15) {
        print("makes playoffs")
        playoffs <- c(playoffs, "makes playoffs")
    } else {
        print ("does not make playoffs")
        playoffs <- c(playoffs, "does not make playoffs")
    }


##### 8. Function

thunderbirds <- c(4,3,5,1,0,2,4,3,2,2,1,4)
flamethrowers <- c(2,4,6,0,3,4,2,3,3,2,1,0)

predict_winner <- function(x, x_name, y, y_name) {
    x_mean <- mean(x) 
    y_mean <- mean(y)
    if  (x_mean  > y_mean) {
        print(x_name)
    } else {
        print(y_name)
    }
}

winner <- predict_winner(thunderbirds, 'thunderbirds', flamethrowers, 'flamethrowers')

##### 9. Apply function

# lapply - takes in a list or vector, applied the function to the list and returns a list

brazil <- list(c(1,2),c(1,2),c(0,0),)
total_goals <- lapply(brazil, sum)  
print(total_goals)
# out: [[1]] 
#      [1] 3 
#      [[2]]
#      [1] 3
#      [[3]]
#      [1] 0

# sapply - returna a vector instead of a list
# out: [1] 3 3 0



england_scores <- list(c(1,2),c(1,2),c(0,0))
get_difference <- function(x) {
    return(abs(x[1] - x[2]))
}

goal_diff <- sapply(england_scores,get_difference)
print(goal_diff)  # Out: [1] 1 1 0

# vapply -- performs the same function as an sapply but explicitly state the data type that is returned.

england_scores <- list(c(1,2),c(1,2),c(0,0))

total <- vapply(england_scores,sum,as.numeric(1)) # return a single numeric value

# tapply -- apply a fucntion to subsets of a data structure

home_average <- tapply(df_scores$home_goals, list(df_scores$home_country), mean)


##### 10. Strings and Dates

# 1.1 paste() -- concatenate them together

# match_date : 12 June

df$match_date <- paste(df$match_date, '-2014')  # Out： "12 June -2014"


# 1.2 substr() -- extract values from a string
months <- substr(df$match_date, 3,8) # Out: " June "

# 1.3 strsplit() -- to split the string by a specific condition
df$match_date <- as.character(df$match_date)
date_split <- strsplit(df$match_date, split=' ')
print(date_split)  # Out: "12" "June" "-2014"

# 1.4 sub() -- replace the first occurence of a pattern with the new string
df$match_date <- sub('June', '-06', df$match_date)  # Out: "12 -06 - 2014"


# 1.5 gsub() -- replaces all occurance of a pattern (sub() only find the first one)

remove_space <- gsub(' ', '', df$match_date)

# 2.1 as.Date() -- converting s string into a data format

df$match_date  <- as.Date(df$match_date, format="%d-%m-%Y") # Out: "2014-06-12"

# 2.2 POSIXlt() 

pos_obj <- as.POSIXlt(df$match_date)  # Out: "2014-06-12 UTC"

# 2.3 obejct$sec / object$min / object$hour / object$mon / object$year / object$wday / object$yday
dayofweek <- df$match_date$wday 



















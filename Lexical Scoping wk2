setwd('/Week 2/specdata')

#Question1#
#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data 
#from the directory specified in the 'directory' argument 
#and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA.

pollutantmean <- function(pollutant, id = 1:332){
    temp <- list.files(pattern = "*.csv")
    sum <- 0
    p <- c()
    
    for (i in id){
        file <- read.csv(temp[i], header = TRUE)
        p <- c(p, file[,pollutant])
    }
    mean_p <- mean(p, na.rm = TRUE)
    return(round(mean_p,3))
}

##tips: should compile all the data together then find the mean. 
##If find the mean for each file, then find the mean of all means, the answer will be different.


#Question2#
#Write a function that reads a directory full of files 
#and reports the number of completely observed cases in each data file. 
#The function should return a data frame where the first column is the name of the file 
#and the second column is the number of complete cases

complete <- function(id = 1:332){
    temp <- list.files(pattern = "*.csv")
    df <- data.frame()
    
    for (i in id){
        file <- read.csv(temp[i], header = TRUE)
        num <- numeric(1)
        num <- sum(complete.cases(file))
        df <- rbind(df, c(i, num))
        names(df) <- c('id', 'nobs')
    }
    return(df)
}

#another way: 
complete <- function(id = 1:332){
    temp <- list.files(pattern = "*.csv")
    id_v <- c()
    num_v <- c()
    
    for (i in id){
        file <- read.csv(temp[i], header = TRUE)
        num_v <- c(num_v, sum(complete.cases(file)))
        id_v <- c(id_v, i)
        df <- data.frame(id = id_v, nobs = num_v)
    }
    return(df)
}

#quiz7
set.seed(42)
cc <- complete(332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


#Question3#
#Write a function that takes a directory of data files and a threshold for complete cases 
#and calculates the correlation between sulfate and nitrate for monitor locations 
#where the number of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.

corr <- function(threshold = 0){
    temp <- list.files(pattern = "*.csv")
    correlation <- c()
    
    for (i in 1:332){
        file <- read.csv(temp[i], header = TRUE)
        comp_num <- sum(complete.cases(file))
        if (comp_num > threshold){
            correlation <- c(correlation, cor(file$sulfate, file$nitrate, use = 'complete'))
        }
    }
    return(correlation)
}

#quiz8
cr <- corr()                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

#quiz9
cr <- corr(129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

#quiz10
cr <- corr(2000)                
n <- length(cr)                
cr <- corr(1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

variance = c()
for (i in 1:500){
    sample = rchisq(10,5)
    variance[i] = var(sample)
}
mean(variance)

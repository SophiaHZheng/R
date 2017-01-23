setwd('/Wk4 data')

# Get a histogram of mortality rate (11th column)
# There is 'not available' in the dataset, so have to convert data into numeric first!!!
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
hist(as.numeric(outcome[,11]))


# Find the best hospital in the state
# Write a function: 

best <- function(state, outcome) {
    #Read and compile the data
    wholedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- data.frame(wholedata[,c(7,2,11,17,23)])
    colname <- c('state_name', 'hospital', 'heart attack', 'heart failure', 'pneumonia')
    names(data) <- colname
    #7-state, 11-heart attack, 17-heart failure, 23-pneumonia
    
    #Check validity
    if (!state %in% data$state_name)  {stop('invalid state')}
    if (!outcome %in% colname)  {stop('invalid outcome')}
    
    #Return hospital name in that state with lowest 30-day death
    dataselect <- subset(data, state_name == state, select = c('state_name','hospital', outcome)) 
    dataselect2 <- dataselect[as.numeric(order(dataselect[,3]), dataselect[,2]),] 

    return(dataselect2$hospital[1])
}


# Rank a hospital by outcome in a state
# input a number of the rank, and output the hospital's name, in a state

rankhospital <- function(state, outcome, num){
    #Read outcome data
    wholedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- data.frame(wholedata[,c(7,2,11,17,23)])
    colname <- c('state_name', 'hospital', 'heart attack', 'heart failure', 'pneumonia')
    names(data) <- colname
    
    #Check validity
    if (!state %in% data$state_name)  {stop('invalid state')}
    if (!outcome %in% colname)  {stop('invalid outcome')}
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    dataselect <- subset(data, state_name == state, select = c('state_name', 'hospital', outcome))
    dataselect2 <- dataselect[order(as.numeric(dataselect[,3]), dataselect[,2], decreasing = FALSE, na.last = NA),] 
    
    if (num == 'best') {num <- 1}
    if (num == 'worst') {num <- nrow(dataselect2)}
    if (num > nrow(dataselect2)) {stop('NA')}
    
    return(dataselect2[num,2])
}


# Rank hospitals in all states

rankall <- function(outcome, num = 'best') {
    ## Read outcome data
    wholedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- data.frame(wholedata[,c(7,2,11,17,23)])
    colname <- c('state_name', 'hospital', 'heart attack', 'heart failure', 'pneumonia')
    names(data) <- colname
    
    ## Check that state and outcome are valid
    if (!outcome %in% colname)  {stop('invalid outcome')}
    
    ## For each state, find the hospital of the given rank
    state_num <- factor(data$state_name)   #find number of categories
    state_level <- levels(state_num) 
    hospitalist <- c()
    
    for (i in 1:length(state_level)) {
        dataselect <- subset(data, state_name == state_level[i], select = c('state_name', 'hospital', outcome))
        dataselect2 <- dataselect[order(as.numeric(dataselect[,3]), dataselect[,2], decreasing = FALSE, na.last = NA),] 
        
        if (num == 'best') {num <- 1}
        if (num == 'worst') {num <- nrow(dataselect2)}

        hospitalist <- c(hospitalist, dataselect2[num, 2])
    }
    hospitals <- data.frame(state_level, hospitalist)
    colnames(hospitals) <- c("state", "hospital")
    return(hospitals)
}

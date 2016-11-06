
#develop a data frame with 10 rows and 100000 columns, calculate the mean of all 100000 columns
k1 <- 10
k2 <- 10
df <- as.data.frame(matrix(rnorm(k1*k2), nrow = k1))
m <- vector('numeric', 0)     #create an empty vector with num type and 0 length

for (i in 1:k2){
  m[i] <- colMeans(df)[i]
}
mean1 <- mean(m)
# also can use:     m[i] <- mean(df[,i])



#Write a loop that loops over the columns of x, and for each column stores 
#the minimum, median, mean and maximum in the corresponding column of my.summary.

k<-1000
r<-100
set.seed(5556)
my.data <- as.data.frame(matrix(rnorm(r*k), nrow=r))

my.summary <- matrix(nrow=4,ncol=k)

for (i in 1:k){
  my.summary[1,i] <- min(my.data[,i])
  my.summary[2,i] <- median(my.data[,i])
  my.summary[3,i] <- mean(my.data[,i])
  my.summary[4,i] <- max(my.data[,i])
}

#Write a function my.function(), which takes a vector argument, and returns a length-4 vector of 
#minimum, median, mean and maximum of the input. Then recalculate my.summary using sapply().

my.function <- function(x){
  v <- c(min(x), median(x), mean(x), max(x))
}
sapply(my.data, my.function)

##TWO WAYS OF CALCULATION: sapply will be much faster than loop!!!

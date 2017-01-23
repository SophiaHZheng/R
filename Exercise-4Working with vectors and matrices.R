#creat a list, each term is a 4*4 matrix. 100 matrices in total
set.seed(9852)
my.data<-list()
for(i in 1:100){
  my.data[[i]]<-matrix(rnorm(16),nrow=4)
}

#Create a list my.index with 100  4 by 4 matrices with logical entries, 
#that indicates whether the content of my.data is negative.
my.index <- list()
my.index[[i]] <- (my.data[[i]]<0)

#Create a 4 by 4 matrix my.negatives, where each element contains the count for 
#how often the corresponding element in my.index is negative.
my.negatives <- matrix(rep(0,16),nrow=4)
for(i in 1:100){
  my.negatives  <- my.negatives + my.index[[i]]
}
my.negatives

#Use my.index to extract a vector my.negative.values with all the negative content of my.data
my.negative.values<-numeric(0) 
for(i in 1:100){
  my.negative.values <- c(my.negative.values, my.data[[i]][my.index[[i]]])
}

summary(my.negative.values)


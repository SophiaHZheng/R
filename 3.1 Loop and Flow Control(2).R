
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

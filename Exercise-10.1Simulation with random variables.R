#generate 9 figures each with 25 trials to test normality
par(mfrow = c(3,3))
set.seed(779)
for(i in 1:9){
  hist(rnorm(25), probability=TRUE,main=paste("Histogram",i))
  curve(dnorm,add=TRUE,col="red",lwd=3)
}

#run simulation 1000 times of a binomial trial. 
done <- function(){
  x<- rbinom(1,50,1/6)
  p<- x/50
  p
}
p.sim <- replicate(1000, done()) 
hist(p.sim, breaks = 15)

#is the same as following. Advantage of first method is that you can control the times of simulation you want
#but can also be achieved by function(x) where x is rbinom(x,50,1/6)
x<- rbinom(1000,50,1/6)
p<- x/50
hist(p, breaks = 15)

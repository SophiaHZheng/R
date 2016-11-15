#Always be careful when creating a variable. What's its type? What's the length? 
#1(a)
list = c(1,25,50,100,200,400,800)
y = rep(0,7)

for (l in 1:length(list)){
  n = list[l]
  samples = runif(n,0,1)
  sum = 0
  for (i in 1:n){
    sum = sum + samples[i]
  }
  y[l] = 1/sqrt(n)*sum
}

mean(y)
var(y)

#1(b)
list = c(1,25,50,100,200,400,800)
y = matrix(rep(0,7000), nrow=1000, ncol=7)

for (m in 1:1000){
  for (l in 1:length(list)){
    n = list[l]
    samples = runif(n,0,1)
    sum = 0
    for (i in 1:n){
      sum = sum + samples[i]
    }
    y[m,l] = 1/sqrt(n)*sum
  }
}

yn = rowMeans(y)
hist(yn, breaks = 20)

#1(c)
yn_mean = mean(yn)
yn_sd = sd(yn)
hist(yn, density=20, breaks=25, prob=TRUE, 
     xlab="x-variable", ylim=c(0, 4), xlim = c(5.5,6.5),
     main="normal curve over histogram")
curve(dnorm(x, mean=yn_mean, sd=yn_sd), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


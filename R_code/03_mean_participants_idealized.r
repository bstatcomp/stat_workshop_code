dgp <- rpois(100000, 12.75) # pretending we know the true distribution
mean(dgp)

#### sampling uncertainty when sample size is 16 -------------------------------
n <- 16
m <- 10000

set.seed(1)
mus <- c()
for (i in 1:m) {
  x <- sample(dgp, n, rep = T) # sampling with replacement
  mus <- c(mus, mean(x)) 
  
  if (i <= 10) {
    print(i)
    print(x)
    print(mean(x))
  }
}

hist(mus, breaks = 20)
print(quantile(mus, p = c(0.025, 0.975)))
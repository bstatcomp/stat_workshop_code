setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df    <- read.table("../data/workshops.csv", h = T, sep = ";")
x     <- df$Instructor.s.pacing.within.allotted.time
x_avg <- tapply(x, df$ID, mean)


#### t-test & CI ---------------------------------------------------------------
t.test(x, mu = 1.5, alt = "greater")
t.test(x_avg, mu = 1.5, alt = "greater")

#### bootstrap -----------------------------------------------------------------
library(boot)

res <- boot(x, function(data, idx) { mean(data[idx]) }, R = m)
boot.ci(res, type = "perc", c(0.9, 0.95))

res <- boot(x_avg, function(data, idx) { mean(data[idx]) }, R = m)
boot.ci(res, type = "perc", c(0.9, 0.95))




#### 2-level bootstrap ---------------------------------------------------------
set.seed(1)
m <- 1000
n <- length(x_avg)
mus <- NULL

for (i in 1:m) {
  idx <- sample(1:n, n, rep = T)   # reasample workshops
  
  tmp <- c()

  for (j in idx) {   # resample scores
    x_tmp <- x[df$ID == j]
    z     <- sample(x_tmp, length(x_tmp), rep = T)
    y     <- c(tmp, mean(z))
  }
  
  mus <- rbind(mus, mean(y))
}

quantile(mus, p = c(0.05, 0.95))



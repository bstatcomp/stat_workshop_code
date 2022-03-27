setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df  <- read.table("../data/workshops.csv", h = T, sep = ";")
x   <- table(df$ID) # participants per workshop

#### t-test & CI ---------------------------------------------------------------
t.test(x, mu = 12)


#### bootstrap -----------------------------------------------------------------
dgp <- x
n   <- length(dgp)
m   <- 1000

set.seed(1)
mus <- c()
for (i in 1:m) {
  y <- sample(dgp, n, rep = T) # sampling with replacement
  mus <- c(mus, mean(y))
}

hist(mus)
print(quantile(mus, p = c(0.025, 0.975)))


#### update plot with uncertainty in mean --------------------------------------
library(ggplot2)
df <- data.frame(ID = 1:16, size = as.numeric(x))

ggplot(df, aes(x = ID, y = size)) + geom_bar(stat = "identity") +
  xlab("workshop ID") + ylab("number of participants") +
  geom_hline(yintercept = mean(df$size), colour = "red", lty = "dashed") +
  annotate("text", x = 4, y =  1 + mean(df$size), 
           colour = "red", 
           label = paste0("average = ", mean(df$size), " (shaded area = 95%CI)")) +
  annotate("rect", ymin = quantile(mus, 0.025), ymax =  quantile(mus, 0.975), 
           xmin = 0, xmax = 17,
           alpha = .1, fill = "red")


#### bootstrap using a library -------------------------------------------------
library(boot)
stat_fn <- function(data, idx) {
  mean(data[idx])
}
res <- boot(x, stat_fn, R = m)
boot.ci(res, type = "perc")


#### bootstrapping the median --------------------------------------------------
stat_fn <- function(data, idx) {
  median(data[idx])
}
res <- boot(x, stat_fn, R = m)
boot.ci(res, type = "perc")
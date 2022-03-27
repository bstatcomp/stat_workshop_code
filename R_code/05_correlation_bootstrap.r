setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df    <- read.table("../data/workshops.csv", h = T, sep = ";")
score <- tapply(df$Overall, df$ID, mean)
size  <- tapply(df$Overall, df$ID, length)
df    <- data.frame(score, size)


#### test & CI -----------------------------------------------------------------
cor.test(df$score, df$size)


#### bootstrap -----------------------------------------------------------------
dgp <- df
n   <- nrow(dgp)
m   <- 1000

set.seed(1)
mus <- c()
for (i in 1:m) {
  y <- dgp[sample(1:n, n, rep = T),] # sampling with replacement
  mus <- c(mus, cor(y$score, y$size))
}

hist(mus)
quantile(mus, p = c(0.025, 0.975))


#### update plot ---------------------------------------------------------------
library(ggplot2)

ggplot(df, aes(size, score)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
  xlab("number of participants") + 
  ylab("average overall score") +
  annotate("text", x = 20, y =  1.7, colour = "red", 
           label = paste0("cor. coeff. = ", 
                          round(cor(df$size, df$score), 2), 
                          "; ",
                          sprintf("95CI = [%.2f, %.2f]", quantile(mus, 0.025), quantile(mus, 0.975))))


#### hypothesis testing with CIs -----------------------------------------------
alpha <- 0.1
quantile(mus, p = c(alpha/2, 1 - alpha/2))


# bootstrap using a library ----------------------------------------------------
library(boot)
stat_fn <- function(data, idx) {
  cor(data[idx, 1], data[idx, 2])
}
res <- boot(df, stat_fn, R = m)
boot.ci(res, type = "perc")

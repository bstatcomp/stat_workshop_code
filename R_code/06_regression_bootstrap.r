setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df    <- read.table("../data/workshops.csv", h = T, sep = ";")
score <- tapply(df$Overall, df$ID, mean)
size  <- tapply(df$Overall, df$ID, length)
df    <- data.frame(score, size)


#### ordinary least squares ----------------------------------------------------
summary(lm(score ~ size))


#### bootstrap -----------------------------------------------------------------
library(boot)
m <- 500

stat_fn <- function(data, idx) {
  lm(data[idx, 1] ~ data[idx, 2])$coeff
}

df <- rbind(df, df) # pretending that we have more data
res <- boot(df, stat_fn, R = m)
boot.ci(res, index = 1, type = "perc")
boot.ci(res, index = 2, type = "perc")


#### update plot ---------------------------------------------------------------
library(ggplot2)

g1 <- ggplot(df, aes(size, score)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
  xlab("number of participants") + 
  ylab("average overall score") +
  geom_abline(slope = res$t[,2], intercept = res$t[,1], alpha = 0.1, colour = "red")
g1
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df    <- read.table("../data/workshops.csv", h = T, sep = ";")
x <- df$Instructor.s.pacing.within.allotted.time
x_tgt <- x[df$ID == 6]
x_ref <- x[df$ID != 6]

mean(x_tgt)
mean(x_ref)


#### t-test and CI -------------------------------------------------------------
t.test(x_tgt, x_ref)


#### bootstrap -----------------------------------------------------------------
m <- 1000

set.seed(1)
mus <- c()
for (i in 1:m) {
  y1 <- sample(x_tgt, length(x_tgt), rep = T)
  y2 <- sample(x_ref, length(x_ref), rep = T) 
  mus <- c(mus, mean(y1) - mean(y2))
}

head(mus)
quantile(mus, p = c(0.025, 0.975))
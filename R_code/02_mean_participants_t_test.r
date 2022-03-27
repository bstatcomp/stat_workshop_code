setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.table("../data/workshops.csv", h = T, sep = ";")

#### t-test and CI -------------------------------------------------------------
x  <- table(df$ID) # participants per workshop
t.test(x, mu = 12)
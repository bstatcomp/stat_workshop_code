library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cln <- read.table("../data/workshops.csv", sep = ";", h = T)

#### Piechart ------------------------------------------------------------------
x  <- c("1 - Excellent", "2 - Good", "3 - Fair", "4 - Poor")
y  <- table(cln$Instructor.s.pacing.within.allotted.time)
df <- data.frame(group = x, value = as.numeric(y))

df <- df %>% 
arrange(desc(group)) %>%
mutate(prop = value / sum(df$value) *100) %>%
mutate(ypos = cumsum(prop)- 0.5*prop )

g1 <- ggplot(df, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1.0, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  ggtitle("Instructor's pacing within allotted time (n = 204)") +
  geom_text(aes(y = ypos, label = group), color = "black", size = 2.5) +
  scale_fill_brewer(palette="Set1")  
ggsave("../plots/01_piechart.png", g1, width = 4, height = 4)


#### Scatterplot ---------------------------------------------------------------
score <- tapply(cln$Overall, cln$ID, mean)
size  <- tapply(cln$Overall, cln$ID, length)
df    <- data.frame(score = score, size = size)

g1 <- ggplot(df, aes(size, score)) + geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
  xlab("number of participants") + 
  ylab("average overall score") +
  annotate("text", x = 20, y =  1.7, colour = "red", 
           label = paste0("cor. coeff. = ", round(cor(df$size, df$score), 2)))
ggsave("../plots/01_scatterplot.png", g1, width = 4, height = 4)


#### Barchart 1 ----------------------------------------------------------------
df <- data.frame(ID = 1:16, size = size)
g1 <- ggplot(df, aes(ID, size)) + geom_bar(stat = "identity") +
  xlab("workshop ID") + ylab("number of participants") +
  geom_hline(yintercept = mean(df$size), colour = "red", lty = "dashed") +
  annotate("text", x = 3, y =  1 + mean(df$size), colour = "red", 
           label = paste0("average = ", mean(df$size)))
ggsave("../plots/01_barchart-participants.png", g1, width = 6, height = 4)


#### Barchart 2 ----------------------------------------------------------------
x <- cln$Instructor.s.pacing.within.allotted.time
score_all <- table(x[cln$ID != 6]) / length(x[cln$ID != 6])
score_one <- table(x[cln$ID == 6]) / length(x[cln$ID == 6])

x <- c("1 - Excellent", "2 - Good", "3 - Fair", "4 - Poor")
df <- data.frame(count = c(score_one, score_all), 
                 grade = rep(x, 2), 
                 group = rep(c("instructor", "overall"), each = 4))

g1 <- ggplot(df, aes(grade, count, fill = group)) + 
  geom_bar(stat = "identity", position = "dodge") +
  xlab("grade") + ylab("%") +
  ggtitle("Instructor's pacing within allotted time (n = 192, 11)") 
ggsave("../plots/01_barchart-instructor.png", g1, width = 6, height = 4)
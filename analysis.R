library(tidyverse)
d <- read.csv("Tesla_Deaths.csv")
x <- d$Country[c(1:273)]
y <- strtoi(d$Deaths[c(1:273)]) 
print(pairwise.wilcox.test(y, x,data=d, alternative = "two.sided",paired = FALSE, exact = FALSE ))

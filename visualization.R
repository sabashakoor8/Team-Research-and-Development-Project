library(tidyverse)
pdf("visualization.pdf")
d <- read.csv("Tesla_Deaths.csv")
x <- d$Country[c(1:273)]
y <- strtoi(d$Deaths[c(1:273)]) 
par(mar=c(7.5,4,4,2.1)+0.1)
bp21_25 <- boxplot( y ~ x ,xaxt = "n"
                    , main = "Tesla deaths among different countries" 
                    , xlab=""
                    , ylab = "Deaths" 
                    , pch = 19
                    , las = 2 
                    , side = 1
                    , line = 9
                    , col = "#ffc800"
                    , frame = T
                    , names = sort(c('USA','UK','Canada','Germany','Finland','China','Australia','Netherlands','Switzerland','France','Denmark','Belgium','Portugal','South Korea','Norway','Taiwan','Slovenia','Austria','Ukraine','Spain','Holland','Japan'))
)
tick <- seq_along(bp21_25$names)
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[4] - 4, bp21_25$names, srt = 90, xpd = TRUE,cex=1,pos=4)
mtext("Country",side =1 ,line=5.5)
par(mar=c(4,4,4,4)+.1)
h <- hist(y
          , main = "Frequency of Tesla deaths"
          , xlab = "Deaths"
          , col = "#ffc800"
          , ylim = c(0,250)
          , xlim = c(0,4)
          , font.main = 4, font.lab = 3
)
xfit <- seq(min(y), max(y), length = 40) 
yfit <- dnorm(xfit, mean = mean(y), sd = sd(y)) 
yfit <- yfit * diff(h$mids[1:2]) * length(y) 
lines(xfit, yfit, col = "blue", lwd = 2)
dev.off()  


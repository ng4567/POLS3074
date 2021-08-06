setwd("C:/Users/d/Google Drive/Notability/Research and Design_ Data Analysis/hw4")

pollsUS08 <- read.csv("polls08.csv")

pollsUS08$middate <- as.Date(pollsUS08$middate)

#Question 1. Enter command to calculate days to election here 
#ANSWER: 

pollsUS08$DaysToElection <- as.Date("2008-11-04") - pollsUS08$middate

Obama.pred <- McCain.pred <- rep(NA, 90)

for (i in 1:90) {
  
  avg.data <- subset(pollsUS08, subset = ((DaysToElection <= (90-i+7)) 
                                          & (DaysToElection > (90-i)) ))
  Obama.pred[i] <- mean(avg.data$Obama)
  McCain.pred[i] <- mean(avg.data$McCain)
  
}

plot(90:1, Obama.pred, type = "b", xlim = c(90,0), ylim = c(40,60), 
     col = "blue", xlab = "Days to the election", 
     ylab = "Suport for candidate (percentage points)")
lines(90:1, McCain.pred, type = "b", col = "red")
points(0, 52.93, pch = 19, col = "blue")
points(0, 45.65, pch = 19, col = "red")
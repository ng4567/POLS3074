#Question 1

setwd("C:/Users/d/Google Drive/Notability/Research and Design_ Data Analysis/hw5")

pres08 <- read.csv("pres08.csv")
int08 <- read.csv("intrade08.csv")

#transform to probability
int08$pres.pr<-int08$PriceD/100


#exclude day of election
int08$DaystoElection <- as.Date("2008-11-04")-as.Date(int08$day)

pres08$EV.obama.actual <- ifelse(pres08$Obama>pres08$McCain, pres08$EV, 0)

#Merge data
int08 <- merge(int08, pres08, by="state")

#Subset data to predictions on the day before election only
int08.b4 <- subset(int08, int08$DaystoElection==1)
#other way to subset
int08.b4 <- int08[int08$DaystoElection==1,]

#calculate expected electoral votes
int08.b4$EV.pred <- int08.b4$pres.pr*int08.b4$EV

#Actual electoral votes won plus also actually won NE vote
sum(int08.b4$EV.obama.actual)+1

#Total predicted EV
sum(int08.b4$EV.pred)

#Obama has slightly overperformed expectations, winning 365 EVs, compared to the predicted 348.912

#Question 2

sims <- 10000

n.states <- length(int08.b4$state)

pijs <- int08.b4$pres.pr

#electoral votes
EVs.pr <- int08.b4$EV

Obama.ev <- rep(NA, sims)

for(i in 1:sims){
  result <- rbinom(n.states, size=1, pijs)
  
  Obama.ev[i] <- sum(EVs.pr[result==1])
  
}

hist(Obama.ev, main = "Distribution of Simulated Obama EV Predictions", xlab = "EVs")
abline(v=365, col = "blue")

#Obama's actual electoral votes were on the high end of the simulated distribution.
#The distribution centered at about 350 electoral votes, and is distributed approximately normally.

#Q3

#adjusting for bias
x <- seq(from =.01, to =0.99, by=0.1)

int08.b4$PriceD.adjusted.pr <- pnorm(1.64*qnorm(int08.b4$pres.pr))


plot(x =int08.b4$PriceD/100 , y = int08.b4$PriceD.adjusted.pr, xlab = "Pre Adjusted Probability", ylab = "Probability Adjusted for Bias", pch = 6)

x <- int08.b4$PriceD/100
abline(lm(int08.b4$PriceD.adjusted.pr~x), col = "blue")


#qnorm gives us the boundary value in the normal distribution for a given area under the curve value (in this case probability of Obama winning an election).
#Pnorm is the opposite of qnorm, so it gives you the area under the curve for a given boundary value.
#This transformation is essentially standardizes the probabilities to a distribution with mean 0 and variance 1,
#Then computes a probability while multiplying by 1.64 to inflate the values since we believe that 
#the probabilities underestimated

#Q4 

#Q1 redone


#calculate expected electoral votes
int08.b4$EV.pred.adjusted <- int08.b4$PriceD.adjusted.pr*int08.b4$EV

#Actual electoral votes won plus also actually won NE vote
sum(int08.b4$EV.obama.actual)+1

#Total predicted EV
sum(int08.b4$EV.pred)

#Total adjusted predicted EV
sum(int08.b4$EV.pred.adjusted)

#The new method does improve the prediction to 356.6449, which is closer to the true value of 365.

#Q2 redone

sims <- 10000

n.states <- length(int08.b4$state)

pijs_star <- int08.b4$PriceD.adjusted.pr

#electoral votes
EVs.pr <- int08.b4$EV

Obama.ev.adjusted <- rep(NA, sims)

for(i in 1:sims){
  result <- rbinom(n.states, size=1, pijs_star)
  
  Obama.ev.adjusted[i] <- sum(EVs.pr[result==1])
  
}

#Old histogram
hist(Obama.ev, main = "Distribution of Simulated Obama EV Predictions", xlab = "EVs", xlim = range(250,450))
abline(v=365, col = "blue")
mean(Obama.ev)

#Histogram with adjusted probability
hist(Obama.ev.adjusted, main = "Distribution of Adjusted Simulated Obama EV Predictions", xlab = "EVs")
abline(v=365, col = "blue")
mean(Obama.ev.adjusted)

#Since the mean of the distribution moved from 348 to about 356, the simulation data also simulated results closer to the actual outcome. 
#The histogram for the adjusted probabilities shifts over, demonstrating the increased accuracy of the new prediction method.
#Data is clustered around the new center of 356.

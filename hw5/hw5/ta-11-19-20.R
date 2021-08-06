pres08 <- read.csv("/Users/nikhilgopal/Google Drive/Notability/Research and Design_ Data Analysis/hw4/pres12.csv")
int08 <- read.csv("/Users/nikhilgopal/Google Drive/Notability/Research and Design_ Data Analysis/hw4/intrade08.csv")

#Q1.1

#transform to probability
int08$pres.pr<-int08$PriceD/100


#exclude day of election

int08$DaystoElection <- as.Date("2008-11-04")-as.Date(int08$day)


#calculate winning electoral votes
pres08$EV.obama.actual <- ifelse(pres08$Obama>pres08$McCain, pres08$EV, 0)

#Merge data
int08 <- merge(int08, pres08, by="state")

#Subset data to before election only
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

#Q2

#Set up our simulation parameters

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


#Q3

#adjusting for bias
x <- seq(from =.01, to =0.99, by=0.1)

int08.b4$PriceD.adjusted.pr <- pnorm(1.64*qnorm(int08.b4$pres.pr))

plot(x =int08.b4$PriceD , y = int08.b4$PriceD.adjusted.pr, xlab = "Pre Adjusted Probability", ylab = "Probability Adjusted for Bias")



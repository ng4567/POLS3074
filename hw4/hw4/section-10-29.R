setwd("C:/Users/d/Google Drive/Notability/Research and Design_ Data Analysis/hw4")

intrade08 <- read.csv('intrade08.csv')

pres08 <- read.csv('pres08.csv')


inpres <- merge(intrade08, pres08, by='state')

#days before election
inpres$daysUntil <- as.Date("2008-11-04")-as.Date(inpres$day)

#subtract betting market price of D minus R
inpres$Obama.betting.margin <- inpres$PriceD-inpres$PriceR

#Actual Results
inpres$Obama.actual.margin <- inpres$Obama-inpres$McCain

#Day before election subset
lastDay <- subset(inpres, inpres$daysUntil==1,)

#States that markets predicted correctly vs not
lastDay$state[sign(lastDay$Obama.actual.margin) !=sign(lastDay$Obama.betting.margin)]


intrade12 <- read.csv('intrade12.csv')
pres12 <- read.csv('pres12.csv')

inpres12 <- merge(intrade12, pres12, by='state')

#days before election
inpres12$daysUntil <- as.Date("2012-11-06")-as.Date(inpres12$day)

#subtract betting market price of D minus R
inpres12$Obama.betting.margin <- inpres12$PriceD-inpres12$PriceR

#Actual Results
inpres12$Obama.actual.margin <- inpres12$Obama-inpres12$Romney


#Day before election subset
lastDay12 <- subset(inpres12, inpres12$daysUntil==1,)

#States that markets predicted correctly vs not
lastDay12$state[sign(lastDay12$Obama.actual.margin) !=sign(lastDay12$Obama.betting.margin)]



#Question 2

Obama.dailyprediction <- rep(NA, 90)

for(i in 1:90){
  
  daily <- subset(inpres, subset = (inpres$daysUntil == i))
  
  Obama.dailyprediction[i] <- sum(daily$EV[daily$Obama.betting.margin >0])
  
}

plot(1:90, Obama.dailyprediction, main="Daily Prediction of ECV", xlim=c(90,-1), ylim=c(200, 400), pch=19)
points(0, 365, pch=19)
abline(h=365, lty="dashed")
text(60,380, "Actual Votes")


#Question 3

Obama.weeklyprediction <- rep(NA,90)


for(i in 1:90){
  
  week.data <- subset(inpres, subset = (inpres$daysUntil <= (90 -i +7)) & (inpres$daysUntil>(90-i)))
  
  
  #Average price margin with each state
  #Bind price margin to electoral votes
  
  week.means <- cbind(tapply(week.data$Obama.actual.margin, week.data$state, mean, na.rm=TRUE), tapply(week.data$EV, week.data$state, mean, na.rm=TRUE))
  
  #only states Obama is predicted to win
  
  week.obama.win <- week.means[week.means[,1]>0,]
  
  #Sum Obama EV
  
  Obama.weekly.prediction <- sum(week.obama.win[2,])
                               
  
}

plot(1:90, Obama.weekly.prediction, main="7 day average EV", ylim = c(200,400), xlim=c(0,90))
abline(h=365, lty="dashed")


#Question 3
prog <- read.csv('progresa.csv')

itt.turn <- mean(prog$t2000[prog$treatment==1]-mean(prog$t2000[prog$treatment==0]))
itt.turn
#increases turnout by 4.3%

reg.turn  <- lm(prog$t2000~prog$treatment)
reg.turn <- lm(t2000~treatment, data=prog)
#increases turnout by 4.27%

itt.pri <- mean(prog$pri2000s[prog$treatment==1])-mean(prog$pri2000s[prog$treatment==0])

reg.pro <- lm(pri2000s~treatment, data=prog)
summary(reg.pri)
#3.62


#Q2


mreg.turn <- lm(t2000~treatment+avgpoverty+pobtot1994+votos1994+pri1994+pan1994+prd1994, data = prog)
mreg.turn
#4.6% increase

mreg.pri <- lm(pri2000s~treatment+avgpoverty+pobtot1994+votos1994+pri1994+pan1994+prd1994, data = prog)
meg.pri
#2.9%
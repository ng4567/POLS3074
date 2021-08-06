setwd("/Users/nikhilgopal/Google Drive/Notability/Research and Design_ Data Analysis/Final/data")

library(dplyr) 
library(ggplot2)
library(ggpubr)

data <- read.csv("masterdata.csv") 
election <- read.csv("1976-2016-president.csv")

data$voteddem <- ifelse(data$VotedDem.=="Yes", 1,0)
data$didoptin <- ifelse(data$OptedIn=="Yes", 1,0)

data$voteddem = as.factor(data$voteddem)
data$didoptin <- as.factor(data$didoptin)

election = subset(election, year == 2016)
election$voteshare = election$candidatevotes[election$party=="democrat"]/election$totalvotes[election$party=="democrat"]

election$vote2 = election$candidatevotes/election$totalvotes


sub1 <- subset(election, year == 2016)
sub1 <- subset(sub1, party=="democrat")
sub1 = subset(sub1, candidate=="Clinton, Hillary")
sub1 = subset(sub1, candidatevotes > 100)

sub2 <- subset(election, year == 2012)
sub2 <- subset(sub1, party=="democrat")
sub2 = subset(sub1, candidate=="Clinton, Hillary")
sub2 = subset(sub1, candidatevotes > 100)


model_2012_included <- lm(sub1$vote2~data$didoptin+sub2$vote2+data$InflationAdjustedACAsubsidy+data$Medicaid.Federal.Dollars.Spent)

model_wo_2012 <- lm(sub1$vote2~data$didoptin+data$InflationAdjustedACAsubsidy+data$Medicaid.Federal.Dollars.Spent)

summary(model_votedem_V_medicaid_subsidy_opted_in)
#next steps
#add in 2012 vote share into OLS model, more graphics


#uninsured data


plot(x=data$voteddem, y=masterdata$X2009uninsured, main="2009 State Uninsured Rates by Party", 
     xlab="Republican (0) vs Democrat (1) ", ylab="Uninsured Rate" )
 
#graphs

#means that democratic vote share was higher for states that opted in
plot(x=data$didoptin, y=sub1$vote2, main = "2016 Dem Vote Share vs Opting into Medicaid Expansion", 
     xlab="Opted Out (0) vs Opted In (1)", ylab="2016 Democratic Vote Share")

#doesn't violate conditions for regression 
plot(residuals(model_2012_included), main = "Residual Plot of Model with 2012 Election Data")
hist(residuals(model_2012_included), main = "Histogram of Residual Plot of Model with 2012 Election Data")


plot(residuals(model_wo_2012), main = "Residual Plot of Model without 2012 Election Data")
hist(residuals(model_wo_2012), main = "Histogram of Residual Plot of Model without 2012 Election Data")

#change in uninsured rate vs party
data$changeuninsured = data$X2019.uninsured-data$X2009uninsured

plot(x=data$voteddem, y=data$changeuninsured, main="Decrease in Uninsured Rates by Party", 
     xlab="Republican (0) vs Democrat (1) ", ylab="Uninsured Rate" )
#table of how each state voted vs vote share

plot(x=data$voteddem, y=data$InflationAdjustedACAsubsidy, main = "Insurance Subsidy Spending by Party", xlab = "Republican (0), Democrat(1)")

summary(model_votedem_V_medicaid_subsidy_opted_in)

exp(coef(model_votedem_V_medicaid_subsidy_opted_in))

plot(model_votedem_V_medicaid_subsidy_opted_in)

plot(residuals(model_votedem_V_medicaid_subsidy_opted_in))

hist(data$X2009uninsured[data$VotedDem.=="No"])
hist(data$X2009uninsured[data$VotedDem.=="Yes"])

summary(data$X2009uninsured[data$VotedDem.=="No"])

summary(data$X2009uninsured[data$VotedDem.=="Yes"])


summary(data$X2019.uninsured[data$VotedDem.=="No"])
summary(data$X2019.uninsured[data$VotedDem.=="Yes"])


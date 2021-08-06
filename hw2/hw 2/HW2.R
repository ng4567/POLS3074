setwd("/Users/nikhilgopal/Google Drive/Notability/Research and Design_ Data Analysis/hw 2")
library(dplyr)
library(plyr)

data_TA <- read.csv("STAR.csv")

#take the mean of race 1, but remove the NULL values so error isn't thrown
mean(data_TA$race==1, na.rm = TRUE)

tapply(data_TA$race, data_TA$g4reading, mean)

#mean per g4 reading score race with NULL values removed
tapply(data_TA$g4reading, data_TA$race, mean, na.rm=TRUE)

#table of counts 
table(data_TA$race)

#create a new column and assign White to race == 1 and other for everything else
data_TA$raceword <- ifelse(data_TA$race == 1, "white", "other")

#turn race into a factor object type
data_TA$race <- as.factor(data_TA$race)

#revalue race column with the assigned values as below:
data_TA$race <- revalue(data_TA$race, c("1"="white", "2"="black", "3" = ""))

prop.table(table(data_TA$race))


#2.8.1.1 
#create a new classtype kinder that recodes classtype column
data_TA$kinder <- NA

#assign values to the kinder column based on values of the classtype column as detailed below:
data_TA$kinder[data_TA$classtype == 1] <- "small" 
data_TA$kinder[data_TA$classtype == 2] <- "regular"
data_TA$kinder[data_TA$classtype == 3] <- "regular with aid" 

## turn kinder variable into factor type
data_TA$kinder <- as.factor(data_TA$kinder)

#turn race into a factor object type
data_TA$race <- as.factor(data_TA$race)

#revalue race column with the assigned 1 = white, 2 = black,, 3/5/6 = others, 4 = hispanic
data_TA$race <- revalue(data_TA$race, c("1"="white", "2"="black", "3" = "others", "4" = "Hispanic", "5" = "others", "6" = "others"))

#2.8.1.2


#mean reading score by class type w NULL values removed 
tapply(data_TA$g4reading, data_TA$kinder, mean, na.rm=TRUE)
#standard deviations of class types
tapply(data_TA$g4reading, data_TA$kinder, sd, na.rm=TRUE)


#mean math score by class type w NULL values removed 
tapply(data_TA$g4math, data_TA$kinder, mean, na.rm=TRUE)
#standard deviations of class types
tapply(data_TA$g4math, data_TA$kinder, sd, na.rm=TRUE)


#number of observations by class type
count(data_TA$kinder)

#2.8.1.3

#subset data
smallclasses <- subset(data_TA, data_TA$kinder == "small")
regularclasses <- subset(data_TA, data_TA$kinder == "regular")

#reading/math quantiles of small classes
smallreading <- quantile(smallclasses$g4reading, probs = seq(0.33, 0.66, 0.33), na.rm = TRUE)
smallmath <- quantile(smallclasses$g4math, probs = seq(0.33, 0.66, 0.33), na.rm = TRUE)

#reading/math quantiles of regular classes
regularreading <- quantile(regularclasses$g4reading, probs = seq(0.33, 0.66, 0.33), na.rm = TRUE)
regularmath <- quantile(regularclasses$g4math, probs = seq(0.33, 0.66, 0.33), na.rm = TRUE)

smallreading
smallmath
regularreading
regularmath

#2.8.1.4

prop.table(table(data_TA$kinder, data_TA$yearssmall))

#reading scores mean/median
tapply(data_TA$g4reading, data_TA$yearssmall, mean, na.rm = TRUE)
tapply(data_TA$g4reading, data_TA$yearssmall, median, na.rm = TRUE)

#math scores mean/median
tapply(data_TA$g4math, data_TA$yearssmall, median, na.rm = TRUE)
tapply(data_TA$g4math, data_TA$yearssmall, median, na.rm = TRUE)

#number of students who had small class sizes for a given # of years
count(data_TA$yearssmall)

#2.8.1.5

#subset the data to get info for only white and minority students

white <- subset(data_TA, data_TA$race=="white")
minority <- subset(data_TA, data_TA$race=="black" | data_TA$race=="Hispanic")

#White vs minority math scores by class type
tapply(white$g4math, white$kinder, mean, na.rm=TRUE)
tapply(minority$g4math, minority$kinder, mean, na.rm=TRUE)

#White vs minority reading scores by class type
tapply(white$g4reading, white$kinder, mean, na.rm=TRUE)
tapply(minority$g4reading, minority$kinder, mean, na.rm=TRUE)

#2.8.1.6

#high school graduation rates by class size
tapply(data_TA$hsgrad, data_TA$kinder, mean, na.rm = TRUE)

#high school graduation rates by #of years spent in small classes
tapply(data_TA$hsgrad, data_TA$yearssmall, mean, na.rm = TRUE)

#high school graduation rates by race
tapply(data_TA$hsgrad, data_TA$race, mean, na.rm = TRUE)

#2.8.3.1

leaders <- read.csv("leaders.csv")

#how many assasination attempts
dim(leaders)

#how many countries are there
length(unique(leaders$country))

#avg attempts per year
nrow(leaders)/length(unique(leaders$country))


#2.8.3.2

#create a success column in the dataframe, assign a value of 1 if the leader died and 0 if he didn't
leaders$success <- NA

leaders$success[leaders$result == "dies between a day and a week" 
               | leaders$result == "dies between a week and a month" 
               | leaders$result == "dies within a day after the attack"
               | leaders$result == "dies, timing unknown"] <- 1

leaders$success[leaders$result == "hospitalization but no permanent disability" 
               | leaders$result == "not wounded"
               | leaders$result == "plot stopped"
               | leaders$result == "survives but wounded severely"
               | leaders$result == "survives, whether wounded unknown"
               | leaders$result == "wounded lightly"] <- 0

#mean success rate of leader assassination
mean(leaders$success)

#2.8.3.3

#mean polity score before assasination attempt
mean(leaders$politybefore)

#mean polity score after
mean(leaders$polityafter)

#Avg polity score before assasination attempt for successful and unsuccessful leaders
tapply(leaders$politybefore, leaders$success, mean)

#Avg polity score after assasination attempt for successful and unsuccessful attempts
tapply(leaders$age, leaders$success, mean)

#2.8.3.4

#create a new column in the dataframe
leaders$warbefore <- NA

#create a new column in the dataframe
leaders$warbefore <- NA

#set value of col to 1 if there was a war before assassination attempt
leaders$warbefore[leaders$civilwarbefore == 1 | leaders$interwarbefore == 1] <- 1

#set value of col to 0 of no war before assassination attempt
leaders$warbefore[leaders$civilwarbefore != 1 & leaders$interwarbefore != 1] <- 0

#subset data, only capture data with war before attempt
warbeforecol <- subset(leaders, leaders$warbefore == 1)

#avg polity before assassination attempt of leaders in countries with war before by success
tapply(warbeforecol$politybefore, warbeforecol$success, mean)

#avg age of leaders in countries with war b4 attempt by success
tapply(warbeforecol$age, warbeforecol$success, mean)

#2.8.3.5

#create new column (need to because warbefore = 0, doesn't mean there was a war after)
leaders$warbefore <- NA

#assign values to the column

leaders$warafter[leaders$civilwarafter == 1 | leaders$interwarafter == 1] <- 1

leaders$warafter[leaders$civilwarafter != 1 & leaders$interwarafter != 1] <- 0

#subset data for warafter column
waraftercol <- subset(leaders, leaders$warafter == 1)

#average rate of war after assassination attempt for successful vs unsuccessful assasination attempts
tapply(leaders$warafter, leaders$success, mean)

#average polity after war for successful/unsuccessful attempts
tapply(leaders$polityafter, leaders$success, mean)





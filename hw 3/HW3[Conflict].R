setwd("/Users/nikhilgopal/Google Drive/Notability/Research and Design_ Data Analysis/hw 3")
data <- read.csv("unvoting.csv")

#subset data
un1980 <- subset(data, Year==1980) 
un2000 <- subset(data, Year==2000)

#1980 histogram and quantiles
hist(un1980$idealpoint, xlab = "Ideal Point Score", main = "Distribution of Ideal scores in 1980")
quantile(un1980$idealpoint)
 #add median to the plot
abline(v = median(un1980$idealpoint), col = "blue")


#2000 histogram and quantiles
hist(un2000$idealpoint, xlab = "Ideal Point Score", main = "Distribution of Ideal scores in 2000")
abline(v = median(un1980$idealpoint), col = "yellow")
quantile(un2000$idealpoint)


#Question 2
#obtain a unique list of each year
years <- unique(data$Year) 

#obtain average % agreement per year
avgpctagreeUSA = tapply(data$PctAgreeUS, data$Year, mean, na.rm = TRUE)
avgpctagreeRussia = tapply(data$PctAgreeRUSSIA, data$Year, mean, na.rm = TRUE)
#plot data, type = l changes from dots to line
plot(x = years, avgpctagreeUSA, xlab = "Year", ylab = "Avg % Agreement W/ US", main = "Yearly Avg % Agreement W/ US", type = "l",col = "blue")

setwd("C:/Users/d/Google Drive/Notability/Research and Design_ Data Analysis/hw 3")
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

#get the avg agreement w/USA for each year
pctagreeUSA <- tapply(data$PctAgreeUS, data$Year, mean, na.rm = TRUE)
#get the unique years to, store it for graphing later
unique_years <- unique(data$Year)

plot(unique_years, pctagreeUSA, xlab = "Year", ylab = "% Agreement w/USA", type = 'l')

#agreement for Russia
pctagreeRussia <- tapply(data$PctAgreeRUSSIA, data$Year, mean, na.rm = TRUE)

#add Russia to the graph
lines(unique_years, pctagreeRussia, type = "l", col = "blue")

#avg agreement w/US + Russia by country
agreement_USA_by_country <- tapply(data$PctAgreeUS, data$CountryName, mean, na.rm = TRUE)
agreement_Russia_by_country <- tapply(data$PctAgreeRUSSIA, data$CountryName, mean, na.rm = TRUE)

print(sort(agreement_Russia_by_country[1:5]))
print(tail(sort(agreement_USA_by_country), n = 5))

#Question 3

plot(unique_years, data$idealpoint[data$CountryAbb == "USA"], xlab = "Year", ylab = "Ideal Points", type = 'l', main = "Russian/USA Ideal Score Over Time")
#add Russia to the graph
lines(unique_years, data$idealpoint[data$CountryAbb == "RUS"], col = "blue")

#add median of all countries ideal points to the graph
lines(unique_years, tapply(data$idealpoint, data$Year, median)) 

#Question 4

#vector containing each country
postUSSRcountries <- c("Estonia", "Latvia", "Lithuania", "Belarus", "Moldova", "Ukraine", "Armenia", "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan", "Russia")


#subset to only data in year 2012
data2012 <- subset(data, Year == 2012)
#subset again for only post soviet countries
data2012postUSSR <- subset(data2012, CountryName %in% postUSSRcountries)
plot(data2012$idealpoint[data2012$CountryName %in% postUSSRcountries], data2012postUSSR$PctAgreeUS, pch = 24, xlim = c(-3, 3), ylim = c(0, 0.6), col = "red", 
     xlab = "Ideal Points", ylab = "Proportion of Votes Agreeing with US", main = "Ideal Points vs %Votes agreeing w/USA")

#vector with all country names
all_country_names <- unique(data2012$CountryName)
#empty vector to hold all country names minus the USSR country names
all_countries_not_post_USSR <- c()

#for loop thru vector, add the countries that aren't post USSR countries to new vector
for (country in all_country_names) {
  if(country %in% postUSSRcountries){
  }else{
    all_countries_not_post_USSR <- c(all_countries_not_post_USSR, country)
  }
}

#add other countries to the graph, need to do the above steps so that points aren't plotted twice
points(data2012$idealpoint[data2012$CountryName %in% all_countries_not_post_USSR], data2012$PctAgreeUS[data2012$CountryName %in% all_countries_not_post_USSR], pch = 12, col = "blue") 

#Question 5

#plot median ideal score by year for all countries except post soviet ones
plot(unique_years, 
     tapply(data$idealpoint[data$CountryName %in% all_countries_not_post_USSR], 
            data$Year[data$CountryName %in% all_countries_not_post_USSR], 
            median, na.rm = TRUE), 
     type = "l", ylim = c(-3, 3), xlab = "Year", 
     ylab = "Median Ideal Point", col = "green",
     main = "Median Yearly Ideal Point Score Post Soviet vs All Other Countries") 

#add post soviet countries to the graph
lines(unique_years, tapply(data$idealpoint[data$CountryName %in% postUSSRcountries], data$Year[data$CountryName %in% postUSSRcountries], median, na.rm = TRUE), col = "blue")

#add line at 1989 to indicate fall of Berlin wall
abline(v=1989)


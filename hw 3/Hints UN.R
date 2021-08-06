

rm(list = ls())
un <- read.csv("~/Dropbox/TA_folder/Assignments/Assignment3/unvoting.csv")

un1980 <- subset(un, subset = (Year == 1980))
un2000 <- subset(un, subset = (Year == 2000))


# Sort and tapply Q2

# Of variable pctagreed, by country, give me the mean. Then, in decreasing order, first 10, excludnig 
# US and Russia.

tapply(un$PctAgreeUS, un$CountryName, mean, na.rm = TRUE)

sort(tapply(un$PctAgreeUS, un$CountryName, mean, na.rm = TRUE), 
     decreasing = TRUE)[2:11] # first one is US itself, then countries with mos agreement.
sort(tapply(un$PctAgreeRUSSIA, un$CountryName, mean, na.rm = TRUE), 
     decreasing = TRUE)[2:11] # first one is Russia itself

# In operator Q4

# Example

list1 <- c("a", "b", "c", "d")
list2 <- c("a", "c")

match <- ifelse(list1 %in% list2, 1,0)
match

# Use it for for the former soviet countries. If list 2 matches list 1, then 1, 0 otherwise.

post.soviet <- 
  c("Estonia", "Latvia", "Lithuania", "Belarus", "Moldova", "Ukraine", 
    "Armenia", "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", 
    "Tajikistan", "Turkmenistan", "Uzbekistan", "Russia")

un$post.soviet <- ifelse(fill here)
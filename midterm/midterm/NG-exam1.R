setwd("C:/Users/d/Google Drive/Notability/Research and Design_ Data Analysis/midterm")

polls <- read.csv("polls_2020.csv")
pres <- read.csv("vote_2020.csv")

polls$daysleft <- as.Date("2020-11-3")-as.Date(polls$middate)

pres$margin <- pres$trump - pres$biden

#Question 1.a.i Enter command to create variable for the poll margin here

polls$margin <- polls$trump - polls$biden

st.names <- unique(sort(polls$state))
poll.pred <- rep(NA, 51)

for(i in 1:51) {
  state.data <- subset(polls, subset = (state == st.names[i]))
  latest <- state.data$daysleft == min(state.data$daysleft)
  poll.pred[i] <- mean(state.data$margin[latest])
}

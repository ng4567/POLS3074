#Question 1

data = read.csv("progresa.csv")

treatment_average_turnout = mean(data$t2000[data$treatment==1])
control_average_turnout = mean(data$t2000[data$treatment==0])

treatement_average_support = mean(data$pri2000s[data$treatment==1])
control_average_support = mean(data$pri2000s[data$treatment==0])


treatement_average_pri_vote_share = mean(data$pri2000v[data$treatment==1])
control_average_pri_vote_share = mean(data$pri2000v[data$treatment==0])


regression_turnout = lm(data$t2000~data$treatment)
regression_votes_as_share_of_voting_pop = lm(data$pri2000s~data$treatment)

summary(regression_turnout)
summary(regression_votes_as_share_of_voting_pop)

#Question 2

regression_turnout_using_original_model = lm(t2000~treatment +avgpoverty+pobtot1994+votos1994+pri1994+pan1994+prd1994, data = data)
regression_support_using_original_model = lm(pri2000s~treatment +avgpoverty+pobtot1994+votos1994+pri1994+pan1994+prd1994, data = data)

summary(regression_turnout_using_original_model)
summary(regression_support_using_original_model)


#Question 3
natural_turnout_model = lm(t2000~treatment+log(pobtot1994)+t1994+pri1994s+pan1994s+prd1994s+avgpoverty, data = data)
natural_support_model = lm(pri2000s~treatment+log(pobtot1994)+t1994+pri1994s+pan1994s+prd1994s+avgpoverty, data = data)

summary(natural_turnout_model)
summary(natural_support_model)

#Question 4

#Total population
boxplot(pobtot1994~treatment, data = data, ylab = "Total 1994 Precinct Population")
#Avg Poverty
boxplot(avgpoverty~treatment, data = data, ylab = "Average Poverty")
#Previous Turnout Rate
boxplot(t1994~treatment, data = data, ylab = "Previous Turnout Rate as Share of Voting Population")
#Previous PRI support rate
boxplot(pri1994s~treatment, data=data, ylab = "Previous PRI Support Rate")

#Question 5

turnout_rate_as_outcome = lm(t2000r ~ treatment + avgpoverty + log(pobtot1994) + t1994r + pri1994v + pan1994v + prd1994v, data = data)
summary(turnout_rate_as_outcome)

votes_cast_as_outcome = lm(pri2000v ~ treatment + avgpoverty + log(pobtot1994) + t1994r + pri1994v + pan1994v + prd1994v, data = data)
summary(votes_cast_as_outcome)


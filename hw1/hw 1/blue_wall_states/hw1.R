setwd("C:/Users/d/Google Drive/Notability/Research and Design_ Data Analysis/hw 1/blue_wall_states")

#Q1

#load data
dat_ny <- read.csv('pres_vote_NY.csv')
data_WI <- read.csv('pres_vote_WI.csv')
data_IL <- read.csv('pres_vote_IL.csv')

#dimensions
dim(data_IL)
dim(data_WI)

#Q2

#total votes for dems/republicans
total_2p_WI <- data_WI$vote_D + data_WI$vote_R
total_2p_IL <- data_IL$vote_D + data_IL$vote_R

#This is how to add within the dataframe
data_WI <- cbind(data_WI, TotalDemRepVotes=data_WI$vote_D+data_WI$vote_R)
data_IL <- cbind(data_IL, TotalDemRepVotes=data_IL$vote_D+data_IL$vote_R)

#Democratic share of vote
dem_share_WI <- data_WI$vote_D/total_2p_WI
dem_share_IL <- data_IL$vote_D/total_2p_IL


#dem share of total votes
dem_share_total_votes_WI <- data_WI$vote_D/data_WI$vote_total
dem_share_total_votes_IL <- data_IL$vote_D/data_IL$vote_total



#Q3

#1976-1988
WI76_88 <- data_WI[1:4, ]
IL76_88 <- data_IL[1:4, ]
#1992-2012
WI92_12 <- data_WI[6:10, ]
IL92_12 <- data_IL[6:10, ]

#mean
mean_WI_76_88 <- mean(WI76_88$vote_D/(WI76_88$vote_D+WI76_88$vote_R))
mean_IL_76_88 <- mean(IL76_88$vote_D/(IL76_88$vote_D+IL76_88$vote_R))


mean_WI_92_12 <- mean(WI92_12$vote_D/(WI92_12$vote_D+WI92_12$vote_R))
mean_IL_92_12 <- mean(IL92_12$vote_D/(IL92_12$vote_D+IL92_12$vote_R))

#difference
difference_WI <- mean_WI_92_12 - mean_WI_76_88
difference_IL <- mean_IL_92_12 - mean_IL_76_88

#Question 4

difference_in_vote_share <- dem_share_total_votes_WI - dem_share_total_votes_IL
mean_vote_share_difference_76_88 <- mean(difference_in_vote_share[1:4])
mean_vote_share_difference_92_12 <- mean(difference_in_vote_share[5:10])














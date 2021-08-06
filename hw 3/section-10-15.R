setwd("C:/Users/d/Google Drive/Notability/Research and Design_ Data Analysis/hw 3")

trans <- read.csv("transphobia.csv")

#Question 1

#method 1
t1 <- mean(trans$tolerance.t0[trans$treat_ind==1], na.rm = TRUE)
t2 <- mean(trans$tolerance.t1[trans$treat_ind==1], na.rm = TRUE)
t3 <- mean(trans$tolerance.t2[trans$treat_ind==1], na.rm = TRUE)
t4 <- mean(trans$tolerance.t3[trans$treat_ind==1], na.rm = TRUE)
t5 <- mean(trans$tolerance.t4[trans$treat_ind==1], na.rm = TRUE)


#control group
c1 <- mean(trans$tolerance.t0[trans$treat_ind==0], na.rm = TRUE)
c2 <- mean(trans$tolerance.t1[trans$treat_ind==0], na.rm = TRUE)
c3 <- mean(trans$tolerance.t2[trans$treat_ind==0], na.rm = TRUE)
c4 <- mean(trans$tolerance.t3[trans$treat_ind==0], na.rm = TRUE)
c5 <- mean(trans$tolerance.t4[trans$treat_ind==0], na.rm = TRUE)

treats <- c(t1, t2, t3, t4, t5)
conts <- c(c1,c2,c3,c4,c5)
effects <- treats-controls

weeks <- c(0, 0.5,3,6,12)

plot(weeks, treats, main="Tolerance Levels Over Time", type = "b", pch = 19, ylab = "Tolerance Level")
lines(weeks, conts, type='b', ylim=c(-.1, .4))

text(4, 0.05, "placebo group")
text(8, 0.2, "treatment group")

#give me all the rows of treated people, and all the columns
treated <- trans[trans$treat_ind==1,]

#use every row and only columns 5:9
cor(treated[,5:9], use = "complete.obs")

controlled <- trans[trans$treat_ind==0,]

#correlation for control group
cor(controlled[,5:9], use = "complete.obs")

#Question 3

dem <- trans[trans$vf_party == "D",]
rep <- trans[trans$vf_party == "R",]
ind <- trans[trans$vf_party=="N",]


trans_p0 <- tapply(trans$tolerance.t0[trans$treat_ind==0], trans$vf_party[trans$treat_ind==0], mean, na.rm =TRUE)
trans_p1 <- tapply(trans$tolerance.t1[trans$treat_ind==0], trans$vf_party[trans$treat_ind==0], mean, na.rm =TRUE)
trans_p2 <- tapply(trans$tolerance.t2[trans$treat_ind==0], trans$vf_party[trans$treat_ind==0], mean, na.rm =TRUE)
trans_p3 <- tapply(trans$tolerance.t3[trans$treat_ind==0], trans$vf_party[trans$treat_ind==0], mean, na.rm =TRUE)
trans_p4 <- tapply(trans$tolerance.t4[trans$treat_ind==0], trans$vf_party[trans$treat_ind==0], mean, na.rm =TRUE)

trans_tp0 <- tapply(trans$tolerance.t0[trans$treat_ind==1], trans$vf_party[trans$treat_ind==1], mean, na.rm =TRUE)
trans_tp1 <- tapply(trans$tolerance.t1[trans$treat_ind==1], trans$vf_party[trans$treat_ind==1], mean, na.rm =TRUE)
trans_tp2 <- tapply(trans$tolerance.t2[trans$treat_ind==1], trans$vf_party[trans$treat_ind==1], mean, na.rm =TRUE)
trans_tp3 <- tapply(trans$tolerance.t3[trans$treat_ind==1], trans$vf_party[trans$treat_ind==1], mean, na.rm =TRUE)
trans_tp4 <- tapply(trans$tolerance.t4[trans$treat_ind==1], trans$vf_party[trans$treat_ind==1], mean, na.rm =TRUE)

avg_p <- c(trans_p0-trans_tp0, trans_p1-trans_tp1, trans_p2-trans_tp2, trans_p3-trans_tp3, trans_p4-trans_tp4)

#dem effects
d0 <- trans_tp0[1] - trans_p0[1]
d1 <- trans_tp1[1] - trans_p1[1]
d2 <- trans_tp2[1] - trans_p2[1]
d3 <- trans_tp3[1] - trans_p3[1]
d4 <- trans_tp4[1] - trans_p4[1]

#republican effects
r0 <- trans_tp0[3] - trans_p0[3]
r1 <- trans_tp1[3] - trans_p1[3]
r2 <- trans_tp2[3] - trans_p2[3]
r3 <- trans_tp3[3] - trans_p3[3]
r4 <- trans_tp4[3] - trans_p4[3]


dem_effects <- c(d0,d1,d2,d3,d4)

rep_effects <- c(r0,r1,r2,r3,r4)

plot(weeks, rep_effects, pch =2, col = 'red')
lines(weeks, dem_effects, pch =5, col = "blue")
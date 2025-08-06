#####################################
# Author: Evan Whitfield
# Date Last Edit: 1-24-25
# Purpose: To answer problems for ST503 HW2
#####################################
#
# Problem 1
#
#################################
library(estimability)

#Define model matrix
X <- cbind(rep(1,6), 
           c(rep(1, 3),rep(0,3)),
           c(rep(0, 3),rep(1,3)),
           c(1,0,0,1,0,0),
           c(0,1,0,0,1,0),
           c(0,0,1,0,0,1))

#coefficient vectors
cvec1 <- c(0, 1, -1, 0, 0, 0)
cvec2 <- c(0, 0, 0, 1, -2, 1)
nb <- nonest.basis(X)

#checking estimability
is.estble(cvec1,nb)
is.estble(cvec2,nb)


#################################
#
# Problem 2
#
#################################

library(faraway)

#Fit data from teengamb with gamble as response variable
out <- lm(gamble ~ ., data = teengamb)
out$coefficients
summary(out)

#Determining statistics for the residuals
mean(out$residuals)
median(out$residuals)
max(out$residuals)
out$residuals

#Finding correlation between the residuals and the fitted values
cor(out$residuals,out$fitted.values)

#Finding correlation between residuals and income variable
cor(out$residuals,teengamb$income)

#Sub-setting the data based on gender
female_data <- teengamb[teengamb$sex == 1, ]
male_data <- teengamb[teengamb$sex == 0, ]

#Determing linear model for each gender
male_out <- lm(gamble ~ ., data = male_data)
female_out <- lm(gamble ~ ., data = female_data)

#Calculating mean(expected) fitted value
mean_males <- mean(male_out$fitted.values)
mean_females <- mean(female_out$fitted.values)

#calculating the difference beween the expected values of each gender
diff <- mean_males - mean_females
diff

############################################
#
# Problem 3
#
############################################

out_wages_1 <- lm(wage ~ educ + exper, data = uswages)
out_wages_1$coefficients

out_wages_2 <- lm(log(wage) ~ educ + exper, data = uswages)
out_wages_2$coefficients
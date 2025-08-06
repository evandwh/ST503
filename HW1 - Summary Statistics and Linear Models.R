#open faraway library
library(faraway)

#summary stats for each variable
summary(teengamb)

#histogram of gamble and income
hist(teengamb$gamble, freq = FALSE,
     main = "Histogram of Gamble", xlab = "Gamble")
lines(density(teengamb$gamble))

hist(teengamb$income, freq = FALSE,
     main = "Histogram of Income", xlab = "Income")
lines(density(teengamb$income))

#scatterplots for all variables except gender which was just 0s and 1s
pairs(teengamb[,c(2,3,4,5)], pch = 19)

#Getting linear fit for gamble vs. income
out <- lm(gamble ~ income, data=teengamb)
out$coefficients

#plotting gamble vs income with the regression line
plot(teengamb$income, teengamb$gamble,
     xlab = "Income", ylab = "Gamble",
     pch = 19)
abline(out, col = "red", lwd = 2)

#looking at summary of gamble and income variables based on gender
sapply(split(teengamb$income, teengamb$sex), mean)
sapply(split(teengamb$income, teengamb$sex), sd)
sapply(split(teengamb$gamble, teengamb$sex), mean)
sapply(split(teengamb$gamble, teengamb$sex), sd)



# Fitting a linear model for each gender separately
model_male <- lm(gamble ~ income, data = subset(teengamb, sex == 0))
model_female <- lm(gamble ~ income, data = subset(teengamb, sex == 1))

model_male$coefficients
model_female$coefficients


#plotting by color
plot(teengamb$income, teengamb$gamble, col = ifelse(teengamb$sex == 0, "blue", "pink"),
     xlab = "Income", ylab = "Gamble", main = "Scatterplot of Gamble vs Income by Gender", pch = 19)

abline(model_male, col = "lightblue", lwd = 3)
abline(model_female, col = "red", lwd = 3)



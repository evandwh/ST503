#################################
# Author: Evan Whitfield
# Date: 3/9/2025
# Purpose: For HW5 in ST503
#################################


#install package(s) and necessary libraries
install.packages("clubSandwich")
library(dplyr)
library(ggplot2)
library(magrittr)
library(nlme)
library(clubSandwich)

#read in data
dental.data=read.table("dental.txt",header=F)
names(dental.data) <- c("obs","child","age","distance","gender")
dental.data$gender <- factor(dental.data$gender)
head(dental.data)

#Plot the data
ggplot(dental.data) +
  geom_line(aes(age, distance, group = child,col = gender))

#Find the mean response at each age for each gender
agg <- dental.data %>%
  mutate(gender = factor(gender)) %>%
  group_by(gender, age) %>%
  summarise(mean_distance = mean(distance)) %>%
  as.data.frame

#Model
meanform <- distance ~ age + gender + age:gender

#Equal Error Variance
gls.fit.exch <- gls(model = meanform,
                    data = dental.data,
                    correlation = corCompSymm(form = ~1|child))
summary(gls.fit.exch)

cf <- gls.fit.exch$coefficients
round(cf, 3)

#Plot data with model
ggplot(agg) +
  geom_line(aes(age, mean_distance,
                group = gender, col = gender),
            lwd = 1) +
  geom_point(aes(age, mean_distance,
                 group = gender, col = gender),
             cex = 2) +
  geom_abline(intercept = cf[1], slope = cf[2]) +
  geom_abline(intercept = cf[1] + cf[3], slope = cf[2] + cf[4])

#Model for unequal error variance
gls.cs.uv <- gls(model = meanform,
                 data = dental.data,
                 correlation = corCompSymm(form= ~ 1 | child ),
                 weights = varIdent(form = ~ 1|age))
summary(gls.cs.uv)

#Getting the variance
Sigma <- getVarCov(gls.fit.exch, individual = 1)
round(Sigma, 2)

betahat <- gls.fit.exch$coefficients

# Robust covariance matrix and se of betahat
V.robust <- vcovCR(gls.fit.exch, type = "CR0")
se.robust <- sqrt(diag(V.robust))

# df
df <- nrow(gender) - length(betahat)
# t stats

t.robust <- betahat/se.robust
# p-values

p.value <- round(2*pt(q = abs(t.robust),
                      df = df,
                      lower.tail = FALSE), 4)
# results
tab <- data.frame(betahat, se.robust, t.robust, p.value)
tab

#Part e - testing whether mean trends have same rate of change
# L matrix
L <- matrix(c(0, 0, 0, 1), nrow = 1)
cc <- nrow(L)
df <- nrow(dental.data) - length(betahat)

# estimate and covariance matrix of L\beta
est <- L %*% betahat
varmat <- L %*% V.robust %*% t(L)

# Wald test
Wald <- c( t(est) %*% solve(varmat) %*% (est) )
p.value <- pchisq(q = Wald, df = cc, lower.tail=FALSE)
data.frame(Wald, p.value)


# Part f - testing mean trends 
# L matrix
L <- rbind(c(0, 0, 1, 0), c(0, 0, 0, 1))
cc <- nrow(L)
df <- nrow(dental.data) - length(betahat)

# estimate and covariance matrix of L\beta
est <- L %*% betahat
varmat <- L %*% V.robust %*% t(L)

# Wald test
Wald <- c( t(est) %*% solve(varmat) %*% (est) )
p.value <- pchisq(q = Wald, df = cc, lower.tail=FALSE)
data.frame(Wald, p.value)

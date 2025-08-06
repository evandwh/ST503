#############################
# Author: Evan Whitfield
# Date: 2-15-25
# Purpose: To answer HW4
#############################

#############################
#
# Problem 2
#
##############################

library(faraway)

head(sat)

#fitting the data
out <- lm(total ~ . - verbal - math, data = sat)
res <- resid(out)
fitted <- fitted(out)
res_std <- rstandard(out)

#plotting the residuals
par(mfrow = c(1,3), mar = c(4,4,0,2))
plot(fitted, res_std,
     xlab = "Fitted values",
     ylab = "Standardized Residuals", pch =19)
plot(fitted, abs(res_std),
     xlab = "Fitted values",
     ylab = "|Standardized Residuals|",
     pch =19)
plot(fitted, sqrt(abs(res_std)),
     xlab = "Fitted values",
     ylab = "sqrt{|Standardized Residuals|}",
     pch =19)

#plotting the residuals vs variables
par(mfrow=c(1,4), mar = c(4,4,0,2))
names <- c("expend", "ratio", "salary", "takers")
for(nm in names){
  plot(sat[[nm]], res_std, xlab = nm, ylab = "Std. residuals", pch = 19)
}


#checking normality of residuals
par(mfrow = c(1,2))
hist(rstandard(out), probability = TRUE)
curve(dnorm(x), to = 3, from = -3, add = TRUE)
qqnorm(rstandard(out), pch = 19)
abline(a = 0, b = 1)
shapiro.test(rstandard(out))

par(mfrow = c(1,1))
# leverage
lv <- hatvalues(out)
# with highest leverage
halfnorm(lv, nlab = 4)
# 3 times average of leverages
abline(h = 3*mean(lv))
# 2 times average of leverages
abline(h = 2*mean(lv))
# Identify high leverage points
which(lv >= 3*mean(lv))
# Identify moderately high leverage points
which(lv >= 2*mean(lv))


#Serial Correlation
library(lmtest)
dwtest(total ~ . - verbal - math, data = sat)

plot(resid(out), ylab = "Residuals", pch=19, type="b")
abline(h=0)



#####################################
#
# Problem 3
#
#####################################


# Simulate the data
n <- 1000
X <- runif(n, 0, 10)
e <- rnorm(n, mean = 0, sd = 1)
Y_true <- X^2 + e

# Fit a linear model 
out_linear <- lm(Y_true ~ X)

# Get the fitted values and residuals
Y_predicted <- out_linear$fitted.values
residuals <- out_linear$residuals

# Plot data
plot(X, Y_true, pch = 19,
     xlab = "X", ylab = "True Y Values")
abline(out_linear, col = "red", lwd = 3)

#Plot residual plot
plot(Y_predicted, residuals, 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

#Achieving Linearity
out_linearized <- lm(sqrt(abs(Y_true)) ~ X)
plot(X, out_linearized$fitted.values,
     ylab = "Transformed Y Value")
abline(out_linearized, col = "red", lwd = 2)

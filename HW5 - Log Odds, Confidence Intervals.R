library(faraway)
library(lmtest)

#Getting Data
wcgs_small <- data.frame(height = wcgs$height, 
                        cigs = wcgs$cigs, 
                        chd = ifelse(wcgs$chd == "yes", 1, 0))

#Plot
plot(wcgs_small$cigs, wcgs_small$height,
     col = wcgs_small$chd+1, pch = wcgs_small$chd+10, 
     xlab = "cigs", ylab = "height")

#Linear Model
out <- lm(chd ~ ., data = wcgs_small)
head(out$fitted.values)

#GLM Full
wcgs_glm <- glm(chd ~ ., data = wcgs_small, family = binomial())
wcgs_glm

#Reduced Model
wcgs_red <- glm(chd ~ cigs, data = wcgs_small, family = binomial())

#LRT
lrtest(wcgs_red, wcgs_glm)

#confidence interval
confint(wcgs_glm)

#log odds
alpha = 0.05
zcrit <- qnorm(alpha/2, lower.tail = FALSE)
newx <- data.frame(height = 70, cigs = 12)
lds <- predict(wcgs_glm, newdata = newx,
               type = "link", se.fit = TRUE)
logodds <- data.frame(estimate = lds$fit,
                      Lower = lds$fit - zcrit*lds$se.fit,
                      Upper = lds$fit + zcrit*lds$se.fit)
logodds

## Odds
odds <- exp(logodds)
odds

## P(y = 1)
prob <- odds/(1 + odds)
prob


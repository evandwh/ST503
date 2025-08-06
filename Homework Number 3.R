library(faraway)

#Problem 1

out_full <- lm(gamble ~ sex + status + income + verbal, data = teengamb)
summary(out_full)
RSS_full <- sum(out_full$residuals^2)

out_red <- lm(gamble ~ income, data = teengamb)
summary(out_red)
RSS_red <- sum(out_red$residuals^2)

anova(out_full, out_red)

F <- ((RSS_red - RSS_full)/3) / (RSS_full/42)
F

p_value <- pf(F, lower.tail = FALSE, df1 = 3, df2 = 42)
p_value

#Problem 2

out_1 <- lm(total ~ expend + ratio + salary, data = sat)
out_2 <- lm(total ~ 1, data = sat)

anova(out_1, out_2)

out_3 <- lm(total ~ expend + ratio + salary + takers, data = sat)
summary(out_3)

#Problem 3

out_pro_1 <- lm(lpsa ~ lcavol, data = prostate)
summary(out_pro_1)

out_pro_2 <- lm(lpsa ~ lcavol + lweight, data = prostate)
summary(out_pro_2)

out_pro_3 <- lm(lpsa ~ lcavol + lweight + svi, data = prostate)
summary(out_pro_3)

out_pro_4 <- lm(lpsa ~ lcavol + lweight + svi + lbph, data = prostate)
summary(out_pro_4)

out_pro_5 <- lm(lpsa ~ lcavol + lweight + svi + lbph + age, data = prostate)
summary(out_pro_5)

out_pro_6 <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp, data = prostate)
summary(out_pro_6)

out_pro_7 <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45, 
                data = prostate)
summary(out_pro_7)

out_pro_8 <- lm(lpsa ~ lcavol + lweight + svi + 
                lbph + age + lcp + pgg45 + gleason, 
                data = prostate)
summary(out_pro_8)

RSE_list <- c(0.7875, 0.7506, 0.7168, 0.7108, 0.7073, 0.7102, 0.7048, 0.7084)
plot(RSE_list, type = "l")

R_squared_list <- c(0.5394, 0.5859, 0.6264, 0.6366, 0.6441, 0.6451,0.6544, 0.6548)

plot(R_squared_list, type = "l")
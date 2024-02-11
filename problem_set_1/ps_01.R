library(sandwich)
library(Metrics)
library(ggplot2)
library(plotly)
library(lmtest)
library(dplyr)
library(psych)


cps <- read.csv(file = "cps99_ps1.csv")

head(cps)

# 6
cps %>%
  select(ahe, yrseduc, female) %>%
  describe() %>%
  select(mean, sd)


# 7.a
reg_a <- lm(data = cps, formula = ahe ~ yrseduc)
resuls_a <- summary(reg_a)

round(resuls_a$coefficients["yrseduc", "Estimate"], 3)
# It means that if yrseduc raised by 1 then ahe would increase by that value
# I think that this value is rather small than large in economic sense,
# because payoff seems to be not very large in tems of earnings.



# 7.b
ggplotly(
  ggplot(data = cps, aes(x = yrseduc, y = ahe)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE)
  # geom_line(aes(y = reg_a$fitted.values)) # nolint
)


# 7.c & 7.d
round(digits = 3,
      x = cbind(
        coeftest(reg_a, level = 0.95, vcov. = vcovHC(reg_a)),
        confint(reg_a, level = 0.95, vcov. = vcovHC(reg_a))
      ))
# p-value << 5% -> H0 is rejected -> coef. of yrseduc is significant


# 7.e
round(summary(reg_a)$adj.r.squared, 3)
# it means that we are able to describe how 16% of ahe is formed using yrseduc


# 7.f
cor(cps$ahe, cps$yrseduc)

cor(cps$ahe, cps$yrseduc) ^ 2   # ~ R squared


# 7.h
rmse(cps$ahe, reg_a$fitted.values)
# this means by what value we usually deviate from the actual observation


# 8.
reg_b <- lm(data = cps, formula = ahe ~ female)


# 8.a
round(summary(reg_b)$coefficients["female", "Estimate"], 3)
# this means that if the respondent is female, her ahe is lower by 3.2 on average # nolint: line_length_linter.


# 8.b
# the test needed for this is the standard Student test
# no need for linearHypothesis
coeftest(reg_a, level = 0.95, vcov. = vcovHC(reg_a))["yrseduc", "Pr(>|t|)"]
# < 5% -> H0 is rejected -> coef. significantly larger differ from 0


# 8.c
t.test(
  cps %>% filter(female == 0) %>% select(ahe),
  cps %>% filter(female == 1) %>% select(ahe),
  alternative = "two.sided",
  var.equal = FALSE
)
# p-value << 5% -> H0 is rejected -> means are not equal -> wage gape is significant # nolint: line_length_linter.


# to check if means are calculated correctly:
means <- as.data.frame(cps %>% group_by(female) %>% summarise(mean(ahe)))
means

means[1, "mean(ahe)"] - means[2, "mean(ahe)"]
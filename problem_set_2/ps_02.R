library(lmtest)
library(sandwich)

setwd("problem_set_2/")

cps <- read.csv(file = "cps99_ps1.csv")

# 5.a?
reg <- lm(data = cps, formula = ahe ~ yrseduc)

# to cause omitted variable bias omitted variable should satisfy both:
# 1. have non-zero correlation with used regressors
cor(cps$female, reg$residuals)
# 2. explain residual
resid_reg <- lm(formula = reg$residuals ~ cps$female)
coeftest(resid_reg, vcov. = vcovHC(resid_reg, type = "HC1"))
# -> conclusion: gender causes omitted variable bias

# from "omitted variable bias formula" the bias is
cov(cps$yrseduc, reg$residuals) / var(cps$yrseduc)


# 5.b
# obviously is not even an ommited variable, because any person with basic understanding of the world would doubt any effect of alphabetical order of surname on earnings # nolint: line_length_linter.
# but even if there would be some effect of this variable on earnings, there is no chance that that would be correlated with years of education of the person


# 5.c
# it would definitely cause omitted variable bias, because it has been shown many times that one's native abilities in significant way determine years of education, so it would be a truly omitted variable that has fundamental causality effect. Even so, years of education are used by many economists as an instrumental variable when we can't estimate person's native abilities more precisely.


# 6.a,b
# To simplify the question, suppose that we're instested only in estimating the effect on earnings of college/university education (not school). In this case ideal randomized controlled experiment would be the following:
# We would need to take a large diversified sample with many different areas of living of people with the same years of school education and specify random number of years for each person that s/he has to spent on her/his education.
# Since experiment is ideal, we need to come up with the solution that would prevent people from not studying and etc. Since an additional year of education is expected to have different effect based on number of years spent on education before, we need to compare 0 years of college/university education with 1 year (control group for 1 year), (C. G. for 2 years) 1 with 2 and so on.
# We will compare earnings (suppose, 1 year after getting given number of education) people from the same schools with approximately same GPA (so there would be no cross-school comparison) that come to the same universities, because it would help us avoid any endogenity. To do that, we could simply create the linear regression using school and university and number of years spent on college/university education as categorical variables & school GPA as continuous variable. The level-k for categorical variable number of years spent on college/university education would be k years of education. Using level-0 as base level, we estimate all K levels of education via regression and then compare k level with k - 1 where k>0. By doing this, we would get list of effects on earnings of an additional k year of college/university education.


# 7.i
regi <- lm(data = cps, formula = ahe ~ yrseduc)


# 7.ii
regii <- lm(data = cps, formula = ahe ~ yrseduc + female)


# 7.a
regii_coeftest <- coeftest(regii, vcov. = vcovHC(regii, type = "HC1"))
regii_coeftest["yrseduc", "Estimate"]
# ceteris paribus increase of yrseduc by 1 increases ahe by this value


# 7.b
regii_coeftest["yrseduc", "Pr(>|t|)"]
# this value <5% -> reject H0 -> sex has significat effect on ahe
# hypothesis in everyday words: sex has no real effect on ahe


# 7.c
coeftest(regi)["yrseduc", "Estimate"] / coeftest(regii)["yrseduc", "Estimate"]
# -> extra small relative difference, because sex has almost no effect on years of eduaction:
cor(cps$yrseduc, cps$female)


# 8
reg <- lm(data = cps, formula = ahe ~ yrseduc + ba)
confint(reg, vcov. = vcovHC(reg, type = "HC1"))["ba", c("2.5 %", "97.5 %")]

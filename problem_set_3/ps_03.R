library(lmtest)
library(sandwich)

setwd("problem_set_3/")

cps <- read.csv(file = "cps99_ps3.csv")


# 5
reg5 <- lm(data = cps, formula = log(ahe) ~ yrseduc + age + female + hsdipl)


# 5.a
estimates5 <- coeftest(reg5, vcov. = vcovHC(reg5, type = "HC1"))

# if the value of yrseduc OR age increased ceteris paribus by 1 unit, then ahe is on average increased by (exp(estimated parameter of yrseduc OR age) - 1)%.
(exp(estimates5["yrseduc", "Estimate"]) - 1) * 100      # percents
(exp(estimates5["age", "Estimate"]) - 1) * 100          # percents

# if the value of female changes from 0 to 1, then ahe is on average increased by (exp(estimated parameter of female) - 1)%
(exp(estimates5["female", "Estimate"]) - 1) * 100          # percents
# -> is on average decrased by 22% if respondent is female.


# 5.b
estimates5["age", "Std. Error"]
# this value tells us how on average our estimated parameter would differ from estimated value.


# 5.c
confint(reg5, vcov. = vcovHC(reg5, type = "HC1"))["female", ]


# 5.d


# 5.e


# 5.f


# 6
reg_quadratic <- lm(data = cps, formula = log(ahe) ~ yrseduc + age + I(age ^ 2) + female + hsdipl)


# 6.a
estimates_quadratic <- coeftest(reg_quadratic, vcov. = vcovHC(reg_quadratic, type = "HC1"))

# check the appendix
age_quadratic_effect <- function(age) {
  estimates_quadratic["age", "Estimate"] +
    2 * estimates_quadratic["I(age^2)", "Estimate"] * age
}

estimates_quadratic[c("age", "I(age^2)"), "Estimate"]

# these parameters could be interpreted only when we supply age, so, for initial effect with [age] our effect of increasing age by 1 unit adds up [age_effect] percent for the ahe itself
data.frame(
  age = c(min(cps$age), mean(cps$age), max(cps$age)),
  age_effect = sapply(c(min(cps$age), mean(cps$age), max(cps$age)), age_quadratic_effect)
)
# this means that when individual is yoing and he is growing up his ahe is growing too, but after some number of years on average effect of age is negative

# generally, the turning point for age effect is age =
-estimates_quadratic["age", "Estimate"] / (2 * estimates_quadratic["I(age^2)", "Estimate"])


# 6.b
estimates_quadratic["I(age^2)", ]
# p-value < 5% -> it is significant -> quadratic relationship


# 6.c
estimates_quadratic["female", "Estimate"]
estimates_quadratic["female", "Estimate"]
# doesn't change; it could have changed if there were structural changes in demographic of men/women ratios within different ages, then catching the full effect (quadratic) of age could really changed female's estimates; but it didn't, so structural changes were insignificant and inappropriate specification (linear age) din't affect


# 6.d
reg_cubic <- lm(data = cps, formula = log(ahe) ~ yrseduc + age + I(age ^ 2) + I(age ^ 3) + female + hsdipl)

estimates_cubic <- coeftest(reg_cubic, vcov. = vcovHC(reg_cubic, type = "HC1"))

estimates_cubic[c("age", "I(age^2)", "I(age^3)"), ]
# p-value of age^3 < 5% -> it is significant -> cubic relationship

# actually, adding a high number of degrees of polynom of some variable is a popular mistake among the beginners, because quite often these degrees are significant but doesn't add up to the explanation much, so unless you're trying to outperform others on kaggle there is no need for degrees higher than 3 in most cases.


# 6.e
library(car)
linearHypothesis(reg_cubic, c("I(age^2) = 0", "I(age^3) = 0"))
# the relationship is qubic.

# I would personally use quadratic model, because it is most commonly used and has good interpretation from economic stand of view. Also, estimates of parameters for all 3 degrees now try to overcome effects of each other, so they are in format of 1,... and not in 0,...


# 7.a
age_cubic_effect <- function(age) {
  estimates_cubic["age", "Estimate"] +
    2 * estimates_cubic["I(age^2)", "Estimate"] * age +
    3 * estimates_cubic["I(age^3)", "Estimate"] * (age ^ 2)
}

age <- c(30, 45, 60)

round(
  t(
    data.frame(
      age = age,
      age_linear_effect = estimates5["age", "Estimate"],
      age_quadratic_effect = sapply(age, age_quadratic_effect),
      age_cubic_effect = sapply(age, age_cubic_effect)
    )
  ),
  3
)


# 7.b
# linear - worst effect, person in his 60s has the same age effect as in his 30s
# quadratic - seems reasonable, but I would love to see that at 30-31 the age effect being positive, however, estimates give us negative result
# cubic - effects are rather large, 3-10 times larger than quadratic, I wouldn't go with those estimates.


# 7.c
# as I have said I would personally use quadratic model, because it is most commonly used and has good interpretation from economic stand of view.
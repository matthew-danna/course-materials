# source: https://stats.oarc.ucla.edu/r/dae/logit-regression/
install.packages('aod')
library(aod)
library(tidyverse)

# get data
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# summarize
summary(mydata)

# get SD's for each variable
sapply(mydata, sd)

# two-way contingency for categorical outcomes and predictors we want
### make sure no 0 cells
xtabs(~admit + rank, data = mydata)

# convert rank to a factor to treat as a categorical variable
mydata$rank <- factor(mydata$rank)

# logit model
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

# confidence intervals with log-likelihood function
confint(mylogit)

# confidence intervals with standard errors
confint.default(mylogit)

# wald test to test for an overall effect of rank 
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

# test a specific rank: create a vector for the terms, set the ones you don't want to test to 0, and the two
# to compare to 1 and -1. this example compares Rank 2 to Rank 3
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

# expoentiate the coefficients to get odds-ratios
exp(coef(mylogit))

# odds ratios and 95% CIs
exp(cbind(OR = coef(mylogit), confint(mylogit)))
### Now we can say that for a one unit increase in gpa, 
### the odds of being admitted to graduate school (versus not being admitted) increase by a factor of 2.23

##### You can also use predicted probabilities to help you understand the model. 
##### Predicted probabilities can be computed for both categorical and continuous predictor variables. 
##### In order to create predicted probabilities we first need to create a new data frame with the values 
##### we want the independent variables to take on to create our predictions.

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

# These objects must have the same names as the variables in your logistic regression above

# predicted probabilities table
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

### In the above output we see that the predicted probability of being accepted into a graduate program is 0.52 
### for students from the highest prestige undergraduate institutions (rank=1), 
### and 0.18 for students from the lowest ranked institutions (rank=4), holding gre and gpa at their means. 

##### We can do something very similar to create a table of predicted probabilities varying the value of gre and 
##### rank. We are going to plot these, so we will create 100 values of gre between 200 and 800, at each value of 
##### rank (i.e., 1, 2, 3, and 4).

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

# predicted probabilities, with standard error
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# predicted probabilities, and 95% confidence intervals
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) + 
  geom_line(aes(colour = rank), size = 1)

##### testing fit, compared to an empty model:
# difference in deviance for the models
with(mylogit, null.deviance - deviance)

# DF for difference between two models is equal to number of predictor values in the mode:
with(mylogit, df.null - df.residual)

# p-value
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



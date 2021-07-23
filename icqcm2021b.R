################################################################################
#' Machine learning with the "Titanic" dataset
#' Tutorial for ICQCM; this is an update of the
#' script I made from a tutorial I did live. 
#' Momin M. Malik
#' 22 July 2021
################################################################################

install.packages("tree")
install.packages("randomForest")
install.packages("caret")
install.packages("e1071") # this is required for caret
library(tree)
library(randomForest)
library(caret)
library(e1071)

# R can read data from the Internet! 
df <- read.csv("https://www.mominmalik.com/titanic.csv")

# Convert categorical variables from characters to factors
df$survived <- relevel(factor(df$survived), ref = "No")
df$pclass <- relevel(factor(df$pclass), ref = "1st")
df$sex <- relevel(factor(df$sex), ref = "male")

# Split data, by the partition of Kaggle/Datacamp (stored in df$train column)
df.train <- df[df$train, ]
df.test <- df[!df$train, ]

# The "base rate" is the majority class
mean(df.train$survived=="No")

#############################
# Exploratory Data Analysis #
#############################
# These descriptive relationships give clues to how we might make a 
# classification model
table(df.train$survived, df.train$sex)
table(df.train$survived, df.train$pclass)

# This is now getting too complicated to follow manually
table(df.train$survived, df.train$pclass:df.train$sex)

# What about continuous variables?
plot(density(df.train$age[df.train$survived=="No"], na.rm = T), col = 2)
lines(density(df.train$age[df.train$survived=="Yes"], na.rm = T))
# We see that there's a "hump" at younger ages: that gives a clue to what
# might help model survival

# What about age:sex interaction?
# with() is a shortcut, treating the first argument like attach()
with(df.train, {
  plot(density(age[survived=="No" & sex=="male"], na.rm = T), col = 2, 
       main = "", 
       xlab = "age")
  lines(density(age[survived=="No" & sex=="female"], na.rm = T), col = 2, lty = 2)
  lines(density(age[survived=="Yes" & sex=="male"], na.rm = T))
  lines(density(age[survived=="Yes" & sex=="female"], na.rm = T), lty = 2)
})

##############################
# Social statistics approach #
##############################
# Note that for convenience, I am ignoring missing age values. These *should*
# be treated appropriately; either by deliberately dropping the cases, or 
# imputing their values (e.g., mean imputation, or median imputation), or making
# a dummy variable for whether age is present or not in case that contains info. 
# By not doing anything, R drops rows with missing values. 

# Also, for social statistics, we don't need to split the data. However, I'll
# use these models later on, so for that reason I fit on the training set only.

# Start with a simple logistic regression
summary(glm1 <- glm(survived ~ sex + pclass + age + fare, 
                    data = df.train, 
                    family = "binomial"))
# Note "observations deleted due to missingness"

# Interpreting coefficients as odds ratio, by exponentiating
exp(coef(glm1))
# Odds ratios are multiplicative. 1 means no effect; greater than 1 means more 
# likely; less than 1 means less likely. 

# Or, transforming back to [0, 1]
logistic <- function(x) 1/(1 + exp(-x))
logit <- function(p) log(p/(1-p))
logistic(coef(glm1))
# The "base rate" is 77% survival; this is high because the other covariates'
# values will bring it down to the average of 38%. Unlike odds ratios, 
# coefficients expressed as probabilities are hard to interpret (other than the 
# intercept), since they aren't additive or multiplicative (as odds ratios they 
# are multiplicative). Still, closeness to 0 or 1 represents the strength of 
# this effect. 'Being' female makes a person far more likely to survive as 
# compared to the reference class (male). Being 2nd class makes a person less 
# likely to survive than the reference class (1st), and being 3rd class makes a 
# person FAR less likely to survive than the reference class (1st). Each 
# additional unit of age (1 year) makes a person slightly less likely to 
# survive. Fare doesn't have a significant effect. 

# But, from our exploratory analysis, it seems like there are interactions. 
# Let's do a model with all interaction effects. 
summary(glm2 <- glm(survived ~ sex*pclass*age*fare, 
                    data = df.train, 
                    family = "binomial"))
round(exp(coef(glm2)), 4)
round(logistic(coef(glm2)), 4)
# This is harder to interpret; lots of interactions are not significant, but 
# some are. It seems like sex is no longer significant, on its own or in 
# interactions with passenger class; instead, the effect is from the interaction
# of age and passenger class. 

# When adding in interaction effects and nonlinear effects (e.g., age^2), there
# are far more possible models than we could ever test, so we have to be guided
# by theory and our exploratory data analysis in terms of trying out different 
# specifications (choices of variables) for the logistic regression, and then 
# potentially formally comparing them (e.g., by AIC or BIC, residual deviance, 
# or, for nested models, F-tests). 

# Our goal is ultimately to use the logistic regression to say something about 
# the relationship of these variables to the likelihood of survival. 


#############################
# Machine learning approach #
#############################
# With machine learning, we are no longer looking at coefficients; instead, we 
# care about how well the fitted values agree with the true values. We will try
# a decision tree first. 

tr1 <- tree(survived ~ sex + pclass + age + fare, 
            data = df.train, 
            na.action = na.pass)

# We can plot the fitted tree. At each split, the condition being "true" means
# go down the left branch. Terminal  (leaf) nodes are the fitted class for that
# path. I am adding in the number of observations at each split, and at each 
# leaf node. 
plot(tr1, type = "uniform")
text(tr1, pretty = 0, label = "n", all = T)
text(tr1, splits = F, adj = c(.5,2))

# We can get more details by printing out the fitted tree. 
tr1
# Or, it's easier to compare split values (but harder to follow the tree 
# structure) from:
tr1$frame

# Note that the terminal node label corresponds to "Yes" if the second yprob 
# entry is bigger than the first, and "No" otherwise. 

# Here, we see the decision tree picking up interactions between sex and 
# passenger class, and for sex==female and pclass==3rd, *fare* is then the most 
# correlated with whether passengers survived or not. For sex==male, having a 
# *higher* fare corresponds to a prediction of not surviving; for those with a
# lower fare, being younger than 10 predicts surviving. 

# Decision trees are "interpretable" (see my "extra" slides for discussion of
# how this is dangerous), but the are making splits only based on what is 
# optimal. 

# What really matters is *performance*. We can manually get a confusion matrix:
table(predict(tr1, type = "class"), 
      df.train$survived, 
      dnn = c("predicted", "true"))

# However, rather than manually calculating all the metrics, we can rely on the 
# caret command confusionMatrix, which has lots of nice metrics. 
# "data" are the fitted/predicted values, "reference" are the true values

### Tree 1 training performance:
confusionMatrix(data = predict(tr1, type = "class"), 
                reference = df.train$survived, 
                positive = "Yes")

# CIs are from a binomial proportions test. From that, we can see that it is 
# significantly better than the No Information Rate (base rate) that we got 
# from mean(df.train$survived=="No"). 

# McNemar's test is most useful for comparing two sets of predictions. Here,
# the cells (from left to right) would be, the count of cases for which both 
# models are correct; the count of cases for which model 1 is correct and model 
# 2 is incorrect; the count of cases where model 1 incorrect and model 2 is 
# correct; and the count of cases where both models are incorrect. 

# When looking at McNemar's test for predictions vs. the reference (true) values,
# it is a test of whether or not the model misclassifies one label significantly
# more than than the other. It would be nice if our models performs equally well
# on both classes, but this is not particularly important. 

# Precision is Pos Pred Value: the proportion of predicted positive values 
# that are actually positive (how much out of returned results are "relevant").
# Recall is Sensitivity: the proportion of people who survived that the model
# correctly "found". 

# But the REAL test is how well the model does on the held-out (test) data. 
### Tree 1 test performance:
confusionMatrix(data = predict(tr1, newdata = df.test, type = "class"), 
                reference = df.test$survived, 
                positive = "Yes")
# The improvement over the base rate is still significant, but the accuracy 
# drops from .82 to .78; precision from .81 to .71, and recall from .71 to .70.
# McNemar's test is no longer significant, but again that isn't too important. 


# One way I can show why we shouldn't necessarily interpret decision trees is 
# in how they depend highly on tuning parameters that have no "substantive" 
# (theoretically-justified) reason for being any particular value. They are set
# to default values that experience has shown to generally work "pretty well".
tr2 <- tree(survived ~ sex + pclass + age + fare, 
            data = df.train, 
            control = tree.control(nobs = nrow(df.train),
                                   mincut = 1,
                                   minsize = 2,
                                   mindev = 0),
            na.action = na.pass)

plot(tr2, type = "uniform")
text(tr2, pretty = 0, label = "n", all = T, cex = .5, adj = c(.5, -1))
text(tr2, splits = F, adj = c(.5,0), cex = .5)

# That is no longer so interpretable! But let's check the performance:
### Tree 2 training performance:
confusionMatrix(data = predict(tr2, type = "class"), 
                reference = df.train$survived, 
                positive = "Yes")
# The accuracy on the training set is WAY better! 

# BUT: what really matters is the test performance. 
### Tree 2 test performance:
confusionMatrix(data = predict(tr2, newdata = df.test, type = "class"), 
                reference = df.test$survived, 
                positive = "Yes")
# The accuracy actually DROPPED in the test data! The CIs overlap, so it's not a 
# significant difference (although formally, we should test this with McNemar's 
# test). This is a sure sign of overfitting.

# Note, however, that it's perfectly possible to use the logistic regression
# for classification! 

# Getting the predicted classes is a bit trickier here, I use the ifelse()
# function as a shortcut. 

glm1.pred.train <- relevel(factor(ifelse(test = predict(glm1, 
                                                        type = "response") > .5, 
                                         yes = "Yes", 
                                         no = "No")), 
                           ref = "No")

glm2.pred.train <- relevel(factor(ifelse(test = predict(glm2, 
                                                        type = "response") > .5, 
                                         yes = "Yes", 
                                         no = "No")), 
                           ref = "No")


glm1.pred.test <- relevel(factor(ifelse(test = predict(glm1, 
                                                       type = "response", 
                                                       newdata = df.test) > .5, 
                                         yes = "Yes", 
                                         no = "No")), 
                          ref = "No")

glm2.pred.test <- relevel(factor(ifelse(test = predict(glm2, 
                                                       type = "response", 
                                                       newdata = df.test) > .5, 
                                         yes = "Yes", 
                                         no = "No")), 
                          ref = "No")

####################################
# # Aside: The Pipe Operator "%>%" #
####################################
# # While I like to stick to base R, I really appreciate the "pipe" operator, 
# # part of the "tidyverse" of packages, and specifically in the "magrittr" 
# # library. This operator, %>%, means "take the thing before, and make it the 
# # first argument of the thing after". To explicitly specify where the left-
# # hand argument should be inserted, we can use a ".". This is amazingly 
# # useful for eliminating endlessly nested function calls. Doing more complex 
# # things is also possible, but then gets complicated and becomes an art in 
# # itself. The above annoying nesting is a perfect place to use piping. 
# library(magrittr)
# glm2.pred.test <- 
#   glm2 %>% 
#   predict(type = "response", 
#           newdata = df.test) %>%
#   is_greater_than(.5) %>%
#   ifelse(yes = "Yes", 
#          no = "No") %>%
#   factor %>%
#   relevel(ref = "No")

# Training performance
confusionMatrix(data = unname(glm1.pred.train), 
                reference = df.train$survived[!is.na(df.train$age)], 
                positive = "Yes")

confusionMatrix(data = unname(glm2.pred.train), 
                reference = df.train$survived[!is.na(df.train$age)], 
                positive = "Yes")

# Test performance
confusionMatrix(data = unname(glm1.pred.test), 
                reference = df.test$survived, 
                positive = "Yes")

confusionMatrix(data = unname(glm2.pred.test), 
                reference = df.test$survived, 
                positive = "Yes")

# Compare:
confusionMatrix(data = predict(tr1, newdata = df.test, type = "class"), 
                reference = df.test$survived, 
                positive = "Yes")
# The logistic regression is not as good as the default decision tree, but it is
# competitive! (Formally, we should test this with McNemar's test.)

# Along with not properly dealing with missing values, I am committing another
# major sin here: having tested several models against the test set, I am 
# effectively using this split for model selection, rather than reserving it for 
# evaluation. This is not good, since it leaves me no set of data on which to 
# cleanly test after I finally select a model that I think works the best. That 
# is because, by trial-and-error, I will eventually find models that work better
# and better on the test set, because I will be overfitting! 
# For doing model SELECTION, I should have further split the training set into
# different partitions. Here, I could have (for example) split into 10 
# partitions, about 90 observations in each, and that would let me try 10 
# different models; for each model, I would train on 9 of the 10 folds, and test
# on the left-out fold. I would pick the model that worked the best (although 
# if certain models perform very close to each other, I would prefer a simpler) 
# model. Such k-fold cross validation has good theoretical properties for 
# getting us to the best model, but k-fold cross validation (averaging across
# the folds, when testing just 1 model) turns out to NOT give a reliable way
# of evaluating our model. 



# Lastly, we can use the best off-the-shelf classifier, a random forest. 
# This bootstraps across observations and variables, so we should set a seed.
set.seed(20170722)
rf1 <- randomForest(survived ~ sex + pclass + age + fare, 
                    data = df.train, 
                    na.action = na.roughfix)

table(predict(rf1), df.train$survived, dnn = c("Predicted", "True"))

confusionMatrix(data = predict(rf1), reference = df.train$survived, positive = "Yes")
confusionMatrix(data = predict(rf1, newdata = df.test), reference = df.test$survived, positive = "Yes")

# In this case, the test performance is about the same. Random forests really 
# excel with lots of variables, especially for finding interaction effects and
# nonlinear effects; here there aren't enough variables for a random forest
# to really shine. 

# Also note that random forests (like decision trees) pick up interactions 
# automatically, no need to specify them. 

# Random forests also have a measure of variable importance; this is how 
# much a given variable increases performance, across all splits across all 
# individual trees in the forest. It is useful for interpreting in an 
# exploratory way, but it is NOT the same as statistical significance. 
importance(rf1)
varImpPlot(rf1)


# Note that the "ctree" and "rpart" libraries are alternatives to the "tree" 
# library; they too build trees, but with some subtle differences in the kind 
# of trees and how they are built (conditional inference trees and recursive 
# partitioning, respectively). Both ctree and rpart have great plotting 
# capabilities, with functions to make much nicer and more information-rich 
# plots than the manual ones I made for tree. I used "tree" only because it's 
# easier to give tuning parameters that overfit (this is probably a bad thing 
# in general, but I wanted an overfitted model to make a point).

# "party" is an alternative to "randomForest", built on top of ctree. Trees 
# extracted from a forest in party can be plotted as ctrees. 

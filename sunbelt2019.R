############################################################################
# This script is a cleaned-up version of one dynamically done as a
# demonstration at the workshop "Everything you ever wanted to know
# about network statistics but were afraid to ask", given at Sunbelt
# 2019, the XXXIX Conference of the International Network for Social 
# Network Analysis, 18 June 2019, Montreal, Canada. 
# It demonstrates how to manipulate network data to get it in a 
# form in which we can "naively" model it with a logistic regression.
# Without "dyad-dependent" terms, this is equivalent to an ERGM. 
# However, we can see the dyad-dependent terms, which logistic
# regression CANNOT handle but ERGMs can, change the estimates: so 
# we can think of dyad-dependent terms like reciprocity (number of 
# mutual ties) as causing omitted variable bias when not included. 
# There may be additional omitted variables, and the ERGM may fail
# in ways other that omitted variables, but it is a useful exercise
# to be able to do analysis in logistic regression before going over
# to a network model (whether ERGMs or something else).
# Copyright (c) 2019 Momin M. Malik
# v1.1, 22 June 2019
# License: MIT, https://www.r-project.org/Licenses/MIT
############################################################################


library(magrittr) # For 'piping' operator %>%
# # Directly load data from the Internet:
# temp <- tempfile()
# download.file("https://www.stats.ox.ac.uk/~snijders/siena/LazegaLawyers.zip",temp)
# A <- as.matrix(read.table(unz(temp, "ELfriend.dat")))
# node <- read.table(unz(temp, "ELattr.dat"))
# unlink(temp)
# rm(temp)

# Otherwise, download and unzip data, and 
# replace this with the appropriate Mac 
# directory or path in Windows
setwd("~/Downloads/LazegaLawyers")

# Read in friend network, node attributes
A <- as.matrix(read.table("ELfriend.dat"))
nodes <- read.table("ELattr.dat")

# From the codebook, the columns are these
names(nodes) <- c("seniority",
                  "status",
                  "sex",
                  "office",
                  "tenure",
                  "age",
                  "practice",
                  "lawschool")

# Also from codebook, the column codings
nodes$status <- nodes$status %>% 
  factor(labels=c("partner",
                  "associate"))
nodes$sex <- nodes$sex %>% 
  factor(labels=c("male",
                  "female"))
nodes$office <- nodes$office %>% 
  factor(labels=c("Boston",
                  "Hartford",
                  "Providence"))
nodes$practice <- nodes$practice %>% 
  factor(labels=c("litigation",
                  "corporate"))
nodes$lawschool <- nodes$lawschool %>% 
  factor(labels=c("Harvard/Yale",
                  "UConn",
                  "Other"))

# Install if you don't already have
# install.packages("corrplot")
library(corrplot)
corrplot(A)
# Adjacency matrix is NOT a correlation, I'm just using
# a package that's overall useful for plotting matrices
# Demonstration:
# M <- matrix(rnorm(500), nrow = 5, ncol = 10)
# corrplot(M, is.corr = F)
# rm(M)

# I want to know how as.vector() works. Does it stack by
# rows, or by columns?
corrplot(as.matrix(as.vector(A)[1:70]), is.corr = T)
# Compare to corrplot(A), we see as.vector() stacks
# by columns

# Will use the number of nodes, save for convenience
n <- nrow(A)


#########
# !!!!! #
#########
# This command turns the adjacency matrix into
# a dataset of edges, including both 1s (ties 
# that are present) AND 0s (ties that are absent),
# as both are needed for logistic regression
# (although not for an edgelist, such as to 
# import into igraph)
df <- data.frame(from = rep(1:n, times = n),
                 to = rep(1:n, each = n))
df$tie <- as.vector(A)
# rep(,times = n) gives 1, 2, ..., 71, 1,...
# which represents the ROW INDEX of as.vector(A)
# rep(,each = n) gives 1, 1, ..., 1, 2, ...
# which represents the COL INDEX of as.vector(A)

# If you have node names, you can replace 1:n
# with the vector of names.

# NOTE: We still have rows for i -> i, which we'll
# want to eliminate. But we'll do that at the end.

# Make sender and receiver edge covariate by copying
# over node covariates, using same times/each trick
df$from.sex <- rep(nodes$sex, times = n)
df$to.sex <- rep(nodes$sex, each = n)
# Make an edge covariate for match of node attributes
df$sex.match <- df$from.sex == df$to.sex

# Now, delete self-loops
df <- df[df$from != df$to,]

# WARNING! Once you delete self-loops, you can no longer
# use the "times = "/"each = " trick like before. So 
# either put in all the node covariates you want to later
# use as or use to make edge covariates before this step. 
# Or else, you can do something like this:
# tmp <- data.frame(from = rep(1:n, times = n),
#                   to = rep(1:n, each = n))
# tmp$from.sex <- rep(nodes$sex, times = n)
# tmp <- tmp[tmp$from!= tmp$to,]
# df$from.sex <- tmp$from.sex

# Can do a quick chi-squared test
chisq.test(table(df$tie, df$from.sex == df$to.sex))
# Reject the null of no relationship, but 
# doesn't give us effect size

# Model 1: only sex.match
glm.1 <- glm(tie ~ sex.match, data = df, family = "binomial")
summary(glm.1)
exp(coefficients(glm.1)) # Exponentiating gives odds ratios
# greater than 1 means that much more likely
# less than 1 means that much less likely

# Can also distinguish male-male and female-female ties
df$sex.match2 <- NA
df$sex.match2[df$from.sex == "male" & df$to.sex == "male"] <- 1
df$sex.match2[df$from.sex != df$to.sex] <- 2
df$sex.match2[df$from.sex == "female" & df$to.sex == "female"] <- 3
df$sex.match2 <- factor(df$sex.match2)

# Model 2: more sophisticated sex.match
glm.2 <- glm(tie ~ sex.match2, data = df, family = "binomial")
summary(glm.2)
exp(coefficients(glm.2))


# Comparing to ERGMs
library(statnet)

# First, need to turn A into a network object
rownames(A)
colnames(A) # These might cause problems, delete
colnames(A) <- NULL

# Create a "network" object for statnet to use 
lazega <- network(A, directed = T)

# Now that it's a network object, we can do a quick plot
plot(lazega) # Pretty bad, we won't spend time making pretty

# Populate the network object with the node attributes
# Note: factors don't work, need to do as.character()
lazega %v% "seniority" <- nodes$seniority
lazega %v% "status" <- as.character(nodes$status)
lazega %v% "sex" <- as.character(nodes$sex)
lazega %v% "office" <- as.character(nodes$office)
lazega %v% "tenure" <- nodes$tenure
lazega %v% "age" <- nodes$age
lazega %v% "practice" <- as.character(nodes$practice)

# First ERGM; compare to glm.1
ergm.1 <- ergm(lazega ~ edges + nodematch("sex"))
summary(ergm.1)
summary(glm.1)
# They are the same!! An ERGM without any "dyad
# dependent" terms is actually just a logistic 
# regression. 

# What ERGMs can do, that logistic regression can't,
# is include dyad-dependent terms. Reciprocity is the
# simplest one, measured by number of mutual dyads

# What if we tried to include reciprocity in the 
# logistic regression
# Here's a trick to get reciprocal ties in a column
df$recip <- df$tie[order(df$from,df$to)]

# # We could also get reciprocal ties like
# tmp <- data.frame(from = rep(1:n, times = n),
#                   to = rep(1:n, each = n))
# tmp$tie <- as.vector(t(A))
# tmp <- tmp[tmp$from!= tmp$to,]
# df$recip2 <- tmp$tie

# # These two approaches are the same
# sum(df$recip!=df$recip2)

# Can do a test. Are ties mutual more likely
# than by chance? Yes.
chisq.test(table(df$tie, df$recip))

# What does a glm fit with this reciprocity term give?
glm.3 <- glm(tie ~ (tie==recip), data = df, family = "binomial")
summary(glm.3)

# How does this compare to an ERGM with the "mutual" term?
ergm.2 <- ergm(lazega ~ edges + mutual)
summary(ergm.2)
summary(glm.3)
# Now *these* are not the same! Can't simply add
# dyad-dependent terms as columns and expect things
# to work out. 

# We can try a adding in nodematch, and see how it compares
# with the equivalent logistic regression
ergm.3 <- ergm(lazega ~ edges + mutual + nodematch("sex"))
summary(ergm.3)

glm.4 <- glm(tie ~ sex.match + (tie==recip), data = df, family = "binomial")
summary(glm.4)
# Indeed, these too are different. 

# Can also compare sex.match2 to nodematch("sex", diff = T)
glm.5 <- glm(tie ~ sex.match2, data = df, family = "binomial")
summary(glm.5)
ergm.4 <- ergm(lazega ~ edges + nodematch("sex", diff = T))
summary(ergm.4)
# Again, without dyad-dependent terms, an ERGM can be done
# as just a logistic regression. 
# Having terms like nodematch(diff = T) can make it easier
# to construct edge covariates, but it's good to know how to
# do it oneself as we did above. 

# Last topic: Getting a null distribution
# We can simulate from a Bernoulli distribution
# (a binomial with only 1 trial, so possible
# values 0 and 1), used also for coin flips
# We want the probability to be the density of
# the network. This is mean(df$tie). 
# 1000 replications is usual, can do as few as 100
# or as many as you'd like. 
null.dist <- rep(NA, 1000)
for (i in 1:length(null.dist)) {
  null.dist[i] <- sum(rbinom(n = nrow(df), size = 1, prob = mean(df$tie)) == 1 &
                        df$from.sex == df$to.sex &
                        df$from.sex == "male")
}

# Compare the observed statistic to the null distribution
obs.stat <- sum(df$tie == 1 &
                  df$from.sex == df$to.sex &
                  df$from.sex == "male")
hist(null.dist)
abline(v = obs.stat)
# This looks far from the mean. But we can calculate
# precisely. 

# Get empirical quantiles
quants <- quantile(null.dist, probs = seq(0,1,.001))

# Where in the quantiles does the observed statistic fit?
quants[min(which(obs.stat < quants))]

# Extracting as a p-value
(100 - as.numeric(sub("%",
                     replacement = "", 
                     names(quants[min(which(obs.stat < quants))])))
)/100
# This is the "empirical p-value", the minimum quantile
# at which we can reject the null hypothesis for a 
# one-sided test. Double the p-valye for a two-sided
# test. 
# But more simply, if the observed statistic is 
# greater than the 97.5th percentile or less than
# the 2.5th percentile, we can reject the null at 
# the 0.05 level for a two-sided test. If the 
# observed statistic is greater than the 99.5th 
# percentile or less than the 0.5th percentile, we
# can reject at the 0.01 level for a two-sided test.


# Instead of using rbinom, we could also shuffle edges.
# This is "bootstrapping", using sampling with replacement
# to get a null distribution. It is amazing that this works, 
# but it does: we can use the variability in the data to 
# estimate the uncertainty of our estimate. 
# It also seems strange that we would make a bootstrap sample
# the same size as our original data set (why not larger?)
# but it turns out to not matter, so we just use the same size.
null.dist <- rep(NA, 1000)
for (i in 1:length(null.dist)) {
  null.dist[i] <- sum(sample(df$tie, 
                             size = nrow(df), 
                             replace = T) &
                        df$from.sex == df$to.sex &
                        df$from.sex == "male")
}
hist(null.dist)
abline(v = sum(df$tie == 1 &
                 df$from.sex == df$to.sex &
                 df$from.sex == "male"))

# Repeat the p-value extraction above. These won't be 
# identical to the results from the Bernoulli null
# distribution, but larger numbers of values in the 
# null distribution should make them the same. But
# these values are approximate anyway, so we shouldn't
# both with increasing the precision of the estimate. 
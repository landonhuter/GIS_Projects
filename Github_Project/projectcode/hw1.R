# # # # # # # # # # # # # # # # # # # #
# # # WILD 3810 # # # # # # # # # # # #
# # # Spring 2022 # # # # # # # # # # #
# # # Homework 1: Estimating Abundance #
# # # # # # # # # # # # # # # # # # # #

# Estimating population size using Lincoln-Peterson ####

# Last summer you were working for the UDWR collecting data on desert tortoises.
# Your task was to conduct a CMR study to estimate their population size in Utah.
# Now it's time to analyze your data and get back to UDWR with an estimate. 
# In June, you went out and captured 426 tortoises. You marked them by drawing a
# small mark with non-toxic permanent marker on their carapace. Then you went
# out again in July and captured 399 individuals. Of these, 128 were recaptures.
# What is the estimate of population size + uncertainty measure you will give 
# the UDWR in the final project report (mean + SD)? 


n1 <- 426
n2 <- 399
m2 <- 128

Nhat <- n1 * n2 / m2
round(Nhat)

mean(Nhat)

# Remember that we always need to associate our estimate with a metric of 
# uncertainty. Calculate the variance around your mean estimate.

varN <- ((n1 + 1)*(n2+1)*(n1 - m2) *(n2-m2))/ ((m2+1)^2*(m2+2))

sdN <- sqrt(varN)

# Estimating detection probability ####

# Given your data and your estimate of Nhat, what do you estimate was the value
# of detection probability for tortoises in Utah? 

dp <- n1/Nhat

# Demonstration that LP is unbiased ####

# You only got to go out and do CMR on tortoises once, but what if you were able
# to repeat your study 1000 times? Assuming that your estimate of population 
# size matches the true value, and from the estimate of p you derived, use 
# simulations to demonstrate that LP is an unbiased estimator of population size. 
# Make a histogram of the resulting estimates and add a red line to mark your
# original value of Nhat.

N <- 1328

p <- .32

nsims <- 1000


n1 <- rbinom(n = nsims, size = N, prob = p)

m2 <- rbinom(n = nsims, size = n1, prob = p)

nm2 <- rbinom(n = nsims, size = N-n1, prob = p)

n2 <- m2+nm2

Nhat <-  n1*n2 / m2

mean(Nhat)

hist(Nhat)

abline(v=N, col= "red")


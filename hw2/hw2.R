#Assignment 2 

#I want to know what is working directiory 
getwd()
#change to working directory I want to
setwd('coding_r/413hw/hw2')
#Using R reading csv file
PgWeight = read.csv('Assignment 2 PenguinWeights.csv')

#Problem 1a generate a histogram of sample weighs with 15 bins
str(PgWeight)
x = PgWeight$PenguinWeights #assign x to be the data set
hist(x,breaks = 15, xlab = 'sample weight', main = 'sample weighs with 15 bins', col = 'blue')
#Problem 1b generate a histogram of sample weighs with 7 bins
hist(x,breaks = 7, xlab = 'sample weight', main = 'sample weighs with 7 bins', col = 'Red')

#2.Determine the 95% confidence interval for mean of the penguin weights.
x_bar = mean(x) #sample mean
std = sd(x) #sample deviation
z_value = qnorm(0.975) # get z value
up_bnd = x_bar + z_value*std/sqrt(50) # get upper bound of 95% confidence interval
up_bnd
low_bnd = x_bar - z_value*std/sqrt(50) # get lower bound of 95% confidence interval
low_bnd

#3.Test the hypothesis that the sample is from a population of penguins with a mean weight of 13.5. Specify the p-value of the test and your interpretation on the test results.
t.test(x , mu = 13.5, conf.level = 0.95)

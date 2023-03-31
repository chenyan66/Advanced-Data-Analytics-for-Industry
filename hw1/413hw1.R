#1. Vectors & Matrices  
#a. Create a vector with 100 elements with randomly generated real numbers ranging from 1 to 25   
x = runif(100,1,25)

#b. Reshape this vector into a 10 by 10 matrix reading by column.   
x = matrix(x,10,10)

#c. Similarly, generate another vector with 500 elements of standard normal distribution 
#(with mean = 2 and sd =0.5) and plot its histogram. Reshape this into a 10 by 50 
#matrix reading by row.(Hint: use matrix() and rnorm() functions)   
y = matrix(c(rnorm(500,mean = 2, sd = 0.5)), 10, 50)
      
#2a. Use the summary() function to produce a numerical summary of the variables in the data set.
Colleges = read.csv("Assignment1CollegeDataset.csv")
summary(Colleges)

#b. Create a boxplot of variable "Outstate" as a function of "Private" (Yes for Private; No for Public).  Explain your interpretation of the boxplots. 
attach(Colleges)
names(Colleges)
boxplot(Outstate[Private=="Yes"],Outstate[Private=="No"])

#3. Load the Auto data set, which is in the ISLR library. Understand information about this data set by either ways we introduced in class (like “?Auto” and names(Auto))   
#a. Make a scatterplot between cylinders and mpg. Draw pairwise scatterplot between  “mpg”, “displacement”, “horsepower”, “weight”, “acceleration” (try to plot all scatterplots in one figure; hint: use pairs() command). By observing the plots, do you think the two variables in each scatterplot are correlated? If so, how?  
library(ISLR)
names(Auto)
attach(Auto)
plot(cylinders, mpg, xlab= "#cylinders", ylab= "# mpg")
pairs(~mpg+displacement+horsepower+weight+acceleration, data = Auto, main = 'Auto Scatterplot Matrix')
#b. Create a scatterplot between mpg and horsepower. Draw a straight line on the scatterplot to represent relationship between the two variables.   
plot(mpg, horsepower , xlab= "#mpg", ylab= "# horsepower") 
abline(lm(horsepower ~ mpg, data = Auto), col = "Red")
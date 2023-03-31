########### Sample code to help with Assignment 3 ###############

#Remember to change working directory to where you are storing Assignment3Data.RData
setwd("C:/Users/doncorleron/Documents/coding_r/413hw/hw3")
#Be sure to keep Rdata file in the specified working directory
load("Assignment 3.RData")

### display the dataset, check the dimension of the dataset
train
test
View(train)
View(test)



### you can use "lm" function to model the data (x,y) using different levels of polynomials of x
### remember to specify your model using I(x^d) where d is the degree of desired polynomial
### for example, I(x^2) will fit a model with qudratic x vales
### I(x^3) will create a model with x-cubed values
## I(x^1) shown below will fit a model with simple linear coefficients
Y = c() #make a Y vector for storing value of MES for 5 different polynomials
for( i in 1:5){ #making a for loop to calculate different polynomials 

model <- lm(y ~ I(x^i),data=train)

### you can look up for details of the model
#summary(model)



### use this code to fit data in test set
pred=predict(model,test)

### compute the test error
msetest=mean((test$y-pred)^2)
Y <- append(Y,msetest)
msetest}

X = c(1,2,3,4,5)#making X vector for MES
plot(Y~X, type = 'b', main = "MES for different polynomials")
#plot picture for test MES




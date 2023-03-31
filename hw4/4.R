library(ISLR)
# call the library
attach(Auto)
lm_fit = lm(mpg~cylinders+displacement+horsepower+ weight+acceleration) 
# fit the data as multiple linear regression, let mpg as y, others as predictor
summary(lm_fit)
# Summary the model
new_data = data.frame(cylinders = c(8), displacement = c(307), horsepower = c(130), weight = c(3504),acceleration = c(12))
# insert new data set 
pre_con = predict(lm_fit,newdata = new_data,interval = "confidence", level = 0.95)
pre_con
pre_pre = predict(lm_fit,newdata = new_data,interval = "prediction", level = 0.95)
pre_pre
# check prediction value, confidence interval and prediction interval.

# diagnostic plots
par(mfrow = c(3, 2)) # Set plot layout to 3x2
plot(lm_fit, which = 1) # Residuals vs Fitted
plot(lm_fit, which = 2) # Normal Q-Q
plot(lm_fit, which = 3) # Scale-Location
plot(lm_fit, which = 4) # Cook's distance
plot(lm_fit, which = 5) # Residuals vs Leverage

#transforming  of a select predictor variable
lm_fit = lm(mpg~cylinders+I(displacement^2)+horsepower+ weight+acceleration) 
summary(lm_fit)

#transforming  of  response variable
lm_fit = lm(I(mpg^2)~cylinders+displacement+horsepower+ weight+acceleration) 
summary(lm_fit)

#interation
lm_fit = lm(mpg~cylinders*displacement+horsepower+ weight+acceleration) 
summary(lm_fit)
library(fma)

'''
Question 1
a) Plot the data and find the regression model for Mwh with temperature as an explanatory
   variable. Why is there a negative relationship?                              
'''
day <- c(1,2,3,4,5,6,7,8,9,10,11,12)
mwh <- c(16.3,	16.8,	15.5,	18.2,	15.2,	17.5,	19.8,	19.0,	17.5,	16.0,	19.6,	18.0)
temp <- c(29.3,	21.7,	23.7,	10.4,	29.7,	11.9,	9.0,	23.4,	17.8,	30.0,	8.6,	11.8)

df <- data.frame(mwh, temp)

mwf <- ts(mwh)
temp <- ts(temp)

par(mfrow=c(2,1))
plot(mwf)
plot(temp)

lm_fit <- lm(mwh~temp, data= temp)


res <- residuals(lm_fit)

par(mfrow=c(1,1))
plot(fitted(lm_fit), res,
     xlab="Predicted scores", ylab="Residuals")
abline(0,0)




qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 





newdata <- data.frame(temp = c(10, 35, 18))
predict(lm_fit, newdata)
#1        2 
#18.74795 15.11902 

par(mfrow=c(1,1))
plot(df$mwh, df$temp, xlab="Energy Consumption Mwh", ylab="Temp")

library(fma)



install.packages('forecast', dependencies = TRUE)
library(forecast)
forecast(lm_fit, newdata, level = 99)


tmodel <- ar.ols(temp,  order.max = 3)
pred_ar <- predict(tmodel, n.ahead = 1)
pred_ar$pred
#18.94

wmodel <- ar.ols(mwh,  order.max = 5)
wpred_ar <- predict(wmodel, n.ahead = 1)
wpred_ar$pred

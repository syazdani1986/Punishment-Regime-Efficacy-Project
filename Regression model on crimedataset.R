# importing the crime data
rm (list = ls())
set.seed(1)

getwd() # where I am
setwd("C:/academic/Introduction to analytics modeling course/Fall 2019/hmw5")


uscrime <- read.table("uscrime.txt",stringsAsFactors = FALSE, header = TRUE)
# checho to make sure data is read correctly
head(uscrime)
library(corrplot)
corrplot(cor(uscrime[,1:16]))


#fit a linear reggression model using all of predictor variable
lm_uscrime<- lm(Crime~. , data=uscrime)


# for better accurate of the quality of the prediction
# buildingc cross validation model
library(DAAG)
install.packages("caret",dependencies = TRUE)
install.packages("quantreg")


library(caret)
data_ctrl <- trainControl(method = "cv", number = 5)
model_caret <- train(Crime~M+Ed+Po1+U2+Ineq+Prob ,   
                     data = uscrime,                        
                     trControl = data_ctrl,             
                     method = "lm",                      
                     na.action = na.pass)     

summary(model_caret)
#using cross validation cv.lm function with our previous linear model
set.seed(42)
lm_uscrime_cv <- cv.lm(uscrime, lm_uscrime, m=4)

summary(lm_uscrime_cv)

attr(lm_uscrime_cv, "ms")

#print summary of model
lm_uscrime
summary(lm_uscrime)
AIC(lm_uscrime)

#We know that null hypothesis for each predictor is that 
# the predictor is not significant which means its coefficient =0

#For using which predictor in our model we should consider 
#the related p value for making decision
# We also can infer from adjusted R-quared that how much variance in our data
# is explain by our model


# creating the test data point manually using data.frame() function.
test_point<- data.frame(M = 14.0, So = 0, Ed = 10.0 ,Po1 = 12.0 ,Po2 = 15.5
                        ,LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1,U1 = 0.120
                        ,U2 = 3.6, Wealth = 3200 ,Ineq = 20.1 ,Prob = 0.04
                        ,Time = 39.0)

# predict the crime rate for test data point.
pre_model <- predict(lm_uscrime, test_point)
pre_model 

pre_model1 <- predict(lm_uscrime_cv, test_point)

# to find out that if this is the good prediction 
qqnorm(uscrime$Crime)

# try more revised linear regression model only include predictor with significant P value

lm2_uscrime<- lm(Crime~M+Ed+Po1+U2+Ineq+Prob , data=uscrime)
summary(lm2_uscrime)
AIC(lm2_uscrime)
 
pre_model2 <- predict(lm2_uscrime, test_point)
pre_model2


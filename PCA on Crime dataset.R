rm(list = ls())
set.seed(1)

getwd() # where I am
setwd("C:/academic/Introduction to analytics modeling course/Fall 2019/hmw6")


uscrime <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)
head(uscrime)

# to check the coreltion in our dataset,
# we pick the variable that we used in our final model
library(ggplot2)
library(GGally)
ggpairs(uscrime, columns = c("M", "Ed", "Po1", "U2", "Ineq", "Prob"))
ggpairs(uscrime, columns = c("M","So","Ed","Po1","Po2","LF","M.F","Pop","NW","U1","U2","Wealth","Ineq","Prob","Time")) 

library(corrplot)
corrplot(cor(uscrime[,1:16]))
# we can use PCA for reducing the correlation among predictors,
#therefore we overcome the overfitting issue and will have simple model

# we have multicollinearity in our dataset some of our variables are correlated
#which affect coefficient value in our reggression model

# since PCA does not work well with binary data
#our column 2,"So", we can run PCA on the rest of data

# we run the PCA on the our scalled data set

PCA <- prcomp(uscrime[, 1:15], scale. = TRUE)
summary(PCA)

#PCA$rotation is the matrix of eigenvectors
PCA$rotation


# we can plot variance of each of the PCA to visualiza and
#to decide which PCA we should use.
par("mar")
par(mar = c(5,5,3,1))
screeplot(PCA, type = "lines", col= "blue")

prop_of_variance <- (PCA$sdev)^2/sum((PCA$sdev)^2) 
prop_variance_explained <- prop_of_variance/(sum(prop_of_variance)) 
plot(prop_variance_explained, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b") 

plot(cumsum(prop_variance_explained), xlab = "Principal Component",      ylab = "Proportion of Variance Explained",type = "b") 

# we select the first 6 principle componenet
PC <- PCA$x[,1:6]
PC
# these are our transfromed data, therefore we pick 15 factor and transdformed to 6 PCA

# we build the a linear regression model with the first principal components

uscrimePC <- cbind(PC, uscrime[, 16])
uscrimePC

modelPC <- lm(V7~., data = as.data.frame(uscrimePC))
summary(modelPC)

# Need to the eigenvectors to transform our PCI models back into  terms of the original factors: 
eigenvectors = PCA$rotation[,1:6]

# We will need these quantities to un-scale the original dataset: 
# We have the means and sd from the pca

mu <- PCA$center     
mu

sigma <- PCA$scale

# we get the beta from this PC regression model and transformit to alphas into 
# original SCALED vaiables and then unscaled the coeffient so that
#we get the original unscaled coeffient and then we can get the prediction for the new point data

# then we can compare the quality of this modele to previous homework model
# comparing adjusted R2

# Get betas from the model we just fit
b0_15 <- modelPC$coefficients[1] 
b0_15 
# (Intercept)  
#   905.0851 

#Transform to original coordinates 
b_vals_6 <- as.matrix(c(modelPC$coefficients[2], 
                        modelPC$coefficients[3],
                        modelPC$coefficients[4],
                        modelPC$coefficients[5],
                        modelPC$coefficients[6],
                        modelPC$coefficients[7])) 


a_vals_6 = PCA$rotation[,1:6] %*% b_vals_6/ sigma 
a0_15 <- b0_15 - sum(a_vals_6*mu) 


#predict the new data point and compare the previous predicted value in homework 5
#M = 14.0,So = 0, Ed = 10.0, Po1 = 12.0,
#Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150, 
#NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.040,Time = 39.0)


new_point <-data.frame(M = 14.0,So = 0, Ed = 10.0, 
                      Po1 = 12.0, Po2 = 15.5,LF = 0.640, 
                      M.F = 94.0, Pop = 150, NW = 1.1, 
                      U1 = 0.120, U2 = 3.6, Wealth = 3200,
                      Ineq = 20.1, Prob = 0.040,Time = 39.0)


# Compute prediction for "New City" Crime Rate (modelPC):
predicted_fit_PCI_15 = a0_15 + sum(a_vals_6 * new_point) 
predicted_fit_PCI_15 




library(DAAG)
lm.uscrime.cv_PC_5.cv <- cv.lm(modelPC$model, modelPC, m = 5, seed = 99) 
ms_PC_5 <- attr(lm.uscrime.cv_PC_5.cv, "ms") 
ms_PC_5 # 72775
# Degrees of freedom for cross validation (same regardless of # of PCIs) 
df <- attr(lm.uscrime.cv_PC_5.cv, "df") 
df 
# Sum of Squares: 
ss_PC_5 <- ms_PC_5 * df 
ss_PC_5 

# Calculate SST for uscrime$Crime (independent of PCIs; based on original dataset, not model): 
SST <- sum((uscrime$Crime - mean(uscrime$Crime))**2) 
SST # 6880928 

# The Cross-Validated R-squared is: 
rsquared_PC_5.cv <- 1 - (ss_PC_5 / SST) 
rsquared_PC_5.cv 
# 0.503
# Compare to full (non-cross-validated) model's multiple r-squared: # 0.6452 

# The Cross-Validated Adjusted R-Squared is (with n = 47 observations and # PCIs = 6):
adj.rsquared_PC_5.cv <- 1 - (((47 - 1)/(47 - 6 - 1)) * (1 - rsquared_PC_5.cv))
adj.rsquared_PC_5.cv 
11 

#new point predcition with CV model:


b0_15 <- lm.uscrime.cv_PC_5.cv $coefficients[1] 
b0_15 
# (Intercept)  
#   905.0851 

#Transform to original coordinates 
b_vals_6 <- as.matrix(c(lm.uscrime.cv_PC_5.cv$coefficients[2], 
                        lm.uscrime.cv_PC_5.cv$coefficients[3],
                        lm.uscrime.cv_PC_5.cv$coefficients[4],
                        lm.uscrime.cv_PC_5.cv$coefficients[5],
                        lm.uscrime.cv_PC_5.cv$coefficients[6],
                        lm.uscrime.cv_PC_5.cv$coefficients[7])) 


a_vals_6 = PCA$rotation[,1:6] %*% b_vals_6/ sigma 
a0_15 <- b0_15 - sum(a_vals_6*mu) 




# Compute prediction for "New City" Crime Rate (modelPC):
predicted_fit_PCI_15 = a0_15 + sum(a_vals_6 * new_point) 
predicted_fit_PCI_15 

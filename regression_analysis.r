install.packages("ggfortify")
install.packages("ggplot2")

# Load libraries ----------------------------------------------------------
library(dplyr)
library(tidyverse)
library(rstatix)
library(ggplot2)
library(bestglm)
library(olsrr)
library(MASS)
library(ggfortify)
library(ggplot2)

path = "C:/Users/amfar/OneDrive/Documents/ICS/project_3/Bikedata.csv"
data = read.csv(path)
data.df <- as.data.frame(data)
#data.df <- data.df[,-1]

colnames(data.df)
head(data.df)
str(data.df)
dim(data.df)

#Check for missing data:  missing values
sum(is.na(data.df))
data.df[rowSums(is.na(data)) > 0, ]  

#Dummy coding
data.df$Holiday <- as.factor(data.df$Holiday)
data.df$Seasons <- as.factor(data.df$Seasons)

summary(data.df)

categorical_variables <- data.df[c("Holiday", "Seasons")]
summary(categorical_variables)
xtable::xtable(summary(categorical_variables))

#full model
mModel = lm(log.Rented.Bike.Count~Hour + Temperature + Humidity + Wind.speed  + Visibility + Solar.Radiation +  Rainfall +  Snowfall + Holiday + Seasons , data = data)
(summary(mModel))

#check for multicollinearity (VIF)
car::vif(mModel)
#vif(mModel)
ols_vif_tol(mModel)
#car::vif(ols_vif_tol(mModel))
#xtable::xtable(ols_vif_tol(mModel))

library(magrittr)

#correlation plot
library(corrplot)

# Calculate the correlation matrix
cor_matrix <- cor(data.df[1:9])

# Visualize the correlation matrix with values
corrplot::corrplot(cor_matrix, method = "circle", addCoef.col = "black", number.cex = 0.6)

#Best model using AIC in a forward stepwise algorithm
ols_best_models = ols_step_best_subset(mModel)
ols_best_models

AIC_stepwise = stepAIC(mModel, direction = "forward")
summary((AIC_stepwise))
xtable::xtable(summary((AIC_stepwise)))

AIC_model = lm(AIC_stepwise)
sum  <- summary(AIC_model)
round(sum$coefficients, 3)

AIC_value = AIC (AIC_model)
AIC_value

bestsubSetModel =lm(log.Rented.Bike.Count~Hour + Temperature + Humidity + Wind.speed +  Rainfall  + Holiday + Seasons , data = data)
summary(bestsubSetModel)

res<- data.df$log.Rented.Bike.Count - bestsubSetModel$fitted.values
plot(bestsubSetModel$fitted.values, res, xlab = ("Fitted model"),
     ylab = ("Residuals"), cex = 0.7, main = "",  cex.lab = 1.25)
abline(0,0,col="blue")

qqnorm(res,main="", xlab="Theoretical Quantiles", ylab= "Residuals", cex=0.77, cex.lab = 1.25)
qqline(res, col="blue")

#confidence intervals
conf <- confint(bestsubSetModel, data = df, level = 0.95)
bestsubSetModel$xlevels
round(conf,3)
xtable::xtable(conf)
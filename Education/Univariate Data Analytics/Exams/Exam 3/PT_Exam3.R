#########################################
# This code was created by Paul Tomosky #
# MATH 411                 LKXT@iup.edu #
# Exam 3                       12/16/18 #
#########################################

# Libraries #############################

library(tidyverse)
library(car)
library(dplyr)
library(plyr)
library(leaps)
library(ggplot2)
library(GGally)
library(ggfortify)
library(aod)
library(ROCR)
library(nnet)
library(rms)
library(rJava)
library(glmulti)
library(DAAG)
library(mlr)
library(FSelector)
library(caret)
library(devtools)
library(e1071)

# Variable Summary:
# game     = Game year and game number
# kicker   = name
# success  = response varaible
# distance = length in yards
# weather  = levels: clouds, inside, SnowRain, Sun
# wind 15  = if wind > 15 mph and outside
# temp     = levels: nice, cold, hot
# grass    = kick on grass or not (1,0)
# pressure = Y if last 3 min of game
# iced     = if pressure = 1 AND time-out called beforehand


# Loading the Data:
Kick = read_csv(file.choose())
    # Kick %>% View()
    # summary(Kick)

# Format the Data:                      
Kick = Kick %>% 
  select(success, distance, weather, wind15, temperature, grass, pressure, iced)
Kick$success[Kick$success=="Y"] <- "1" 
Kick$success[Kick$success=="N"] <- "0" 
Kick$success = as.integer(Kick$success)
Kick$weather = as.factor(Kick$weather)
Kick$weather = as.integer(Kick$weather)
Kick$temperature = as.factor(Kick$temperature)
Kick$temperature = as.integer(Kick$temperature)
Kick$pressure[Kick$pressure=="Y"] <- "1" 
Kick$pressure[Kick$pressure=="N"] <- "0" 
Kick$pressure = as.integer(Kick$pressure)
Kick = as.data.frame(Kick)

#Scatter plot Matrix:
    # ggpairs(Kick) 

# Split into traing and test data:
set.seed(2018667)
trainid = sample(1:nrow(Kick), floor(0.8*nrow(Kick)))
train = Kick[trainid,]
test = Kick[-trainid,]


# Full Model:
fullmodel = glm(success ~ . , data = train, family = "binomial")
    # summary(fullmodel)


# Automated Model Selection:
glmulti.logistic.out =
  glmulti(success ~ distance + weather + wind15 + temperature + grass + pressure + iced,
          data        = train,
          level       = 2,         # Interaction considered (2nd order model)
          method      = "i",       # d: diag / h: exhaustive / i:quick
          crit        = "aic",     # AIC as criteria
          confsetsize = 3,         # Keep 5 best models
          plotty      = F, 
          report      = T,         # No plot or interim reports
          fitfunction = "glm",     # glm function
          chunks      = 8,
          family      = binomial)  # binomial family for logistic regression

glmulti.logistic.out@formulas

# creating the selected models:
mod1 = glm(success ~ 1 + distance + weather + iced + wind15:weather + grass:distance + 
             pressure:grass + iced:distance,
           family = binomial,
           data   = train)
    # summary(mod1)

mod2 = glm(success ~ 1 + distance + weather + iced + wind15:weather + grass:distance + 
             pressure:grass + iced:distance + iced:pressure,
           family = binomial,
           data   = train)
    # summary(mod2)

mod3 = glm(success ~ 1 + distance + iced + wind15:weather + grass:distance + 
             pressure:grass + iced:distance,
           family = binomial,
           data   = train)
    # summary(mod3)

    # AIC(fullmodel, mod1, mod2, mod3):
        # fullmodel   1483.814
        # mod1        1476.187
        # mod2        1476.187
        # mod3        1476.235


cv.binary(fullmodel)
cv.binary(mod1)
cv.binary(mod2)
cv.binary(mod3)
    # based on this and the AIC model 1 will be selected:
    # AIC = 1476 
    # Internal estimate of accuracy = 0.785
    # Cross-validation estimate of accuracy = 0.779


# prediction:
pred = predict(mod1, test)
mse = mean((test$success - pred)^2)
    # MSE 1.55
test$pred = pred


# create Confusion Matrix:
prob = predict(mod1, newdata = test, type = "response")
classify = ifelse(prob >= 0.75, 1, 0)
classify
test$classify = classify


confusionMatrix(as.factor(test$classify), 
                as.factor(test$success), 
                positive = "1", 
                mode = "everything")


write.csv(test, file = "C:\\Users\\tomos\\Desktop\\MATH 411\\Exam 3\\PT_testout.csv")









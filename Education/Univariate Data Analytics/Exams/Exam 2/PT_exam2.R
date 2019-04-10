#########################################
# This code was created by Paul Tomosky #
# MATH 411                 LKXT@iup.edu #
# Exam 2                       11/16/18 #
#########################################

# Libraries #############################

library(tidyverse)
library(car)
library(GGally)
library(ggfortify)
library(leaps)
library(ellipse)
library(DMwR)
library(ggplot2)


#########################################
#              PROBLEM 1                #
#########################################


# Load and view data
DLCO = read_csv(file.choose())
DLCO %>% View()
summary(DLCO)


# Change sex into factor variable
DLCO$Sex = as.factor(DLCO$Sex)

# View data distribution
ggpairs(DLCO)
#plotcorr(cor(DLCO))

# Testing default Linear Model

DLCO_fit_default_data = lm(DLCO ~ DLCO + Sex + Age + Height, data = DLCO)
summary(DLCO_fit_default_data)

autoplot(DLCO_fit_default_data, which = 1:2, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue')
                                                                  # normal Q-Q is slightly skewed

# Fix age

DLCO = DLCO %>%
  mutate(age_log = log(Age))        # This fixes some of the skew (enough?)
DLCO = DLCO %>%
  mutate(age_log_squared = (age_log)^2)        # This fixes some of the skew (enough?)




ggpairs(DLCO)

# not useful transformations:

# DLCO = DLCO %>%
#   mutate(age_sqrt= sqrt(Age))
# DLCO = DLCO %>%
#   mutate(age_sqrt_log= log(age_sqrt))
# DLCO = DLCO %>%
#   mutate(age_sqrt_log_sqrd= age_sqrt_log^2)
# DLCO = DLCO %>%
#   mutate(Height_squared = (Height)^2)
# DLCO = DLCO %>%
#   mutate(Height_log = log(Height))
# DLCO = DLCO %>%
#   mutate(DLCO_squared = (DLCO)^2)
# DLCO = DLCO %>%
#   mutate(DLCO_log = log(DLCO))


# Main Linear Model
DLCO_fit_1 = lm(DLCO ~ ., data = DLCO)
summary(DLCO_fit_1) #R^2 = 0.67 ==== too low

autoplot(DLCO_fit_1, which = 1:2, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue')


# Use bestSubs to pick test models <<<< Dont think this is necessary
bestsubs = regsubsets(x = DLCO ~ ., data = DLCO, method = "exhaustive", nbest = 1)
summary(bestsubs) # This gives what models to test


# Using Training Data

set.seed(2018411)

trainid = sample(1:nrow(DLCO), floor(0.8*nrow(DLCO)))
train = DLCO[trainid,]
test = DLCO[-trainid,]

# checking train and test data

train %>% View()
test %>% View()


mod1 = lm(DLCO ~ Sex + Age, data = train)
mod2 = lm(DLCO ~ Sex + Age + Height, data = train)
mod3 = lm(DLCO ~ Sex + Age + Height + age_log, data = train)
mod4 = lm(DLCO ~ Sex + Age + Height + age_log + age_log_squared, data = train)

AIC(mod1, mod2, mod3, mod4) # Model 3 has the lowest value


summary(mod1) #R^2 = 0.60
summary(mod2) #R^2 = 0.65 
summary(mod3) #R^2 = 0.67 # Model 3 has the highest value
summary(mod4) #R^2 = 0.66 


pred1 = predict(mod1, test)
pred2 = predict(mod2, test)
pred3 = predict(mod3, test)
pred4 = predict(mod4, test)



regr.eval(test$DLCO, pred1) # rmse = 51.00
regr.eval(test$DLCO, pred2) # rmse = 47.60
regr.eval(test$DLCO, pred3) # rmse = 47.60
regr.eval(test$DLCO, pred4) # rmse = 47.71


# Based on the r2 and RMSE values, the best model is 3

final_predict = predict(mod3, DLCO)

DLCO_predicted = DLCO %>%
  mutate(Preditor_Value = final_predict)

#output the CSV File

write.csv(DLCO_predicted, file = "c:\\Users\\tomos\\Desktop\\MATH 411\\Exam 2\\PT_DLCO_Predicted.csv", row.names = FALSE)


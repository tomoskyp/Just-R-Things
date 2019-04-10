#########################################
# This code was created by Paul Tomosky #
# MATH 411                 LKXT@iup.edu #
# Homework 3                   10/29/18 #
#########################################

# Libraries #############################

library(tidyverse)
library(car)
library(ggpmisc)
library(gridExtra)
library(GGally)
library(ggfortify)


#########################################
#              PROBLEM 1                #
#########################################


# SET-UP ################################

conconi = load(file.choose())
conconi %>% View()

# 1-A    ################################

conconi %>%
  ggplot(aes(x = speed, y = puls))+
  geom_point(color = "orange", size = 4)+
  labs(title    = "The relationship between speed and pulse",
       y = "Pulse",
       x = "Speed (Km/h)")+
    geom_smooth(method = "lm", se = FALSE, size = 1)

# 1-B    ################################

slr_conconi = lm(puls ~ speed, data = conconi)
summary(slr_conconi)
confint(slr_conconi, level = 0.95)


#########################################
#
#      ANSWERS:
#
# b.1) Speed is the predictor vairable and pulse in the target variable
#
# b.2) I think this can be explained by other variables like height, weight
#      fitness, age, and others not measured in the data. The data is highly
#      coorelated with an R-squared value of 0.98.
#
# b.3) For every +1 km/h incease in speed, the pulse increases by 6.4070. Based on
#      this model, this seems like a plausable answer. Given the data, we can be 95% 
#      confident that the speed increase is between 6.02 and 6.79
#
# b.4) The resting heart rate is 86.6105. This is plausable becuase this is a normal
#      rate for someone who is at rest. Given the data, we can be 95% confident that
#      the resting heart rate (the intercept) is between 81.2 and 91.6
#
#########################################

# 1-C    ################################

autoplot(slr_conconi, which = 1:6, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue',
         ncol = 3)



#########################################
#
#    ANSWERS:
#
# C) The regression line captures the relation correctly
#
#    The varaince of the error is constant
#
#    The errors follow a Nomral distribution
#
#########################################




##################################################################################




#########################################
#              PROBLEM 2                #
#########################################

# 2-A    ################################

gas = load(file.choose())
gas %>% View()

gas %>%
  ggplot(aes(x = temp, y = verbrauch))+
  geom_point(color = "orange", size = 4)+
  labs(title    = "The relationship between Temperature and Consumption",
       y = "consumption (kWh)",
       x = "temperature (C)")+
  geom_smooth(method = "lm", se = FALSE, size = 1)

gas_slr = lm(verbrauch ~ temp, data = gas)

coef(gas_slr)
summary(gas_slr)

autoplot(gas_slr, which = 1:6, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue',
         ncol = 3)

confint(gas_slr, level = 0.95)

ggplot(gas_slr, aes(x = gas_slr$resid)) +
  geom_histogram(aes(y = ..density..), fill = "darkblue")+ 
  scale_x_continuous(breaks = round(seq(min(gas_slr$resid), max(gas_slr$resid), by = 100), 2))+
  labs(title    = "Histogram of the Residuals",
       y = "Density",
       x = "Gas")


#########################################
#
#    ANSWERS:
#
#    The Q-Q plot shows that the error is slightly skewed in the Q-Q plot,
#    but since the error is mostly normal, this model could be good at indicating  
#    trends but have a wide condifence interval
# 
#    The histogram of the residuals shows a clear skew to the left. 
# 
#
#
##########################################




# 2-B    ################################

antikeUhren = load(file.choose())
antikeUhren %>% View()

antikeUhren %>%
  ggplot(aes(x = Alter, y = Preis))+
  geom_point(color = "orange", size = 4)+
  labs(title    = "The relationship between Age and Price",
       y = "Price",
       x = "Age")+
  geom_smooth(method = "lm", se = FALSE, size = 1)

antikeUhren_slr = lm(Alter ~ Preis , data = antikeUhren)

coef(antikeUhren_slr)
summary(antikeUhren_slr)

autoplot(antikeUhren_slr, which = 1:6, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue',
         ncol = 3)

confint(gas_slr, level = 0.95)

ggplot(antikeUhren_slr, aes(x = antikeUhren_slr$resid)) +
  geom_histogram(aes(y = ..density..), fill = "darkblue")+ 
  scale_x_continuous(breaks = round(seq(min(antikeUhren_slr$resid), max(antikeUhren_slr$resid), by = 100), 2))+
  labs(title    = "Histogram of the Residuals",
       y = "Density",
       x = "Antique Clocks")


#########################################
#
#    ANSWERS:
#
#    The Q-Q plot shows that the error is normal,
#    since the error is mostly normal, this model could be good at indicating  
#    trends but have a wide condifence interval. 
#
#    The histogram of the residuals shows a clear skew to the right. 
#
#
##########################################





#########################################
#              PROBLEM 3                #
#########################################


interval = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
bacteria = c(255, 211, 197, 166, NA, 106, 104, 60, 56, 38, 36, 32, 21, 19, 15)

bacteria_experiment = data.frame(interval, bacteria)


bacteria_experiment %>%
  ggplot(aes(x = interval, y = bacteria))+
  geom_point(color = "orange", size = 4)+
  labs(title    = "The relationship between Time and Bacteria",
       y = "bacteria",
       x = "interval")+
  geom_smooth(method = "lm", se = FALSE, size = 1)


bacteria_experiment_slr = lm(bacteria ~ interval , data = bacteria_experiment)
coef(bacteria_experiment_slr)
summary(bacteria_experiment_slr)
confint(bacteria_experiment_slr, level = 0.95)

autoplot(bacteria_experiment_slr, which = 1:6, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue',
         ncol = 3)


bacteria_experiment_mutated = bacteria_experiment %>%
  mutate(log_interval = log(interval),
         log_bacteria = log(bacteria))

bacteria_experiment_mutated %>%
  ggplot(aes(x = log_interval, y = bacteria))+
  geom_point(color = "orange", size = 4)+
  labs(title    = "The relationship between Time (Log) and Bacteria",
       y = "Bacteria",
       x = "Time (log)")+
  geom_smooth(method = "lm", se = FALSE, size = 1)

bacteria_experiment_mutated_slr = lm(bacteria ~ log_interval, data = bacteria_experiment_mutated)
coef(bacteria_experiment_mutated_slr)
summary(bacteria_experiment_mutated_slr)
confint(bacteria_experiment_mutated_slr, level = 0.95)

predict(bacteria_experiment_mutated_slr, data.frame(log_interval = 1.6094379))


#########################################
#
#    ANSWERS:
#
#    a)  The data shows a slight curve, 
#        it could potentially benefit from the OLS algorithm.
#
#    d1) The predicted missing value for the 5th index is 120.56
#
#    d2) The predicted interval before the first interval is 279.80
#
#
#########################################

######################################## replace and remove


gas_mutated_slr = lm(log_temp ~ log_verbrauch, data = gas_mutated)
coef(gas_mutated_slr)
summary(gas_mutated_slr)


#taking the log of TEMP
logfit_temp_gas = lm(log(temp) ~ verbrauch, data = gas)
#summary(logfit_temp_gas)
coef(logfit_temp_gas)

logfit_verbrauch_gas = lm(temp ~ log(verbrauch), data = gas)
#summary(logfit_verbrauch_gas)
coef(logfit_verbrauch_gas)

logfit_both_gas = lm(log(temp) ~ log(verbrauch), data = gas)
#summary(logfit_both_gas)
coef(logfit_both_gas)
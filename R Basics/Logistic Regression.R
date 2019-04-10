library(tidyverse)

wolf = read_csv(file.choose())

dim(wolf)

wolf %>% View()

### Make a new variable 'Landtype'  

wolf = wolf %>% 
        mutate(Landtype = case_when(MAJOR_LC > 20 & MAJOR_LC < 24 ~ "Developed",
                                    MAJOR_LC > 30 & MAJOR_LC < 34 ~ "Barren",
                                    MAJOR_LC > 40 & MAJOR_LC < 44 ~ "Forest",
                                    MAJOR_LC == 51                ~ "Shrubland",
                                    MAJOR_LC == 71                ~ "Grassland",
                                    TRUE                          ~ "Agriculture"))
### Look at the data again

wolf %>% View()

### Make a samller data set that contains 
### only the 153 grid cells in 1999. 
### Call it wolf99

wolf99 = wolf %>% filter(WOLVES_99 != 2)

dim(wolf99)

wolf99 %>% View()

### probablity to odds to log odds:

pol = wolf99 %>%
       group_by(WOLVES_99) %>%
        summarise(freq    = n(), 
                  prob    = (n()/nrow(.)), 
                  odds    = prob/(1-prob), 
                  logodds = log(odds)) %>%
        round(4)

#install.packages("pander")

library(pander)
pander(pol)

### Plot the 1999 data to generate similar 
### graph (Figure 2) presented in the paper

#install.packages("maptools")
#install.packages("rgdal") 

library(maptools)
library(rgdal)

statesPolys = readOGR(file.choose()) 
 
class(statesPolys)
names(statesPolys)

statesPolys$WOLVES_99

### Set the color for present or not

cols0 = c("green", "red") 

w99 = statesPolys[statesPolys$WOLVES_99 != 2, ]

w99$WOLVES_99 = as.factor(w99$WOLVES_99)

spplot(w99, 
       "WOLVES_99", 
       names.attr  = "WOLVES_99", 
       col.regions = rev(cols0), 
       colorkey    = list(space = "bottom"), 
       main        = "1999 Wolf Presence in Grid Cell", 
       scales      = list(draw = TRUE), 
       as.table    = TRUE)

### Your turn: make a similar graph to 
### plot the 2001 data

w01 = statesPolys

w01$WOLVES_01 = as.factor(w01$WOLVES_01)

spplot(w01, 
       "WOLVES_01", 
       names.attr  = "WOLVES_01", 
       col.regions = rev(cols0), 
       colorkey    = list(space = "bottom"), 
       main        = "2001 Wolf Presence in Grid Cell", 
       scales      = list(draw = TRUE), 
       as.table    = TRUE)

### Fit the model without predictor 

f0 = glm(WOLVES_99 ~ 1, data = wolf99, family = binomial)
summary(f0)

### The above model is actually 
### 1-prop Z test you have seen before:)

prop.test(x = sum(wolf99$WOLVES_99), 
          n = length(wolf99$WOLVES_99), 
          correct = FALSE)

### Your turn: plot `WOLVES_99` vs `RD_DENSITY`
### ALso show what's gonna happen if you fit a SLR

wolf99 %>%
  ggplot(aes(x = RD_DENSITY, y = WOLVES_99))+ 
  geom_point(size = 5, color = "cornflowerblue")+
  geom_smooth(method = "lm", color = "red", size = 2)+
  labs(title     = "Wolf Presence VS Road Density",
               x = "Road Density",
               y = "Wolf Presence (1)",
         caption = "Graph by ycao@iup.edu, Fall 2018")+
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         plot.title = element_text(hjust = 0, size = 16),
         plot.subtitle = element_text(hjust = 0, size = 12),
         plot.caption = element_text(hjust = 1, size = 12))

### Reproduce Figure 3

wolves = wolf99 %>%
 filter(WOLVES_99 == 1) %>%
 ggplot(aes(x = RD_DENSITY)) +
  geom_density(fill = "mediumseagreen", alpha = .6, color = "white") +
  geom_vline(aes(xintercept = mean(RD_DENSITY)), 
             color = "dodgerblue", lty = 3, size = 1) +
  scale_x_continuous(breaks = seq(0, 3.3, by = 0.3))+
  annotate("text", 
           label = "Mean = 0.58", 
           x     = 0.8, 
           y     = 0.93, 
           color = "dodgerblue", 
           size  = 6) +
  labs(title    = "Distribution of RD_DENSITY When Wolf Presence",
       subtitle = "",
              x = "RD_DENSITY",
              y = "Density",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

nowolves = wolf99 %>%
 filter(WOLVES_99 == 0) %>%
 ggplot(aes(x = RD_DENSITY)) +
  geom_density(fill = "mediumseagreen", alpha = .6, color = "white") +
  geom_vline(aes(xintercept = mean(RD_DENSITY)), 
             color = "dodgerblue", lty = 3, size = 1) +
  scale_x_continuous(breaks = seq(0, 3.3, by = 0.3))+
  annotate("text", 
           label = "Mean = 0.9", 
           x     = 1.2, 
           y     = 0.75, 
           color = "dodgerblue", 
           size  = 6) +
  labs(title    = "Distribution of RD_DENSITY When Wolf Absence",
       subtitle = "",
              x = "RD_DENSITY",
              y = "Density",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

library(gridExtra)
grid.arrange(wolves, nowolves, ncol = 2)

### Fit the model with road density as the predictor 

f1 = glm(WOLVES_99 ~ RD_DENSITY, data = wolf99, family = binomial)
summary(f1)

### predict() function

predict(f1, newdata = data_frame(RD_DENSITY = c(0.58, 0.9)))

predict(f1, newdata = data_frame(RD_DENSITY = c(0.58, 0.9)), type = "response")

### Plotting a Simple Logistic Regression Model ###

### Let's plot the logistic regression model `f1` 
### for `presence` using the extent of the 
### RD_DENSITY in terms of probabilities. 
### We can use either of two different approaches:

### 1. we can plot the fitted values from our 
### specific model against the original data, 
### using the `augment` function from the `broom` package, or

### 2. we can create a smooth function called 
### `binomial_smooth` that plots a simple 
### logistic model in an analogous way to 
### `geom_smooth(method = "lm")` for a SLR.

### Method 1

library(broom)

f1_aug = augment(f1, wolf99, 
                 type.predict = "response")

f1_aug

f1_aug = f1_aug %>%
          mutate(lower = .fitted - 1.96*.se.fit,
                 upper = .fitted + 1.96*.se.fit)

f1_aug %>% 
  ggplot(aes(x = RD_DENSITY, y = WOLVES_99)) +
   geom_jitter(height = 0.05, size = 5)+
   geom_line(aes(x = RD_DENSITY, y = .fitted), size = 2, col = "red")+
   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "green") +
   labs(x = "Road Density", 
        y = "P(presence)", 
        title = "Confidence Interval for Predicted probabilties")+
   theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))


### Method 2

binomial_smooth = function(...) {
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), ...)
}

wolf99 %>%
  ggplot(aes(x = RD_DENSITY, y = WOLVES_99))+ 
  geom_point(size = 5, color = "cornflowerblue")+
  binomial_smooth(size = 2, color = "red")+
  labs(title     = "Wolf Presence VS Road Density",
               x = "Road Density",
               y = "Wolf Presence (1)",
         caption = "Graph by ycao@iup.edu, Fall 2018")+
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         plot.title = element_text(hjust = 0, size = 16),
         plot.subtitle = element_text(hjust = 0, size = 12),
         plot.caption = element_text(hjust = 1, size = 12))

### Interpreting the Model Summary

f1 %>% summary()

coef(f1)
confint(f1)

exp(coef(f1))
exp(confint(f1))

### Logistic regression with continuous and 
### categorical predictors ###

### Visualize the relationship between 
### the response and "Landtype"

wolf99 %>% 
 ggplot(aes(x = Landtype, fill = as.factor(WOLVES_99)))+ 
  geom_bar()+
  labs(title    = "Wolf Presence in Different Land Type",
              x = "Land Type",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### Take a look at the type of the Landtype 
### variable, is it a factor?

str(wolf99$Landtype)

### Turn it into a factor
### it is not required, but definitely helpful

wolf99$Landtype = as.factor(wolf99$Landtype)

### check to see which is the reference group 

levels(wolf99$Landtype)

### What it implies is 

glm(WOLVES_99 ~ RD_DENSITY + Landtype, data = wolf99, family = binomial)

### Set 'Forest' to be the reference group

wolf99$Landtype = relevel(wolf99$Landtype, ref = "Forest")

### Fit the model

f2 = glm(WOLVES_99 ~ RD_DENSITY + Landtype, data = wolf99, family = binomial)
summary(f2)

### Plot the fitted model by land type
### with method 1 as discussed above

f2_aug = augment(f2, wolf99, 
                 type.predict = "response")

f2_aug

f2_aug = f2_aug %>%
          mutate(lower = .fitted - 1.96*.se.fit,
                 upper = .fitted + 1.96*.se.fit)

f2_aug %>% 
  ggplot(aes(x = RD_DENSITY, y = WOLVES_99, colour = Landtype, fill = Landtype)) +
   geom_jitter(height = 0.05, size = 5)+
   geom_line(aes(x = RD_DENSITY, y = .fitted), size = 2)+
   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
   labs(x = "Road Density", 
        y = "P(presence)", 
        title = "Confidence Interval for Predicted probabilties")+
   theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### Compare the two models in terms of fitting the data 

anova(f1, f2)

anova(f1, f2, test = "LRT")

anova(f1, f2, test = "Chisq")

##############################################
##### From probability to classification #####
##############################################

prob99 = predict(f2, newdata = wolf99, type = "response")
classify99 = ifelse(prob99 >= 0.5, 1, 0)
classify99

### Add classify99 to w99 data 
### in order to make confusion matrix
### and to reproduce Figure 4

w99$classify99 = classify99 

#install.packages(c("caret", "e1071"))

### Build confusion Matrix

library(caret)

confusionMatrix(as.factor(w99$classify99), 
                as.factor(w99$WOLVES_99), 
                positive = "1", 
                mode = "everything")

### Compare it to Figure 4 A
### Accuracy : 0.719 

mean(w99$classify99 == w99$WOLVES_99)  

### TPR or Sensitivity

TPR = 48/(48+13)

### TNR or Specificity

TNR = 1408/(1408+43)
FPR = 1-TNR

confusionMatrix(as.factor(w99$classify99), 
                as.factor(w99$WOLVES_01), 
                positive = "1", 
                mode = "everything")

### Plot ROC curve

#install.packages("InformationValue")

library(InformationValue)

plotROC(w99$WOLVES_99, prob99)

### Reproduce Figure 4 A and B

w99$classify99 = as.factor(w99$classify99)

spplot(w99, 
       "classify99", 
       names.attr  = "classify99", 
       col.regions = rev(cols0), 
       colorkey    = list(space = "bottom"), 
       main        = "Predicted Wolf Presence From f2, 1999", 
       scales      = list(draw = TRUE), as.table = TRUE)

x11()

spplot(w99, 
       "WOLVES_01", 
       names.attr  = "WOLVES_01", 
       col.regions = rev(cols0), 
       colorkey    = list(space = "bottom"), 
       main        = "Observed Wolf Presence in 1999 Grids in 2001", 
       scales      = list(draw = TRUE), as.table = TRUE)

###### Use f2 to predict 2001 Presence ######

### Prepare the 2001 data

levels(wolf99$Landtype)

wolf01 = wolf %>% 
          mutate(Landtype = as.factor(Landtype))

levels(wolf01$Landtype)

### So the levels do not match
### What's gonna happen if we simply predict

predict(f2, newdata = wolf01, type = "response")

table(wolf01$Landtype)

### Remove the two levels 

wolf01.reduced = wolf01 %>%
                  filter(Landtype != "Barren" & Landtype != "Developed")

### Obtain Predicted Probabilities 

prob01 = predict(f2, newdata = wolf01.reduced, type = "response")

### Use 0.5 as the cutoff point

classify01.5 = ifelse(prob01 >= 0.5, 1, 0)
classify01.5

### Confusion Matrix

caret::confusionMatrix(as.factor(classify01.5), 
                       as.factor(wolf01.reduced$WOLVES_01), 
                       positive = "1", 
                       mode = "everything")


### Compare results in Figure 5

plotROC(wolf01.reduced$WOLVES_01, prob01)

### Use 0.5 as the cutoff point

classify01.4 = ifelse(prob01 >= 0.4, 1, 0)
classify01.4

### Confusion Matrix

caret::confusionMatrix(as.factor(classify01.4), 
                       as.factor(wolf01.reduced$WOLVES_01), 
                       positive = "1", 
                       mode = "everything")

### Is there another choice
### Choose the best cutoff point

library(ROCR)

pred01 = prediction(prob01, wolf01.reduced$WOLVES_01)
pred01

test01 = performance(pred01, 'tpr', 'tnr')
test01

plot(test01@alpha.values[[1]], 
     test01@x.values[[1]], 
     type = 'n', 
     xlab = 'cutpoint',
     ylab = 'Sensitivity/Specificity')
lines(test01@alpha.values[[1]], 
      test01@y.values[[1]], 
      col = "red", 
      lwd = 6)
lines(test01@alpha.values[[1]],
      test01@x.values[[1]], 
      col = "blue", 
      lty = 3, 
      lwd = 6)
legend(0.5, 0.8, c('sensitivity', 'specificity'), 
       lty = c(2,2), 
       col = c("red", "blue"), 
       cex = c(0.9, 0.9), bty = 'n')
abline(v = 0.33, col = "green", lwd = 3)
abline(h = 0.79, col = "green", lwd = 3)

### Now make the classfications with cutoff 0.33

classify01.33 = ifelse(prob01 >= 0.33, 1, 0)
classify01.33

### Confusion Matrix

caret::confusionMatrix(as.factor(classify01.33), 
                       as.factor(wolf01.reduced$WOLVES_01), 
                       positive = "1", 
                       mode = "everything")


### Finally plot the results on map

### Plot the predicted probabilities 

w01 = subset(statesPolys, !(as.numeric(as.character(statesPolys$MAJOR_LC)) > 20 & 
                            as.numeric(as.character(statesPolys$MAJOR_LC)) < 34))
w01$prob01 = prob01

cols = heat.colors(100) 

spplot(w01, 
       "prob01", 
       names.attr = "Probability", 
       col.regions = rev(cols),
       colorkey = list(space = "bottom"), 
       main     = "Probability of Wolf Presence in 2001", 
       scales   = list(draw = TRUE), as.table = TRUE)

### Plot the classifications 

w01$classify01.33 = classify01.33
w01$classify01.33 = as.factor(w01$classify01.33)
w01$WOLVES_01 = as.factor(w01$WOLVES_01)

cols0 = c("green", "red")

spplot(w01, 
       "classify01.33", 
       names.attr = "Classification", 
       col.regions = rev(cols0), 
       colorkey = list(space = "bottom"), 
       main     = "Predicted Wolf Presence in 2001", 
       scales   = list(draw = TRUE), as.table = TRUE)
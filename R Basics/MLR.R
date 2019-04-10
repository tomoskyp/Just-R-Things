########## Multiple Linear Regression ##################

library(tidyverse)

### Read in the data

rich = read_csv(file.choose())

###  Take a look at the data 

### Make a subset data that only contains the 4 variables and in the 
### order of y, x1, x2, and x3

rich = rich %>%
        select(srich, discharge, area, propdriest)

### look at the data again

### Scatterplot Matrices from the car Package

library(car)
scatterplotMatrix(rich, data = rich)

library(GGally)

my_sm = function(data, mapping, ...){
  p = ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method = loess, fill = "red", color = "red", ...) +
    geom_smooth(method = lm, fill = "blue", color = "blue", ...)
  p
}

g = ggpairs(rich, columns = 1:4, upper = list(continuous = my_sm))
g

### Matrix of scatterplots to check out: 

### 1. Functional relationship between the response and each of the regressors and 
### 2. `Collinearity` (linear association between predictors)

### In R, MLR is carried out with command `lm()`.

fit0 = lm(srich ~ discharge + area + propdriest, data = rich)
summary(fit0)

### It is worth noting that there is a simple variant of specifying 
### regression problems with many predictors in R. 
### The notation lm(y ~ ., data = data_name)

fit0.1 = lm(srich ~ ., data = rich)
summary(fit0.1)

########## Model Diagnostics ##################

plot(fit0)
plot(fit0, which = 1:6)

### Or

library(ggfortify)

### You can simply do

autoplot(fit0)

### improved 

autoplot(fit0, which = 1:6, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue',
         ncol = 3)


#########################################################
##################### Model Selection  ##################
#########################################################

library(tidyverse)

rich = read_csv(file.choose())

rich = rich %>%
        select(srich, discharge, area, propdriest)

rich = rich %>%
        mutate(logdischarge = log(discharge),
               logarea      = log(area))

library(car)
scatterplotMatrix(rich)

rich = rich %>% 
        mutate(logdis2  = logdischarge^2,
               logarea2 = logarea^2)

rich 

### Initial Model

fit0 = lm(srich ~ discharge + area + propdriest, data = rich)

########## Testing for a single predictor ##################

### Page 18 in Chapter 5 notes
 
summary(fit0)

########## Comparing nested model with anova() ##################

### Page 20 in Chapter 5 notes

fit.big   = lm(srich ~ logdischarge + logarea + propdriest + logdis2, data = rich)
fit.small = lm(srich ~ propdriest, data = rich)

anova(fit.small, fit.big)

fit.small.1 = lm(srich ~ logarea + logdischarge + propdriest, data = rich)
anova(fit.small.1, fit.big)

#this one is best compared to the big model because it has the largest p value = we can reject fit.big
fit.small.2 = lm(srich ~ logdischarge + propdriest, data = rich)
anova(fit.small.2, fit.big)

fit.small.3 = lm(srich ~ logarea + propdriest, data = rich)
anova(fit.small.3, fit.big)

### Why this is the case???
### Becase logarea and logdischarge are 
### linearly correlated, to confirm  

plot(logarea ~ logdischarge, data = rich)

install.packages("ellipse")

library(ellipse)
plotcorr(cor(rich))

### But the question is, why remove 
### logarea but not logdischarge

########## Multicollinearity ##################

### page 24 in Chapter 5 notes 

vif(fit.big)
vif(fit.small.1) #We drop the largest value

### Using AIC/BIC to compare non-nested model #####

### Page 28,28 in Chapter 5

AIC(fit.small.2, fit.small.3)
BIC(fit.small.2, fit.small.3)

### Best subset algorithm ###

### Fitting all possible subset of models 
### identifying the ones that look best
### according to some meaningful criterion              # ADJ r^2 (LARGER), AIC(SMALLER), BIC (SMALLER): and ((AICc, BICc) --- corrected for small sample size)
### and ideally one that includes enough variables 

### Page 30, Chapter 5

install.packages("leaps")

library(leaps)

bestsubs = regsubsets(srich ~ discharge + area + propdriest +
                              logdischarge + logarea +
                              logdis2 + logarea2,
                         data = rich, 
                        nbest = 1, # 1 best model for each number of predictors
                       method = "exhaustive")

summary(bestsubs)

### Plot Adjusted R^2

#Want to pick the one with the highest adj r^2 value
res.legend = subsets(bestsubs, statistic = "adjr2", legend = FALSE, main = "Adjusted R^2")
  # (p-lgd-lgr-lgr2) === 4 predictors

### Plot AIC ( Mallow's Cp)


#want to find the lowest AIC value (CP)
res.legend = subsets(bestsubs, statistic = "cp", legend = FALSE, main = "AIC (CP)")
abline(a = 1, b = 1, lty = 2)
  # p-lgd

## BIC

res.legend = subsets(bestsubs, statistic = "bic", legend = FALSE, main = "BIC")
#same as AIC


### The function below takes `regsubsets()` output and formats it into a table.

f.bestsubset = function(form, dat, nbest = 5){
  
  library(leaps)
  bs = regsubsets(form, data = dat, nvmax = 30, nbest = nbest, method = "exhaustive");
  bs2 = cbind(summary(bs)$which, (rowSums(summary(bs)$which)-1)
               , summary(bs)$rss, summary(bs)$rsq
               , summary(bs)$adjr2, summary(bs)$cp, summary(bs)$bic);
  cn = colnames(bs2);
    cn[(dim(bs2)[2]-5):dim(bs2)[2]] = c("SIZE", "rss", "r2", "adjr2", "cp", "bic");
    colnames(bs2) = cn;
  ind = sort.int(summary(bs)$bic, index.return=TRUE); 
  bs2 = bs2[ind$ix,];
  return(bs2);
}

## perform on our model

i.best = f.bestsubset(formula(srich ~ discharge + area + propdriest +
                              logdischarge + logarea +
                              logdis2 + logarea2)
          , rich)

  op = options(); # saving old options
  options(width = 90) # setting command window output text width wider

i.best

options(op); # reset (all) initial options

#########################################################
##################### Cross Validation ##################
#########################################################

### The idea of CV

### create training and testing data

set.seed(2018411)

## Use 80% of the data to train the model

trainid = sample(1:nrow(rich), floor(0.8*nrow(rich)))
train = rich[trainid,]
test = rich[-trainid,]

### build model on training data

mod1 = lm(srich ~ logdischarge + propdriest, data = train)
mod2 = lm(srich ~ logdischarge + propdriest + logdis2, data = train)
mod3 = lm(srich ~ logdischarge + propdriest + logarea + logarea2, data = train)

AIC(mod1, mod2, mod3)


### get the predictions on test data

pred1 = predict(mod1, test)
pred2 = predict(mod2, test)
pred3 = predict(mod3, test)

### calculate MSE

mse1 = mean((test$srich - pred1)^2)
mse2 = mean((test$srich - pred2)^2)
mse3 = mean((test$srich - pred3)^2)

mse1
mse2
mse3

### alternatively, you can compute all the 
### error metrics in one go
### using the regr.eval() function in DMwR package.

### install.packages("DMwR")

install.packages("DMwR")

library(DMwR)

regr.eval(test$srich, pred1)
regr.eval(test$srich, pred2)
regr.eval(test$srich, pred3)

?regr.eval


install.packages("boot")

library(boot)

    #generalized linerar model

mod1 = glm(srich ~ logdischarge + propdriest, data = rich)
mod2 = glm(srich ~ logdischarge + propdriest + logdis2, data = rich)
mod3 = glm(srich ~ logdischarge + propdriest + logarea + logarea2, data = rich)

### LOOCV method
# Leave one out cross validation

loocv1 = cv.glm(rich, mod1)
summary(loocv1)
loocv1$delta[1]

#but also
mse1


loocv2 = cv.glm(rich, mod2)
loocv2$delta[1]

loocv3 = cv.glm(rich, mod3)
loocv3$delta[1]


### delta --- A vector of length two. The first component is the 
### raw cross-validation estimate of prediction error. 
### The second component is the adjusted cross-validation estimate. 
### The adjustment is designed to compensate for the bias introduced 
### by not using leave-one-out cross-validation.

k3cv1 = cv.glm(rich, mod1, K = 3)
k3cv1$delta[1]
k5cv1 = cv.glm(rich, mod1, K = 5)
k5cv1$delta[1]

k3cv2 = cv.glm(rich, mod2, K = 3)
k3cv2$delta[1]
k5cv2 = cv.glm(rich, mod2, K = 5)
k5cv2$delta[1]

k3cv3 = cv.glm(rich, mod3, K = 3)
k3cv3$delta[1]
k5cv3 = cv.glm(rich, mod3, K = 5)
k5cv3$delta[1]


### Rep the 5-fold CV for each model 3 times 
### Plot the delts[1] values side-by-side

d1 = numeric(100)
d2 = numeric(100)
d3 = numeric(100)

for (i in 1:100){
d1[i] = cv.glm(rich, mod1, K = 5)$delta[1]
d2[i] = cv.glm(rich, mod2, K = 5)$delta[1]
d3[i] = cv.glm(rich, mod3, K = 5)$delta[1]
}

delta = c(d1, d2, d3)
mod = rep(c("mod1", "mod2", "mod3"), each = 100)
dat = tibble(delta, mod)

dat %>% 
 ggplot(aes(x = mod, y = delta))+
  geom_boxplot(fill = "purple")


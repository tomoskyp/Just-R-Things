library(tidyverse)

frogs = read_csv(file.choose())

frogs %>% View()

### Make a scatter plot matrix

library(car)
scatterplotMatrix(~ present + altitude + distance + NumPool + NumSites + 
                    avrain + meanmin + meanmax, 
                    regLine  = FALSE, 
                    smooth   = TRUE,
                    data     = frogs)

### Let's begin by fitting a model in which 
### all variables are included (excluding 
### `northing` and `easting`) in their raw form.

fullmodel = glm(present ~ altitude + distance + NumPool + 
                          NumSites + avrain + meanmin + meanmax, 
                data    = frogs, 
                family  = binomial)

summary(fullmodel)

### Having fit the full model it is not always 
### clear what to do next. 
### One approach at this point would be to build 
### our model using `significance testing`. 
### In this spirit let's try dropping all variables 
### that don't meet 0.05 significance level. 
### This removes `altitude`, `NumSites`, `avrain`, and `meanmax`. 
### To assess whether doing this was a "good move", 
### let's carry out a `likelihood ratio test` 
### (actually an analysis of deviance test) 
### comparing the full model to the reduced model using the anova 
### function with the `test = 'LRT'` option.

### fit reduced model
 
model1 = glm(present ~ distance + NumPool + meanmin, 
             data   = frogs, 
             family = binomial)

### compare full and reduced models 

anova(model1, fullmodel, test = 'LRT')

### Whoops. The test says we went too far. 
### The full model is significantly “better” than the
### reduced model. let’s try a different reduced model, 
### this time dropping only the three 
### least significant variables (rather than all four). 
### The result is that meanmax is added back to the model.

model2 = glm(present ~ distance + NumPool + meanmin + meanmax, 
             data   = frogs, 
             family = binomial)

anova(model2, fullmodel, test = 'LRT')

### Now we’ve clearly got it right.
### The LR test says that the three variables 
### included in the full model, 
### but not in model2 are unnecessary 
### (they don’t significantly improve the fit 
### in the sense of increasing the likelihood, 
### or equivalently, decreasing the deviance). 
### So what happened? 
### Let’s look at the coefficient summary table.

summary(fullmodel)
summary(model2)

### Observe that meanmax which was not significant 
### at the 0.05 level in the full model 
### is now highly significant. How can this be? 
### When this kind of thing happens it usually 
### means that there are redundant variables 
### in the data set — variables that are appear to be 
### conceptually different but which in fact are essentially 
### measuring the same thing. Statistically this often, 
### but not always, manifests itself in the two 
### variables being highly correlated. 

### So with which variable is meanmax highly correlated? 

cor(frogs$meanmax, cbind(frogs$altitude, frogs$avrain, frogs$NumSites))

### It’s worth noting that model2 contains 
### both meanmin and meanmax as significant contributors, 
### yet it’s rather obvious that mean spring time minimum 
### temperatures and mean spring time maximum temperatures
### must be related. In fact they are.

cor(frogs$meanmin, frogs$meanmax)

### We’ll examine later whether keeping such 
### highly correlated variables 
### in the same model is an asset or a liability.

#############################################
## Assessing the structural form of the model
#############################################

### A basic question with logistic regression models, 
### as with all regression models, is have we 
### entered the predictors into our model using 
### the correct structural form? 
### When the form is correct, the regressor 
### should be linearly related to the logit.

### One ways to assess this is to fit the 
### logistic regression model with a restricted 
### cubic spline function of the predictor. 

### The function `rcspline.plot` in "rms" 
### package both fits the logistic regression 
### model and plots the estimated spline model. 

#install.packages("rms")

library(rms)

?rcspline.plot

### The `nk`, is the number of `knots`. 
### `Knots` are anchor points for the cubic spline. 
### Two will automatically be located at the 
### minimum and maximum x-values. 
### The remaining locations are estimated. 
### Values of 3, 4, or 5 are typical choices 
### for `nk` (the default is 5). 
### If a model fails to converge with a specified 
### value you will need to reduce it.

### The argument, `m`, is not required. 
### It specifies the number of observations to use in 
### calculating grouped empirical logits. 
### This is useful for assessing model fit.

### The proper structural form for `meanmin` ###

rcspline.plot(y = frogs$present, x = frogs$meanmin, nk = 5, m = 20)

### To read the graph, the arrows at the bottom 
### of the graph denote the estimated knot locations. 

### $n$ is the number of observations, 212 here.
### $d$ is the number of successes ($Y = 1$), 79 here.
### The "model LR" is a likelihood ratio test of the spline 
### model against a model with only an intercept. 
### A p-value is not computed but a chi-square 
### statistic of 43.04 with 4 degrees 
### of freedom is highly significant. 
### Thus we can conclude that the inclusion of the predictor 
### in a cubic spline function yields a model significantly 
### better than a model with no predictor at all.

### The AIC that is reported is not calculated the same way 
### that we've been calculating AIC. 
### The reported AIC is the likelihood ratio statistic 
### (rather than two times the loglikelihood) minus 
### two times the number of parameters.
### The "association Wald" test is a multivariable version of the 
### Wald test we've been using. It tests the same hypothesis 
### that is tested by the "model LR" described above.
### The "linearity Wald" test is of special interest here. 
### It tests whether the cubic spline model can be reduced to 
### a model in which the logit is linear in the predictor. 
### The low reported p-value ($p = 0.0012$) 
### means we should reject the hypothesis of linearity.

### The structural form for meanmax ###

rcspline.plot(y = frogs$present, x = frogs$meanmax, nk = 5, m = 20)

### Let’s try adding quadratic terms for meanmin and meanmax 
### to model2 that we obtained previously.

model2a = glm(present ~ distance + NumPool + meanmin + meanmax + 
                        I(meanmin^2) + I(meanmax^2), 
                        data   = frogs, 
                        family = binomial)
summary(model2a)

### Model comparison with "anova"

anova(model2, model2a, test = 'Chisq')

### It’s possible that the presence of the other variable 
### in the model makes the quadratic term unnecessary.

###  The structural form for distance ###

rcspline.plot(y = frogs$present, x = frogs$distance, nk = 5, m = 20)

rcspline.plot(y = frogs$present, x = frogs$distance, nk = 4, m = 20)

### log transform 

rcspline.plot(y = frogs$present, x = log(frogs$distance), nk = 4, m = 20)

### Now let's build a `model2b` with 
### `log-transformed distance` 
### and use AIC to compare it with model2 
### which with `untransformed distance`.

model2b = glm(present ~ log(distance) + NumPool + meanmin + meanmax + 
                        I(meanmin^2) + I(meanmax^2), 
                        data   = frogs, 
                        family = binomial)
summary(model2b)

AIC(model2, model2a, model2b)

###############################################################
## Automated model selection
###############################################################

### Package "glmulti" provides a wrapper for 
### glm and other functions automatically generating 
### all possible models (under constraints set by the user) 
### with the specified response and explanatory variables, 
### and finding the best models in terms of some 
### Information Criterion (AIC, AICc or BIC). 
### Can handle very large numbers of candidate models.

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")

library(rJava)

#install.packages("rJava")
#install.packages("devtools")

#devtools::install_github("cran/glmulti")

install.packages("glmulti")
library(glmulti)

### package 'rJava' need to be loaded

frogs = frogs %>%
         mutate(ld = log(distance))

glmulti.logistic.out =
    glmulti(present ~ distance + NumPool + meanmin + meanmax + ld,
            data        = frogs,
            level       = 2,         # Interaction considered (2nd order model)
            method      = "h",       # Exhaustive approach
            crit        = "aic",     # AIC as criteria
            confsetsize = 5,         # Keep 5 best models
            plotty      = F, 
            report      = F,         # No plot or interim reports
            fitfunction = "glm",     # glm function
            family      = binomial)  # binomial family for logistic regression

### Show 5 best models (Use @ instead of $ for an S4 object)

glmulti.logistic.out@formulas

### Based on the results our final model could be

model3 = glm(present ~ distance + NumPool + meanmin + meanmax + 
                       distance:meanmin + distance:NumPool +
                       meanmin:meanmax, 
             family = binomial, 
             data   = frogs) 
summary(model3)

model4 = glm(present ~ ld + NumPool + meanmin + meanmax + 
                       ld:meanmin + ld:NumPool + meanmin:meanmax, 
             family = binomial, 
             data   = frogs) 
summary(model4)

### Compare Model 2b, 3 and 4

AIC(model2b, model3, model4)

#############################################################
######                 Cross-validation                ######
#############################################################

### The cv.binary function in the DAAG package carries out 
### 10-fold cross-validation on a specified model. 

#install.packages("DAAG")

library(DAAG)

cv.binary(model2)
cv.binary(model2b)
cv.binary(model3)
cv.binary(model4)

### "internal estimate of accuracy" is the 
### estimate obtained when the same data are 
### used to build and test the model.

### What does the comparison imply???

### Your turn: use model 3 to find the best
### cutoff for classification

prob.pred = predict(model3, newdata = frogs, type = "response")

library(ROCR)

pred.mod3 = prediction(prob.pred, frogs$present)
test.mod3 = performance(pred.mod3, 'tpr', 'tnr')

plot(test.mod3@alpha.values[[1]], 
     test.mod3@x.values[[1]], 
     type = 'n', 
     xlab = 'cutpoint',
     ylab = 'Sensitivity/Specificity')
lines(test.mod3@alpha.values[[1]], 
      test.mod3@y.values[[1]], 
      col = "red", 
      lwd = 6)
lines(test.mod3@alpha.values[[1]],
      test.mod3@x.values[[1]], 
      col = "blue", 
      lty = 3, 
      lwd = 6)
legend(0.5, 0.8, c('sensitivity', 'specificity'), 
       lty = c(2,2), 
       col = c("red", "blue"), 
       cex = c(0.9, 0.9), bty = 'n')

### Let's use 0.42

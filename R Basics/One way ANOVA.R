########## 2-sample Test As Regression ##################

### Make the data frame

beer = c(27, 19, 20, 20, 23, 17, 21, 24, 31, 26, 28, 20, 
         27, 19, 25, 31, 24, 28, 24, 29, 21, 21, 18, 27, 20)
water = c(21, 19, 13, 22, 15, 22, 15, 22, 20, 12, 24, 24, 21, 19, 18, 16, 23, 20)

NoM = c(beer, water)
group = c(rep("beer", length(beer)), rep("water", length(water)))

mosquito = data.frame(NoM, group)

mosquito

### Perform a 2-sample t test

t.test(NoM ~ group, var.equal = TRUE, alternative = "greater", data = mosquito)

### Solve the problem with a SLR

fit = lm(NoM ~ group, data = mosquito)
summary(fit)

### set "water" as baseline level

mosquito$group = relevel(mosquito$group, "water")

fit = lm(NoM ~ group, data = mosquito)
summary(fit)

########## One-way ANOVA ##################

### Import data, here manually ####

### Many functions in R expect data to be 
### in a long format rather than
### a wide format. Let's use the meat data 
### as an example of how to read 
### a table as text, convert the wide format 
### to long, and then back to wide format.

library(tidyverse)

meat = read.table(text = "
Row Commercial Vacuum Mixed CO2
1 7.66 5.26 7.41 3.51
2 6.98 5.44 7.33 2.91
3 7.8 5.8 7.04 3.66
", header = TRUE)

meat

### From wide to long: use spread().
### ?spread

?gather
    #this is the long format of the data
meat.long = meat %>% gather(treatment, y, -Row)
meat %>% gather(treatment, y, -1)

### From long to wide: Use spread().

meat.long %>% spread(2:3)

?spread                            ##### THIS IS BROKEN

smeat.wide = spread(meat.long, 1)


meat %>% gather(treatment, y, -1)


### We will use meat.long in what follows
### First step (Always): Exploratory data Analysis

## If very few observations: Plot all data points vs. treatment
## With more observations: use side-by-side boxplots 
## Such plots typically give you the same (or even more) information 
## as a formal analysis

ggplot(meat.long)+
  aes(x = treatment, y = y, colour = treatment)+
  geom_point(size = 6)+
geom_hline(yintercept = mean(meat.long$y),
           colour = "black", linetype = "dashed", size = 1)+
stat_summary(fun.y = mean, geom = "point", shape = "-", size = 16, 
             colour = "blue")+
labs(x = "Packaging Type", y = "Log Number of Bacteria psc")+
theme(axis.text.y = element_text(size = 12, colour = 'black'))+
theme(axis.text.x = element_text(size = 15, colour = 'black'))+
theme(axis.title = element_text(size = 16))

### Numerical Summaries 

nums = meat.long %>% 
        group_by(treatment) %>% 
        summarise(sample.size = n(),
                  sample.mean = mean(y),
                  sample.sd = sd(y),
                  standard.error = sample.sd/sqrt(sample.size)) # This is a balanced design


summary(nums)
################## Cell Means Model ##################

c.mod = lm(y ~ treatment-1, data = meat.long)
summary(c.mod)

# Estimate is the meal of the treatments But the standard error is also given.
# Estimate is mu-hat
# This is important because the data needs to be treated as dependant
# related to the residual standard error
0.3404/sqrt(3)
# 

## write down the design matrix for "c.mod", then 
## check your answers with R

model.matrix(c.mod)
meat.long





################## Treatment Effects Model ##################

# By default, R uses treatment contrasts, as you can see when you check 
# the relevant option like this:

options('contrasts')

# Here you see that R uses different contrasts for unordered and ordered # factors. 
# These contrasts are actually contrast functions. They return a matrix 
# with the contrast value for each level of the factor. The default 
# contrasts for a factor with three levels look like this:

X = factor(c('A', 'B', 'C', "D", "E", "F"))
X
contr.treatment(X)

t(contr.treatment(X)) # A is reference group intercept
#t is transpose 

# Level A is represented by two zeros and called the "reference level".
# If we want to change the reference level, we can do this by using the 
# function 'relevel' 

X = relevel(X, ref = "C") # ref is setting the reference group
X

X = relevel(X, ref = "A")
contr.treatment(levels(X))
X = relevel(X, ref = "B")
contr.treatment(levels(X))
X = relevel(X, ref = "C")
contr.treatment(levels(X))
X = relevel(X, ref = "D")
contr.treatment(levels(X))
X = relevel(X, ref = "E")
contr.treatment(levels(X))
X = relevel(X, ref = "F")
contr.treatment(levels(X))










t(contr.treatment(levels(X)))








## Go back to the meat experiment

e.mod = lm(y ~ treatment, data = meat.long)
summary(e.mod)
#mu1 = co2= is the intercept now because it is the reference group
#the treatment estimates are aplha-hat-commerical, mixed, vacuum. 


# extract coefficients 
coef(e.mod)
dummy.coef(e.mod)

str(meat.long)

meat.long $ treatment = as.factor(meat.long$treatment)
str(meat.long)
coef(e.mod)

anova(e.mod)
summary(aov(y ~ treatment, data = meat.long))

## write down the design matrix for "e.mod", then 
## check your answers with R

model.matrix(e.mod)

### You can change these contrasts using the 
### same options() function, like this:

options(contrasts = c('contr.sum','contr.poly'))

e.mod2 = lm(y ~ treatment, data = meat.long)
summary(e.mod2)
# Estimate $ intercept is mu-hat or y-bar

mean(meat.long$y)

dummy.coef(e.mod2)
#the mean in given in the intercept.
# add or subtract the treatment values from this to get mu-bar
# treatment values are aplhas as given. 


## write down the design matrix for "e.mod2", then 
## check your answers with R

model.matrix(e.mod2)

# options(contrasts = c('contr.treatment','contr.poly'))

################## ANOVA as Model Comparison ##################

g.mod = lm(y ~ 1, data = meat.long)
summary(g.mod)
anova(g.mod, e.mod)   # compare the two modes
                      # F: test statistic

################## Checking Model Assumptions ##################

library(ggfortify)
autoplot(e.mod)

################## Inferences for a Treatment Mean ##################

# Let's look at the cell mean model

summary(c.mod)
0.3404/sqrt(3)
7.48/0.1965

# There are two ways to get the CI for group means \mu_i

## One, we can use the 'predict()' function

predict(c.mod, newdata = data.frame(treatment = c("Commercial", "Vacuum", "Mixed", "CO2")), interval = "confidence")

## Or, we can use 'allEffects()' from the 'effects' package.
install.packages("effects")
library(effects)
allEffects(c.mod)
summary(allEffects(c.mod))

### or 

summary(allEffects(e.mod))

# You can also change the confidence level, e.g.

summary(allEffects(c.mod, confidence.level = 0.9))

# Another advantage of the 'allEffects()' function is you can plot 
# it directly, say

plot(allEffects(c.mod))

plot(allEffects(c.mod), main = "Comparison of Treatment Mean with CI")

##########################################################################################
## Meat Storage Data ####
## A different way to input the data #####################################################

meat = data.frame(

steak.id = c(1, 6, 7, 12, 5, 3, 10, 9, 2, 8, 4, 11),
treatment = rep(c("Commercial", "Vacuum", "Mixed", "CO2"), each = 3),
y = c(7.66, 6.98, 7.8, 5.26, 5.44, 5.8, 7.41, 7.33, 7.04, 3.51, 2.91, 3.66))

meat

levels(meat$treatment) ## check order of levels (important!)

#treatment effect model
fit = lm(y ~ treatment, data = meat)


library(tidyverse)
library(broom)

fit %>% summary()

## get ANOVA table

anova(fit)
## p value is very small: reject Ho, there is some effect from the treatment

# three functions from the broom library:

fit %>% tidy
# statistic gives t-stat

fit %>% glance()
# sigma = sigma-hat, statistic = F

fit %>% augment()
#  the fitted value. .fitted = mu-hat


###############
## Contrasts ##
###############

install.packages("effects")

library(effects)

## Or, we can use 'allEffects()' from the 'effects' package.

library(effects)

allEffects(fit)
summary(allEffects(fit))

### or 

summary(allEffects(fit))

# You can also change the confidence level, e.g.

summary(allEffects(fit, confidence.level = 0.9))

# Another advantage of the 'allEffects()' function is you can plot 
# it directly, say

plot(allEffects(fit))

#also called an omnibus test / overall test
plot(allEffects(fit), main = "Comparison of Treatment Mean with CI")




levels(meat$treatment)
                                             
str(meat)




## Define individual contrast ####

contrasts(meat$treatment) = c(1, -1, 1, -1) ## new vs. old

fit = aov(y ~ treatment, data = meat)

## Use built-in R functionality. Can be dangerous if
## non-orthogonal contrasts are involved.
## Multiple contrasts can be combined with *columns*.

str(fit)

str(meat)

fit$contrasts

fit %>% summary

summary(fit, split = list(treatment = list(new.vs.old = 1)))

## Use R-package multcomp ####

install.packages("multcomp")

library(multcomp)

## new vs. old

#General Linear Hypothesis Testing = GLHT
fit.sc = glht(fit, linfct = mcp(treatment = c(1, -1, 1, -1))) 
summary(fit.sc)


plot(fit.sc)
## Test many contrasts at the same time.
## Each contrast defines a *row* of the matrix M

M = rbind(c(1, -1, 1, -1), ## new vs. old
          c(1, 0, -1, 0))  ## CO2 vs. mixed
fit.mc = glht(fit, linfct = mcp(treatment = M))

summary(fit.mc, test = adjusted('none')) ## individual tests

## Construct confidence intervals
confint(fit.mc, calpha = univariate_calpha()) ## *individual* CIs

## To change confidence level, you can do 

confint(fit.mc, calpha = univariate_calpha(), level = 0.9)

## TukeyHSD ####

TukeyHSD(fit) ## R function uses aov object
plot(TukeyHSD(fit))

## Using the package multcomp

# library(multcomp)

## Run this line for any MCP

mcp = glht(fit, linfct = mcp(treatment = "Tukey")) 

# To output Tukey's Test results

summary(mcp)
confint(mcp)

par(mar = c(5, 10, 4, 2) + 0.1)
plot(confint(mcp))

###############################
## Comparison with a Control ##
###############################

## Dunnett ####

fit.dunnett = glht(fit, linfct = mcp(treatment = "Dunnett"))
summary(fit.dunnett) ## CO2 is reference level
plot(fit.dunnett) ## CO2 is reference level


dunnettL = glht(fit,linfct = mcp(treatment = "Dunnett"), alternative = "less")
summary(dunnettL)
plot(dunnettL)

dunnettU = glht(fit, linfct = mcp(treatment = "Dunnett"), alternative = "greater")
summary(dunnettU)
plot(dunnettU)

#### Do two-sided Dunnett MCP with "Commercial" as the control group

meat$treatment = relevel(meat$treatment, ref = "Commercial")
fit2 = aov(y ~ treatment, data = meat)

fit2.dunnett = glht(fit2, linfct = mcp(treatment = "Dunnett"))
summary(fit2.dunnett) 
plot(fit2.dunnett) 

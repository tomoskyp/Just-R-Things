########################################################
#### 2 Way Example 1: Beetles  #########################
########################################################

library(tidyverse)

### import the data 
### Note the data is in wide format

beetles = read.table(file.choose(), header = TRUE)
beetles
str(beetles)

### make dose a factor variable 

beetles$dose = as.factor(beetles$dose)

### Convert wide data to long data 

beetles.long = beetles %>% 
                gather(number, hours10, -dose, -insecticide)

beetles.long
str(beetles.long)

######################################################
############## Data Visualization ####################
######################################################

##### Interaction plots, base graphics 

interaction.plot(beetles.long$dose, 
                 beetles.long$insecticide, 
                 beetles.long$hours10, 
                 lwd  = 3,
                 main = "Beetles interaction plot, insecticide by dose")



interaction.plot(beetles$dose, 
                 beetles$insecticide, 
                 beetles$hours10, 
                 lwd  = 3,
                 main = "Beetles interaction plot, insecticide by dose")


interaction.plot(beetles.long$insecticide, 
                 beetles.long$dose, 
                 beetles.long$hours10, 
                 lwd  = 3, 
                 main = "Beetles interaction plot, dose by insecticide")

####### Use ggplot2 #######

### Calculate the cell means for each factor 
### and means for each (dose, insecticide) combination

beetles.mean.d = beetles.long %>% 
                   group_by(dose) %>%
                   summarize(m = mean(hours10))
beetles.mean.d # Ybar 1.. , Ybar 2.. //////

beetles.mean.i  = beetles.long %>% 
                   group_by(insecticide) %>%
                   summarize(m = mean(hours10))
beetles.mean.i # Ybar .1. , Ybar .2. //////

beetles.mean.di = beetles.long %>% 
                   group_by(dose, insecticide) %>%
                   summarize(m = mean(hours10))
beetles.mean.di # interactions -- summary statistics
                # Ybar 11. , Ybar 12. , ////////// 

### I recommend you start with a side-by-side boxplot
### This plot can check both normality and constant variance

### boxplots

bp = beetles.long %>% 
       ggplot(aes(x      = dose, 
                  y      = hours10, 
                  colour = insecticide))+
       geom_boxplot() 
bp

### Interaction plots, Insecticide by dose

p = beetles.long %>% 
      ggplot(aes(x      = dose, 
                 y      = hours10, 
                 colour = insecticide, 
                 shape  = insecticide))

p = p + geom_hline(aes(yintercept = 0), 
                   colour   = "black",
                   linetype = "solid", 
                   size     = 1)

p = p + geom_point(alpha    = 0.5, 
                   position = position_dodge(width = 0.75))

p = p + geom_point(data = beetles.mean.di, 
                  aes(y = m), 
                  size = 4)

p = p + geom_line(data  = beetles.mean.di, 
                  aes(y = m, group = insecticide), 
                  size  = 1.5)

p = p + labs(title = "Beetles Interaction Plot, Insecticide by Dose") + 
        theme(plot.title = element_text(hjust = 0.5))
p

### Interaction plots, does by insecticide



######################################################
#################    Modeling     ####################
######################################################

### Fit the model: In the lm() function below we specify 
### a first-order model with interactions, 
### including the main effects and two-way interactions.

fit = lm(hours10 ~ dose + insecticide + dose:insecticide,
         data = beetles.long)

## lm(hours10 ~ dose*insecticide, data = beetles.long) # equivalent

summary(fit)

dummy.coef(fit) # tog et the meaning of the coeficients 

### check the model matrix

model.matrix(fit)

### Tell me what is the intercept? (low A)
### -0.0925 (midium A - low A)

### Test the interaction first ########################

library(car)
Anova(fit, type = 3) # when you want to look at interactions

#only interaction if dose:inseecticide pvalue is very low ---- failed to reject!

### Since the interaction is not significant, 
### I'll drop the interaction term and fit
### the additive model with main effects only.

fit2 = update(fit, ~ . -dose:insecticide )

Anova(fit2, type = 3)
summary(fit2)

### Testing multiple factors is of interest here.
### Note that the code below corrects the p-values
### for all the tests done for both factors together,
### that is, the Bonferroni-corrected significance 
### level is (alpha / (d + i))
### where d = number of dose comparisons
### and   i = number of insecticide comparisons.

### correcting over dose and insecticide

install.packages("multcomp")

library(multcomp)

mcp = glht(aov(fit2), 
           linfct = mcp(dose        = "Tukey",
                        insecticide = "Tukey"))
summary(mcp, test = adjusted("bonferroni"))

### plot the summary
  
op = par(no.readonly = TRUE) # the whole list of settable par's.
# make wider left margin to fit contrast labels
par(mar = c(5, 10, 4, 2) + 0.1)  # order is c(bottom, left, top, right)
# plot bonferroni-corrected difference intervals
plot(summary(mcp, test = adjusted("bonferroni"))
    , sub = "Bonferroni-adjusted Treatment contrasts")
par(op) # reset plotting options

#### Checking assumptions

library(ggfortify)
autoplot(fit2)

#this does not satisfy the model, we need to transform the data:

### Another useful tool for checking constant variances 
### is to plot the sample deviations for each group against 
### the group means.

###  mean vs sd plot

### means and standard deviations for 
### each dose/interaction cell

beetles.meansd.di = beetles.long %>% 
                      group_by(dose, insecticide) %>% 
                      summarize(m = mean(hours10), 
                                s = sd(hours10))

beetles.meansd.di

beetles.meansd.di %>% 
  ggplot(aes(x      = m, 
             y      = s, 
             shape  = dose, 
             colour = insecticide))+
   geom_point(size  = 5)+
   labs(title = "Beetles standard deviation vs mean")

### Diagnostic plots show the following features. 
### The normal quantile plot shows an "S" shape rather 
### than a straight line, suggesting the residuals are not
### normal, but have higher kurtosis (more peaky) than a 
### normal distribution. 
### The residuals vs the fitted (predicted) values 
### show that the higher the predicted value the 
### more variability (horn shaped).

### The plot of the s_ij against \bar{y}_{ij} show
### the tendency for the spread to increase with the mean.

### Survival times are usually right skewed, with the spread
### or variability in the distribution increasing 
### as the mean or median increases.

######################################################
###### A Remedy for Non-Constant Variance ############
######################################################

### A plot of cell standard deviations against the cell means 
### is sometimes used as a diagnostic tool for suggesting 
### transformations of the data. Here are some suggestions 
### for transforming non-negative measurements to make 
### the variability independent of the mean 
### (i.e., stabilize the variance). 
### The transformations also tend to reduce skewness, if present 
### (and may induce skewness if absent!).

## 1. If s_{ij} increases linearly with \bar{y}_{ij},
## use a log transformation of the response.

## 2. If s_{ij} increases as a quadratic function of \bar{y}_{ij}, 
## use a reciprocal (inverse) transformation of the response.

## 3. If s_{ij} increases as a square root function of \bar{y}_{ij}, 
## use a square root transformation of the response.

## 4. If s_{ij} is roughly independent of \bar{y}_{ij}, 
## do not transform the response. 
## This idea does not require the response to be non-negative!

### A logarithmic transformation or a reciprocal (inverse) 
### transformation of the survival times might help to 
### stabilize the variance.

### As a first pass, let's consider the reciprocal transformation 
### because the inverse survival time has a natural interpretation 
### as the 'dying rate'.

#### Example: Beetles, non-constant variance #######################

### create the rate variable (1/hours10)

beetles.long = beetles.long %>% 
                mutate(rate = 1/hours10)

### boxplots

beetles.long %>% 
 ggplot(aes(x      = dose, 
            y      = rate, 
            colour = insecticide))+
 geom_boxplot()

### mean vs sd plot

## obtain means and standard deviations for 
## each dose/interaction cell

rate.means = beetles.long %>% 
                      group_by(dose, insecticide) %>% 
                      summarize(m = mean(rate), 
                                s = sd(rate))
rate.means

## Making s_{ij} vs. \bar{y}_{ij} plot 

rate.means %>% 
  ggplot(aes(x      = m, 
             y      = s, 
             shape  = dose, 
             colour = insecticide))+
  geom_point(size = 6)+
  labs(title = "Beetles dying rate standard deviation vs mean")

### Interaction plots, insecticide by dose

beetles.long %>% 
 ggplot(aes(x      = dose, 
            y      = rate, 
            colour = insecticide, 
            shape  = insecticide))+
 geom_hline(aes(yintercept = 0), 
            colour   = "black",
            linetype = "solid", 
            size     = 0.2, 
            alpha    = 0.3)+
 geom_point(alpha    = 0.5, 
            position = position_dodge(width = 0.75))+
 geom_point(data = rate.means, aes(y = m), size = 4)+
 geom_line(data  = rate.means, aes(y = m, group = insecticide), size = 1.5)+
 labs(title = "Beetles interaction plot, insecticide by dose")

### Fit full model and check for interaction effects 

fit.rate = lm(rate ~ dose + insecticide + dose:insecticide,
              data = beetles.long)

library(car)
Anova(fit.rate, type = 3)

### Fit the additive model

fit.rate.add = lm(rate ~ dose + insecticide,
                  data = beetles.long)
Anova(fit.rate.add, type = 3)

### residual analysis

autoplot(fit.rate.add)

### Testing multiple factors is of interest here.

#library(multcomp)

glht.rate = glht(aov(fit.rate.add), 
                 linfct      = mcp(dose = "Tukey", 
                 insecticide = "Tukey"))

summary(glht.rate, test = adjusted("bonferroni"))

par(mfrow=c(1,1))
# plot the summary
  op = par(no.readonly = TRUE) # the whole list of settable par's.
  # make wider left margin to fit contrast labels
  par(mar = c(5, 10, 4, 2) + 0.1)  # order is c(bottom, left, top, right)
# plot bonferroni-corrected difference intervals
plot(summary(glht.rate, test = adjusted("bonferroni"))
    , sub="Bonferroni-adjusted Treatment contrasts")
  par(op) # reset plotting options


########################################################
#### 2 Way Example 2: Output voltage for batteries  ####
########################################################

# library(tidyverse)

battery = read.table(file.choose(), header = TRUE)

battery
str(battery)


### Making factors 

battery$material = as.factor(battery$material)
battery$temp     = as.factor(battery$temp)
str(battery)

### Making long data

battery.long = battery %>%
  gather(number, maxvolt, -material, -temp)

battery.long
str(battery.long)

### Interaction plots, temp-by-material, add boxplots too

p = ggplot(battery.long, 
           aes(x      = material, 
               y      = maxvolt, 
               colour = temp, 
               shape  = temp))

p = p + geom_hline(aes(yintercept = 0), 
                   colour   = "black",
                   linetype = "solid", 
                   size     = 0.2, 
                   alpha    = 0.3)

p = p + geom_boxplot(alpha  = 0.25, 
                     outlier.size = 0.1)

p = p + geom_point(alpha    = 0.5, 
                   position = position_dodge(width = 0.75))

p = p + geom_point(data  = battery.mean.mt, 
                   aes(y = m), 
                   size  = 4)

p = p + geom_line(data = battery.mean.mt, 
                  aes(y = m, group = temp), 
                  size = 1.5)
p = p + labs(title = "Battery interaction plot, temp by material")
p

### Interaction plots, material-by-temp, add boxplots too

p = ggplot(battery.long, 
           aes(x      = temp, 
               y      = maxvolt, 
               colour = material, 
               shape  = material))

p = p + geom_hline(aes(yintercept = 0), 
                   colour   = "black",
                   linetype = "solid", 
                   size     = 0.2, 
                   alpha    = 0.3)
p = p + geom_boxplot(alpha  = 0.25, 
                     outlier.size = 0.1)

p = p + geom_point(alpha = 0.5, 
                   position = position_dodge(width = 0.75))
p = p + geom_point(data = battery.mean.mt, aes(y = m), size = 4)
p = p + geom_line(data = battery.mean.mt, aes(y = m, group = material), size = 1.5)
p = p + labs(title = "Battery interaction plot, material by temp")
p

### Modeling, start with interaction model

fit = 

library(car)
Anova(fit, type = 3)
summary(fit)

### Warning multiple comparison 

library(multcomp)

glht(aov(fit), linfct = mcp(temp = "Tukey"))

### The mcp may be inappropriate because of covariate interactions. 
### That is, interactions make the main effects less meaningful
### (or their interpretation unclear) since the change in response 
### when one factor is changed depends on what the second factor is.

### However, you can compare materials at each temperature, 
### and you can compare temperatures for each material.
### We do this use `lsmeans`

###########################################################
#####  Multiple comparisons when interaction exists   #####
###########################################################

### The `lsmeans` provides a way to compare cell means 
### (combinations of factors), something that is not possible 
### directly with glht(), which compares marginal means.

### When there are only main effects, the two methods agree.

### fit additive (main effects) model #######################

fit.main = lm(maxvolt ~ material + temp, data = battery.long)

### comparing means (must be balanced or have only one factor)
### correcting over temp

glht.battery.t = glht(aov(fit.main), linfct = mcp(temp = "Tukey"))
summary(glht.battery.t, test = adjusted("bonferroni"))

### comparing lsmeans (may be unbalanced)
library(lsmeans)
### compare levels of main effects
# temp

lsmeans(fit.main, list(pairwise ~ temp), adjust = "bonferroni")

### When there are model interactions, the comparisons of 
### the main effects are inappropriate, and give different 
### results depending on the method of comparison.

# fit interaction model (same as before)

fit = lm(maxvolt ~ material*temp, data = battery.long)

### comparing means (must be balanced or have only one factor)
# correcting over temp
glht.battery.t = glht(aov(fit), linfct = mcp(temp = "Tukey"))
summary(glht.battery.t, test = adjusted("bonferroni"))

### comparing lsmeans (may be unbalanced)
## compare levels of main effects
# temp
lsmeans(fit, list(pairwise ~ temp), adjust = "bonferroni")


### When there are model interactions and you want to 
### compare cell means, levels of one factor at each 
### level of another factor separately, then you must use lsmeans().

## compare levels of one factor at each level of another factor separately
# material at levels of temp

lsmeans(fit, list(pairwise ~ material | temp), adjust = "bonferroni")

# temp at levels of material
lsmeans(fit, list(pairwise ~ temp | material), adjust = "bonferroni")

### Finally, an important point demonstrated in the next section 
### is that the cell and marginal averages given by the means and 
### lsmeans methods agree here for the main effects model because 
### the design is balanced. For unbalanced designs with two or more 
### factors, lsmeans and means compute different averages. 
### I will argue that lsmeans are the appropriate averages 
### for unbalanced analyses. You should use the means statement 
### with caution --- it is OK for balanced or unbalanced one-factor 
### designs, and for the balanced two-factor designs 
### (including the RB) that we have discussed.



#########################################
# This code was created by Paul Tomosky #
# MATH 411                 LKXT@iup.edu #
# Homework 4                   11/26/18 #
#########################################

# Libraries #############################

library(tidyverse)
library(car)
library(ggfortify)
library(ellipse)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(GGally)
library(leaps)
library(boot)
library(effects)
library(lsmeans)
library(multcomp)


#########################################
#              PROBLEM 1                #
#########################################

FoHF = load(file.choose())
FoHF %>% View()


#1a
FoHF_fit_1 = lm(FoHF ~ ., data = FoHF)                         # [RV, CA, FIA, CTA] significant
summary(FoHF_fit_1)


 # ajusted r squared = 0.779, is close to one but not quite
 # RV: for each increase by 1 of FOFH, RV decreases by 0.388

autoplot(FoHF_fit_1, which = 1:2, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue')

 # the residual plot is randomly scattered the points are evenly scattered and are equally distributed
 # according the the Q-Q plot is light tailed and skewed slightly to the right
 # This will need to be fixed in order for this model to work

ggpairs(FoHF)
scatterplotMatrix(FoHF)
plotcorr(cor(FoHF))
vif(FoHF_fit_1)

bestsubs = regsubsets(FoHF ~ RV + CA + FIA + CTA,
                      data = FoHF, 
                      nbest = 1, # 1 best model for each number of predictors
                      method = "exhaustive")

summary(bestsubs)

# using best subs: remove all non selected variables

FoHF = FoHF %>%
  select(FoHF, RV, CA, FIA, CTA)

FOHF_fit_2 =  lm(FoHF ~ ., data = FoHF) 

# view normal QQ again

autoplot(FOHF_fit_2, which = 1:2, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue')

# There is still some Q-Q variance

ggpairs(FoHF)

# Fix FIA first since it has the wosrt distribution

#FoHF = FoHF %>%
#   mutate(FIA_square  = FIA ^2)

FoHF = FoHF %>%
  mutate(FIA_cuberoot  = FIA ^(1/3))  # This did a good job

    #FoHF = FoHF %>%
    #  mutate(FIA_log  = log(FIA)) 

    #FoHF$FIA_log = NULL 
    # FoHF$FIA_square = NULL 
  
# test the new big lm

FOHF_fit_3 =  lm(FoHF ~ RV + CA + FIA_cuberoot + CTA, data = FoHF) 
summary(FOHF_fit_3)  
  
# Futher adjustment to FIA_Cuberoot  

    #FoHF = FoHF %>%  
    #  mutate(FIA_cuberoot_log  = log(FIA_cuberoot))
  #FoHF$FIA_cuberoot_log = NULL                    # no luck here

# moving onto CA

  #FoHF = FoHF %>%
  #  mutate(CA_cuberoot  = CA ^(1/3))
  #FoHF$CA_cuberoot = NULL                    # no luck here


FoHF = FoHF %>%
  mutate(CA_sqrt  = CA ^(1/2))        # This did a good job


# test the new big lm

FOHF_fit_4 =  lm(FoHF ~ RV + CA + CA_sqrt + FIA + FIA_cuberoot + CTA, data = FoHF) 
summary(FOHF_fit_4)  


ggpairs(FoHF)

# I think I'm happy with this model.... get bestsubs

bestsubs = regsubsets(FoHF ~ RV + CA + CA_sqrt + FIA + FIA_cuberoot + CTA, 
                      data = FoHF, 
                      nbest = 1, # 1 best model for each number of predictors
                      method = "exhaustive")
  
res.legend = subsets(bestsubs, statistic = "bic", legend = FALSE, main = "BIC")

summary(res.legend)







#########################################
#              PROBLEM 2                #
#########################################

cwb = load(file.choose())
cwb %>% View()

cwb$gender = as.factor(cwb$gender)

# visualize
scatterplotMatrix(cwb, data = cwb)
ggpairs(cwb)
summary(cwb)

cwb_fit_1 = lm(duration ~ ., data = cwb)

summary(cwb_fit_1) # Everything is significant except age

autoplot(cwb_fit_1, which = 1:2, colour = 'red', size = 2,
         smooth.colour = 'black', smooth.linetype = 'dashed',
         ad.colour = 'blue', ad.size = 1,
         label.size = 2, label.n = 5, label.colour = 'blue')

bestsubs = regsubsets(x = duration ~ ., data = cwb, method = "exhaustive", nbest = 1)
summary(bestsubs)

res.legend = subsets(bestsubs, statistic = "bic", legend = FALSE, main = "BIC")

# for this we will select O-L-P-G as out model since it has the smallest BIC value

cwb_fit_2 = lm(duration ~ offer + lapse + price + gender, data = cwb)
summary(cwb_fit_2) # Every p-value is significant

# create generalized Linear Model

cwb_mod1 = glm(duration ~ ., data = cwb)
cwb_mod2 = glm(duration ~ offer + lapse + price + gender, data = cwb)

# Not sure what these values give but okay...

k5cv1 = cv.glm(cwb, cwb_mod1, K = 5)
k5cv1$delta[1]

k5cv2 = cv.glm(cwb, cwb_mod2, K = 5)
k5cv2$delta[1]

?cv.glm

# A vector of length two. 
# The first component is the raw cross-validation estimate of prediction error. 
# The second component is the adjusted cross-validation estimate. 
# The adjustment is designed to compensate for the bias introduced by not using leave-one-out cross-validation.

# This would imply that cwb_mod1 is better since is has a lower error

d1 = numeric(100)
d2 = numeric(100)

for (i in 1:100){
  d1[i] = cv.glm(cwb, cwb_mod1, K = 5)$delta[1]
  d2[i] = cv.glm(cwb, cwb_mod2, K = 5)$delta[1]}

delta = c(d1, d2)
mod = rep(c("mod1", "mod2"), each = 100)
dat = tibble(delta, mod)

dat %>% 
  ggplot(aes(x = mod, y = delta))+
  geom_boxplot(fill = "orange")

# alternate check to see MSE values of mod1 and mod2

cwb_pred1 = predict(cwb_mod1, cwb)
cwb_pred2 = predict(cwb_mod2, cwb)

cwb_mse1 = mean((cwb$duration - cwb_pred1)^2)
cwb_mse2 = mean((cwb$duration - cwb_pred2)^2)

cwb_mse1
cwb_mse2









#########################################
#              PROBLEM 3                #
#########################################

# create the data:
treatment = rep(c("C", "Lo1", "Lo2", "Lo3", "Hi1", "Hi2", "hi3"), each = 5)
yield     = c(507, 551, 525, 533, 516, 568, 525, 551, 577, 568, 586, 612, 620, 577, 568, 629, 577, 568, 595, 586, 586, 612, 568, 551, 595, 612, 595, 586, 629, 647, 595, 612, 647, 595, 612)
grape     = data.frame(treatment, yield)
      # str(grape)
      # summary(grape)

# plot the distributions:
ggplot(grape) +
  aes(x = treatment, y = yield, colour = treatment) +
  geom_point(size = 4) +
  geom_hline(yintercept = mean(grape$yield), colour = "black", linetype = "dashed", size = 1)+
  stat_summary(fun.y = mean, geom = "point", shape = "-", size = 12, colour = "blue")+
  labs(x = "Copper sulfate Treatments", y = "Grape Yields (lbs)")+
  theme(axis.text.y = element_text(size = 12, colour = 'black'))+
  theme(axis.text.x = element_text(size = 15, colour = 'black'))+
  theme(axis.title = element_text(size = 16))  

# numerical summaries:
nums = grape %>% 
  group_by(treatment) %>% 
  summarise(sample.size = n(),
            sample.mean = mean(yield),
            sample.sd = sd(yield),
            standard.error = sample.sd/sqrt(sample.size)) # This is a balanced design
summary(nums)

# Table of orthogonal coefficients:
contrastmatrix = cbind(c(6, -1, -1, -1, -1, -1, -1), # all of the treatments vs response
                       c(0, 1, 1, 1, -1, -1, -1),    # high vs lo 
                       c(0, 2, -1, -1, 2, -1, -1),   # Hi1 and Lo1 vs reamining treatments
                       c(0, 0, 1, -1, 0, 1, -1),     # medium vs high treatments
                       c(0, 2, -1, -1, -2, 1, 1),           # ???
                       c(0, 0, 1, -1, 0, -1, 1))            # ???

contrasts(grape$treatment) = contrastmatrix
      # grape$treatment

# create anova model
grape_contrast_model = aov(yield ~ treatment, grape)
      # summary(grape_contrast_model)

# ANOVA table:
summary(grape_contrast_model, split = list(treatment = list("CS Response" = 1,
                                                            "CS Level" = 2,
                                                            "1 vs Split" = 3,
                                                            "2 vs 3" = 4,
                                                            "1 vs split * CS Level" = 5,
                                                            "2 vs 3 * CS Level" = 6)))

# CI data and graph:
summary(allEffects(grape_contrast_model))
plot(allEffects(grape_contrast_model), main = "Comparison of Treatment Mean with CI")








#########################################
#              PROBLEM 4                #
#########################################


# http://www.sthda.com/english/wiki/two-way-anova-test-in-r

# data 
Measurement    = c(52, 64, 72, 60, 67, 55, 78, 68, 86, 72, 43, 51)
thickness_glue = rep(c("Thin", "Moderate", "Thick"), each = 4)
material_type  = rep(rep(c("plastic", "wood"), each = 2), times = 3)
force          = data.frame(thickness_glue, material_type, Measurement)
    # str(force)
    # table(force$material_type, force$thickness_glue)
    # head(force)

# vizualise

force_box_plot = force %>% 
  ggplot(aes(x      = material_type, 
             y      = Measurement, 
             colour = thickness_glue))+
  geom_boxplot() 
force_box_plot



force.mean.di = force %>% 
  group_by(material_type, thickness_glue) %>%
  summarize(m = mean(Measurement))


force %>% 
  ggplot(aes(x      = material_type, y = Measurement, colour = thickness_glue, shape = thickness_glue)) +
  geom_hline(aes(yintercept = 0), colour = "black", linetype = "solid", size = 1) +
  geom_point(alpha  = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = force.mean.di, aes(y = m), size = 4) +
  geom_line(data  = force.mean.di, aes(y = m, group = thickness_glue), size = 1.5) +
  labs(title = "Force Interaction Plot, Material Type by Force Measurement") + 
  theme(plot.title = element_text(hjust = 0.5))

# Creating model
fit_interaction = lm(Measurement ~ material_type*thickness_glue, data = force)
    # summary(fit_interaction)
    # dummy.coef(fit_interaction)
    # model.matrix(fit_interaction)
Anova(fit_interaction, type = 3) # interaction IS significatn at the 0.05 level


# Effects of the Glue effects across the levels of Thickness
glue.lsm  = lsmeans(fit_interaction, list(pairwise ~ material_type|thickness_glue), adjust = "turkey")
var1 = summary(glue.lsm)[[1]]

ggplot(var1, aes(x = thickness_glue, y = lsmean)) +
  geom_line(aes(group = 1), size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, size = 1) +
  geom_point(aes(y = lsmean), size = 3, shape = 21, fill = "white") +
  facet_wrap(~material_type)
 

thick.lsm = lsmeans(fit_interaction, list(pairwise ~ thickness_glue|material_type), adjust = "bonferroni")
var2 = summary(thick.lsm)[[1]]

ggplot(var2, aes(x = material_type, y = lsmean)) +
  geom_line(aes(group = 1), size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, size = 1) +
  geom_point(aes(y = lsmean), size = 3, shape = 21, fill = "white") +
  facet_wrap(~thickness_glue)












# line plot of glue effects across levels of thickness
force_glht = glht(aov(fit_interaction), linfct = mcp(thickness_glue = "Tukey"))
summary(force_glht, test = adjusted("bonferroni"))





d = summary(lsmeans(fit_interaction, ~thickness_glue))

ggplot(d, aes(thickness_glue)) +
  geom_line(aes(y = lsmean,  group = 1)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_point(aes(y = lsmean), size = 3, shape = 21, fill = "white") +
  labs(x = "Thickness", y = "Force Required", title = "ls mean result over time") +
  theme_bw()


# line plot of the pairwise comparison of glue effects across levels of thickness
d2 = summary(lsmeans(fit_interaction, list(pairwise ~ thickness_glue), adjust = "bonferroni"))

ggplot(d2, aes(thickness_glue)) +
  geom_line(aes(y = lsmean,  group = 1)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_point(aes(y = lsmean), size = 3, shape = 21, fill = "white") +
  labs(x = "Thickness", y = "Force Required", title = "ls mean result over time") +
  theme_bw()




lsmeans(fit_interaction, list(pairwise ~ thickness_glue | material_type), adjust = "bonferroni")

lsmeans(fit_interaction, list(pairwise ~ thickness_glue), adjust = "bonferroni")
lsmeans(fit_interaction, list(pairwise ~ material_type), adjust = "bonferroni")











#########################################
#           The Grave yard              #
#########################################

# 1 ##############################################################################################################################

      #FoHF_fit_2 = lm(FoHF ~ RV + CA + FIA + CTA, data = FoHF)       # [RV, CA, FIA, CTA] selected for model
      #summary(FoHF_fit_2)
      
      #autoplot(FoHF_fit_2, which = 1:2, colour = 'red', size = 2,
      #         smooth.colour = 'black', smooth.linetype = 'dashed',
      #         ad.colour = 'blue', ad.size = 1,
      #         label.size = 2, label.n = 5, label.colour = 'blue')

      #scatterplotMatrix(FoHF, data = FoHF)

res.legend = subsets(bestsubs, statistic = "adjr2", legend = FALSE, main = "Adjusted R^2")
res.legend = subsets(bestsubs, statistic = "cp", legend = FALSE, main = "AIC (CP)")
abline(a = 1, b = 1, lty = 2)
res.legend = subsets(bestsubs, statistic = "bic", legend = FALSE, main = "BIC")

FoHF_2 = FoHF %>%
  select(FoHF, RV, CA, FIA, CTA)

ggpairs(FoHF_2)
scatterplotMatrix(FoHF_2)

#This does not work for any of them
FoHF_2 = FoHF_2 %>%
  mutate(log_FoHF = log(FoHF),
         log_RV   = log(RV),
         log_CA   = log(CA),
         log_FIA  = log(FIA),
         log_CTA   = log(CTA))

FoHF_2 = FoHF_2 %>%
  mutate(FoHF2 = (FoHF^2),
         RV2   = (RV^2),
         CA2   = (CA^2),
         FIA2  = (FIA^2),
         CTA2  = (CTA^2))

FoHF = FoHF %>%
  #  mutate(CA_log  = log(CA)) +    # NaNs... All logs produce NaN
  #  mutate(FIA_log = log(FIA)) + 
  #  mutate(ED_log  = log(ED)) + 
  #  mutate(GM_log  = log(GM)) +  
  #  mutate(CTA_log = log(CTA)) +
  #  mutate(SS_log  = log(SS))
  #  mutate(CA_2  = (CA^2)) +    
  mutate(FIA_2 = (FIA^2)) + 
  mutate(ED_2  = (ED^2)) + 
  mutate(GM_2  = (GM^2)) +  
  mutate(CTA_2 = (CTA^2)) +
  mutate(SS_2  = (SS^2))

FoHF$CA_2 = NULL  

FoHF = FoHF %>%
  mutate(FIA_2 = (FIA^2))

FoHF_fit_2 = lm(FoHF ~ RV + CA + FIA + CTA, data = FoHF)       # [RV, CA, FIA, CTA] selected for model

anova(FoHF_fit_1, FoHF_fit_2)

# not sure how to interpert these functions, but this would be for part D
# Need to determine if the fits used are correct. 

AIC(FoHF_fit_1, FoHF_fit_2)
BIC(FoHF_fit_1, FoHF_fit_2)

# 2 ##############################################################################################################################


# might help
#my_data$dose <- factor(my_data$dose, 
#                      levels = c(0.5, 1, 2),
#                       labels = c("D0.5", "D1", "D2"))
#head(my_data)
# http://www.sthda.com/english/wiki/two-way-anova-test-in-r



# ATTEMPT 1:

# Data:
C   = c(507, 551, 525, 533, 516)
Lo1 = c(568, 525, 551, 577, 568)
Lo2 = c(586, 612, 620, 577, 568)
Lo3 = c(629, 577, 568, 595, 586)
Hi1 = c(586, 612, 568, 551, 595)
Hi2 = c(612, 595, 586, 629, 647)
hi3 = c(595, 612, 647, 595, 612)

cme = data.frame(C, Lo1, Lo2, Lo3, Hi1, Hi2, hi3)
cme %>% View()

# summary(cme)
# str(cme)


# this is the long format of the data:
cme.long = cme %>% gather(treatment, y)
cme.long$treatment = as.factor(cme.long$treatment)



# visualize the data:
ggplot(cme.long)+
  aes(x = treatment, y = y, colour = treatment)+
  geom_point(size = 4)+
  geom_hline(yintercept = mean(cme.long$C),
             colour = "black", linetype = "dashed", size = 1)+
  stat_summary(fun.y = mean, geom = "point", shape = "-", size = 12, 
               colour = "blue")+
  labs(x = "Copper sulfate Treatments", y = "Grape Yields (lbs)")+
  theme(axis.text.y = element_text(size = 12, colour = 'black'))+
  theme(axis.text.x = element_text(size = 15, colour = 'black'))+
  theme(axis.title = element_text(size = 16))

# Cell means Model:
# c.mod = lm(y ~ treatment-1, data = cme.long)
# summary(c.mod)
# model.matrix(c.mod)


# experimental Model:                         #I think this is what we need for the orthogonal coefficients...
e.mod = lm(y ~ treatment, data = cme.long)
summary(e.mod)

summary(allEffects(e.mod))

plot(allEffects(e.mod), main = "Comparison of Treatment Mean with CI")

# determine what the contrasts should be:
levels(cme.long$treatment)
leastsquare = lsmeans(e.mod, "treatment")

# Looking at the contrasts:
contrasts(cme.long$treatment)

# big ANOVA model:
summary(aov(y ~ treatment, data = cme.long))
# with a good p-value = 0 = huge effect of the treatments on biomass

# split:
summary.lm(aov(y ~ treatment, data = cme.long))


# 3 ##############################################################################################################################



# General Linear Hypothesis Testing
fit.sc = glht(fit, linfct = mcp(treatment = c(1, -1, 1, -1, 1, -1))) 
summary(fit.sc)

plot(fit.sc)

# extract coefficients: 
coef(e.mod)
dummy.coef(e.mod)

cme.long$treatment = as.factor(cme.long$treatment)
str(cme.long)
coef(e.mod)

anova(e.mod)
summary(aov(y ~ treatment, data = cme.long))

# design matrix:
model.matrix(e.mod)

# ANOVA model comparison:
g.mod = lm(y ~ 1, data = cme.long)
summary(g.mod)
anova(g.mod, e.mod)

# Checking model assumptions:
autoplot(e.mod)

# get CI for group means
allEffects(c.mod)
summary(allEffects(c.mod))

# plot CI with means
plot(allEffects(e.mod), main = "Comparison of Treatment Mean with CI")


# 4 ##############################################################################################################################


      force.long
      #(c(c(52, 64), c(72, 60), c(67, 55), c(78, 68), c(86, 72), c(43, 51))
      
      force1 = data.frame(ncol= 2, nrow = 3)
      colnames(force1) <- list(c("Plastic","Wood"))
      rownames(force1) <- list(c("Thin","Moderate", "Thick"))
      
      force1$Plastic[1] = list(c(53, 64))
      force1$Plastic[2] = list(c(67, 55))
      force1$Plastic[3] = list(c(86, 72))
      force1$Wood[1]    = list(c(72, 60))
      force1$Wood[2]    = list(c(78, 68))
      force1$Wood[3]    = list(c(43, 51))
      
      force1$Plastic = list(c(53, 64), c(67, 55), c(86, 72))
      force1$Wood    = list(c(72, 60), c(78, 68), c(43, 51)) 
      force1
      
      # outputs -------------- broken
      
      ggboxplot(force, x = "material_type", y = "thickness_glue")
      ggboxplot(force, x = "plastic", y = "wood")
      
      ggline(force, x = "plastic", y = "wood", color = "supp",
             add = c("mean_se", "dotplot"),
             palette = c("#00AFBB", "#E7B800"))
      
      
      #levels(force$material_type)
      #force_fit = lm(y ~ treatment, data = meat)
      
      ggboxplot(force, x = material_type, y = thickness_glue)
      
      
      # additive model
      
      fit_additive = aov(Measurement ~ material_type + thickness_glue, data = force)
      summary(fit_additive) # There was no significance found here. Indicates that
      # there is no difference in chaning just material or just thickenss will have an inpact
      
      
library(tidyverse)

### Example of Multinomial Distribution ###

### assume these are the true probabilities of each class

pi_j = c(0.25, 0.35, 0.2, 0.1, 0.1)  

### just create 1 sample with 1000 trials

n_j = rmultinom(n = 1, size = 1000, prob = pi_j) 

n_j

### shows n_j randomly generated from each class

data.frame(n_j, pihat_j = n_j/1000, pi_j) 

###################################################
######      Wheat Classification Example     ######
###################################################

wheat = read_csv(file.choose())

wheat %>% View()

wheat = wheat %>% 
         mutate(class = as.factor(class),
                 type = as.factor(type))

### observe that 'Healthy' is the base case

levels(wheat$type)  

### Let's start with a multinomial model to predict 
### type on the basis of these 6 key predictors, 
### using the nultinom function from nnet package

# install.packages("nnet")
library(nnet)
fit = multinom(type ~ class + density + hardness + 
                      size + weight + moisture, 
               data = wheat)
summary(fit)

library(car)
Anova(fit)

### Here are two equivalent ways to see the estimated 
### probabilities for each class for each observation:

head(fit$fitted.values)
head(predict(fit, newdata = wheat, type = 'probs'))
head(predict(fit, newdata = wheat, type = 'class'))

### find st dev for continuous vars (we'll use for 'c')

sd_wheat = wheat %>% 
            select(-class, -type) %>% 
            summarise_all(funs(sd)) %>%
            as.numeric()
  
c_value = c(1, sd_wheat)
c_value

beta_hat_scab = coef(fit)[1, 2:7]
beta_hat_sprout = coef(fit)[2,2:7]

### We calculate the OR (according to a c = 1 * sd change) 
### for all variables, but can only change one at a time

or_scab = exp(c_value * beta_hat_scab)
round(or_scab, 2)  # OR for a c-unit increase (scab vs healthy)
round(1/or_scab, 2)  # OR for a c-unit descrease (scab vs healthy)

or_sprout = exp(c_value * beta_hat_sprout)
round(or_sprout, 2)  # OR for a c-unit increase (sprout vs healthy)
round(1/or_sprout, 2)  # OR for a c-unit descrease (sprout vs healthy)

ci_betas = confint(fit, level = 0.95)

or_scab_ci = exp(c_value * ci_betas[2:7, 1:2, 1])  # make sure to get the set for scab
round(or_scab_ci, 2)  # OR CI for a c-unit increase
round(1/or_scab_ci, 2) # OR CI for a c-unit decrease
or_sprout_ci =  exp(c_value * ci_betas[2:7, 1:2, 2])  # make sure to get the set for sprout
round(or_sprout_ci, 2)  # OR CI for a c-unit increase
round(1/or_sprout_ci, 2) # OR CI for a c-unit decrease

##############################################################
###### Multi-class classification with machine learning ######
##############################################################

### First, we will split the original dataset into 3 subsets:

# 1. A train-subset containing 195 cases, for model training
# 2. A validation subset of 14 cases for calibrating the model in h2o
# 3. An independent subset (66 cases) for testing the trained model.

### The caret package (short for 
### _C_lassification _A_nd _RE_gression _T_raining) 
### is a set of functions that attempt to streamline 
### the process for creating predictive models. 
### The package contains tools for:

#data splitting
#pre-processing
#feature selection
#model tuning using resampling
#variable importance estimation

### as well as other functionality.
install.packages("caret")
library(caret)
set.seed(123)

idTrain = createDataPartition(y    = wheat$type, 
                              p    = 0.7, 
                              list = FALSE)
trainset = wheat[idTrain,]

remain = wheat[-idTrain,]

idtest = createDataPartition(y    = remain$type, 
                             p    = 0.15, 
                             list = FALSE)
validset = remain[idtest,]
testset  = remain[-idtest,]

### Our next step consists of exploring the 
### potential contribution of 6 features. 
### This could be done using the mlr package:
install.packages("mlr")
library(mlr)

install.packages("FSelector")
library(FSelector)

task = makeClassifTask(id     = "type", 
                       data   = trainset, 
                       target = "type")

generateFilterValuesData(task, 
                         method = "information.gain") %>%
                        .$data %>%
                        ggplot(aes(x = reorder(name, information.gain),
                                   y = information.gain,
                                   fill = reorder(name, -information.gain)))+
                        geom_bar(stat  = "identity",
                                 color = "black",
                                 show.legend = F)+
                        scale_x_discrete("Features")+
                        coord_flip()+
                        scale_fill_brewer(palette   = "Reds",
                                          direction = -1)

### The information gain results indicate that 
### NOT all features contribute to the type classification. 
### density, weight, and size were the most important 
### features in our problem. 
### Despite that such exploration is not always accurate, 
### its result might eventually help us 
### filtrating the irrelevant features.

Control = caret::trainControl(method          = "repeatedcv",
                              number          = 10,
                              repeats         = 10,
                              classProbs      = TRUE,
                              summaryFunction = caret::multiClassSummary)

install.packages("rattle")
install.packages("rpart.plot")
install.packages("partykit")
install.packages("party")

library(rattle)
library(rpart.plot)
library(partykit)
library(party)


set.seed(123)
cart = caret::train(type~., 
                    data      = trainset,
                    method    = "rpart",
                    trControl = Control)

### Plot the decision tree

fancyRpartPlot(cart$finalModel,
               palettes = "Reds")

### Make predictions 

predcart = predict(cart, 
                   newdata = testset)

confusionMatrix(predcart, 
                reference = testset$type, 
                mode      = "everything")

#############################################
###### Gradient boosting machine (GBM) ######
#############################################

### Gradient boosting machine (GBM) is an 
### Ensemble algorithm that implies Boosting 
### method to combine weak Decision trees together. 
### The principle of Boosting consists of 
### developing multiple weak learners 
### (decision tree) and consecutively isolating 
### the difficult cases within the training data 
### so new learners will focus and adapt to 
### handle them. In other word, 
### we try to reuse a weak learning method 
### several times, each training step will 
### focus on the observations that previous 
### steps failed to classify, then combining 
### their outputs to get a stronger model. 
### Final predictions will be based on majority 
### vote of the weak learners’ predictions, 
### weighted by their individual accuracy.

### We will adopt the GBM algorithm integrated 
### in h2O package. The first thing to do is 
### initialising our h2o package in R.

library(h2o)

h2o.init(nthreads = -1, max_mem_size = "4g")

### then we train a GBM learner on training and validation datasets:

### Following parameters are introduced for 
### training a GBM learner in h2o. 

# Ntrees = how many trees (elementary weak 
#          learner) to be built ?

# Max_depth = How deep each tree is 
#             allowed to grow (the complexity 
#             level for each tree ?)

# Min_rows determines the required number of rows 
# to make a leaf node in decision tree. 
# The default is 10, lower value might 
# lead to overfitting.

# Sampe_rate controls the randomised 
# features feeding to the learner

### We also applied early stopping criteria 
### that based on `mean_per_class_error` metric 
### and a reproducible training process using seed.

wdf    = as.h2o(wheat)
wtrain = as.h2o(trainset)
wtest  = as.h2o(testset)
wvalid = as.h2o(validset)

response = "type"
features = setdiff(colnames(wtrain), response)

gbmod1=h2o.gbm(x = features,
               y = response,
               training_frame       = wtrain,
               nfolds               = 5,
               validation_frame     = wvalid,
               categorical_encoding = "Enum",
               fold_assignment      = "Stratified",
               balance_classes      = TRUE,
               ntrees               = 100, 
               max_depth            = 10,
               min_rows             = 30,
               sample_rate          = 0.8,
               stopping_metric      = "mean_per_class_error",
               stopping_tolerance   = 0.001,
               stopping_rounds      = 5,
               keep_cross_validation_fold_assignment = TRUE, 
               keep_cross_validation_predictions = TRUE,
               score_each_iteration = TRUE,
               seed = 123)

### Evaluating the GBM models’ performance

h2o.performance(gbmod1, wtest)

predgbm = predict(gbmod1, newdata = wtest) %>% as.data.frame()

confusionMatrix(predgbm$predict, 
                reference = testset$type, 
                mode      = "everything")

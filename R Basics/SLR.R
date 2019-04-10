### Load packages

install.packages("ggpmisc")
install.packages("gridExtra")


library(tidyverse)
library(ggpmisc)
library(gridExtra)

### Let's bring in the cleaned Mcfood data 
### Visualize the relationship between 
### Calories and Serving.Size for solid food

food = read_csv(file.choose())

### The scatter plot

food %>%
  filter(Type == 'food.g') %>%
  ggplot(aes(y = Calories, x = Serving.Size))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(25, 650, by = 25))+
  scale_y_continuous(breaks = seq(0, 1900, by = 100))+
  labs(title    = "The relationship between Calories and Serving.Size for Solid Food",
       x = "Serving Size",
       y = "Calories",
       caption = "Graph by ycao@iup.edu, Fall 2018")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))+
  geom_smooth(method = "lm", se = FALSE, size = 1.5)+
  stat_poly_eq(formula   = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse     = TRUE)

?geom_smooth

############################ Fitting the Model ########################

### replace food with only solid food data

food = food %>%
 filter(Type == 'food.g')

### Save the output of your linear regression 
### in a variable called slr.

slr = lm(Calories ~ Serving.Size, data = food)
slr

### Use the summary() command on your slr object to get a 
### print-out of your regression results

summary(slr)

### extract the model matrix ($\mathbf{x}$)

xmat = model.matrix(slr)

### the response variable y:

y = food$Calories

### Now let's construct $(\mathbf{x}^{'}\mathbf{x})^{-1}$. 
### `t()` does transpose and `%*%` does matrix multiplication. 
### `solve(A)` computes $\mathbf{A}^{-1}$:

 #     [ (x'x^-1)]
XtXi = solve(t(xmat) %*% xmat)

### We can get $\hat{\boldsymbol{\beta}}$ directly, using 
### $(\mathbf{x}^{'}\mathbf{x})^{-1}\mathbf{x}^{'}\mathbf{y}$:

XtXi %*% t(xmat) %*% y

### This is a very bad way to compute $\hat{\boldsymbol{\beta}}$. 
### It is inefficient and can be very inaccurate when the predictors 
### are strongly correlated. Such problems are exacerbated by `big data`. 
### A better, but not perfect, way is:

solve(crossprod(xmat), crossprod(xmat, y))

### where `crossprod(xmat, y)` computes $\mathbf{x}^{'}\mathbf{y}$. 
### Here we get the same result as `lm()` because the data are well-behaved. 
### In the long run, you are advised to use carefully programmed code 
### such as found in `lm()` which uses the `QR decomposition`. 

### We can extract the regression quantities we need from the model object. 
### Commonly used are `residuals()`, `fitted()`, `df.residual()` which gives 
### the degrees of freedom, `deviance()` which gives the `SSE` and 
### `coef()` which gives the $\hat{\boldsymbol{\beta}}$. 
### You can also extract other needed quantities by examining 
### the model object and its structure:

names(slr)
str(slr)

### For instance, Use the `coef()` function to get the estimated coefficients.

coef(slr)

## or

slr$coef

################## Checking Model Assumptions ######################

###### This function is needed for making QQplot with ggplot2 ######
####################################################################

qqline_params = function(x){
  y = quantile(x[!is.na(x)], c(0.25, 0.75))
  x = qnorm(c(0.25, 0.75))
  slope = diff(y) / diff(x)
  int = y[1L] - slope * x[1L]
  return(list(slope = slope, int = int))
}
####################################################################

### Before we make the qqplot, can you make a histogram of the residuals

res = data.frame(slr$resid)
res

ggplot(res, aes(x = slr.resid)) +
  geom_histogram(aes(y = ..density..), fill = "seagreen")+ 
  scale_x_continuous(breaks = round(seq(min(res$slr.resid), max(res$slr.resid), by = 100), 2))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

### Or, directly

ggplot(slr, aes(x = slr$resid)) +
  geom_histogram(aes(y = ..density..), fill = "seagreen")+ 
  scale_x_continuous(breaks = round(seq(min(slr$resid), max(slr$resid), by = 100), 2))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

### QQplot 

qq = ggplot(slr, aes(sample = .resid))+
      geom_point(alpha = 0.7, stat = "qq", size = 5, col = "red")+
      geom_abline(intercept = qqline_params(slr$resid)$int, 
                  slope = qqline_params(slr$resid)$slope, size = 1)+
      xlab("Theoretical Quantiles")+ylab("Residuals")+
      ggtitle("Normal Q-Q Plot")
qq

### Residuals vs. Fitted Values

evsf = ggplot(slr, aes(.fitted, .resid))+
       geom_point(alpha = 0.7, size = 5, col = "red")+
       geom_hline(yintercept = 0, col = "blue", linetype = "dashed", size = 1)+
       xlab("Fitted values")+ylab("Residuals")+
       ggtitle("Residual vs Fitted Plot")

evsf

### Lagged plot

ind = 1:length(slr$resid)
dat = data.frame(ind, slr$resid)
names(dat) = c("x", "y")
indplot = ggplot(dat, aes(x, y))+
        geom_point(alpha = 0.7, size = 5, col = "red")+
        geom_hline(yintercept = 0, col = "blue", linetype = "dashed", size = 1)+
        scale_x_continuous(breaks = ind)+
        xlab("Run Orders")+ylab("Residuals")+
        ggtitle("Index Plot")
indplot

# use package GridExtra for this:
grid.arrange(qq, evsf, indplot, ncol = 3)

### Wrap 3 plots up and make a diagnostic function

diagplot = function(model){

library(ggplot2)
library(gridExtra)

qqline_params = function(x){
  y = quantile(x[!is.na(x)], c(0.25, 0.75))
  x = qnorm(c(0.25, 0.75))
  slope = diff(y) / diff(x)
  int = y[1L] - slope * x[1L]
  return(list(slope = slope, int = int))}

qq = ggplot(model, aes(sample = .resid))+
  geom_point(alpha = 0.7, stat = "qq", size = 5, col = "red")+
  geom_abline(intercept = qqline_params(model$resid)$int, 
              slope = qqline_params(model$resid)$slope, size = 1)+
  xlab("Theoretical Quantiles")+ylab("Residuals")+
  ggtitle("Normal Q-Q Plot")

evsf = ggplot(model, aes(.fitted, .resid))+
  geom_point(alpha = 0.7, size = 5, col = "red")+
  geom_hline(yintercept = 0, col = "blue", linetype = "dashed", size = 1)+
  xlab("Fitted values")+ylab("Residuals")+
  ggtitle("Residual vs Fitted Plot")

ind = 1:length(model$resid)
dat = data.frame(ind, model$resid)
names(dat) = c("x", "y")
indplot = ggplot(dat, aes(x, y))+
  geom_point(alpha = 0.7, size = 5, col = "red")+
  geom_hline(yintercept = 0, col = "blue", linetype = "dashed", size = 1)+
  scale_x_continuous(breaks = ind)+
  xlab("Run Orders")+ylab("Residuals")+
  ggtitle("Index Plot")

grid.arrange(qq, evsf, indplot, ncol = 3)
}

diagplot(slr)

slr %>% summary()
dim(food)


################# Inference ###############################################

### CI for the point estimates (The goal is to reflect the uncertainty)
### By default, it gives a 95% CI 

confint(slr)

### You can change the Confidence level by using the `level` argument

confint(slr, level = 0.95)

confint(slr, level = 0.9)

### If you just want the CI for the slope

confint(slr, "Serving.Size") 

### given the data, this CI looks plausible. This reflects the uncertainty 
### and variability in our regression analysis. 
### If the 95%-CI seems unacceptably wide, all we can do is trying to bring
### se() down.

### Obtain the ANOVA table

anova(slr)

#also

summary(slr)


### How the global F-test works 
### Technically, the global F-test is a hierarchical model comparison 

### Fit the intercept-only model

nullmod = lm(Calories ~ 1, data = food)
nullmod
# 1 is the intercept

mean(food$Calories)
# Response Variable

anova(nullmod, slr)


summary(slr)

################# Prediction ###############################################

          # create a new data frame        Creates a normal CI table
predict(slr, newdata = data.frame(Serving.Size = 100), interval = "confidence")

#                                                  vectors
predict(slr, newdata = data.frame(Serving.Size = c(100, 200, 300)), interval = "confidence")


predict(slr, newdata = data.frame(Serving.Size = c(100, 200, 300)), interval = "prediction")

### Plot Intervals on SLR

ggplot(food, aes(y = Calories, x = Serving.Size))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(25, 650, by = 25))+
  scale_y_continuous(breaks = seq(0, 1900, by = 100))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm",  color = 'blue', size = 1)

newsize = data.frame(Serving.Size = seq(min(food$Serving.Size), max(food$Serving.Size), length = 10))

p1 = data.frame(predict(slr, newdata = newsize, interval = ("confidence")))
p2 = data.frame(predict(slr, newdata = newsize, interval = ("prediction")))
p1$interval = "Confidence"
p2$interval = "Prediction"
p1$Serving.Size = newsize$Serving.Size
p2$Serving.Size = newsize$Serving.Size
dat = rbind(p1, p2)
names(dat)[1] = "Calories"

g = ggplot(dat, aes(x = Serving.Size, y = Calories))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) 
g = g + geom_line(size = 1, col = "blue")
g = g + geom_point(data = food, aes(x = Serving.Size, y = Calories), size = 5, col = "red")
g

plot(slr)

################# Model Extensions ###############################################

### Read the Automobile Braking Distance distance data

abd = load(file.choose())
abd


summary(abd)
braking
### Scatter plot

library(ggplot2)

ggplot(braking, aes(y = brdist, x = speed))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(18, 122, by = 10))+
  scale_y_continuous(breaks = seq(1, 60, by = 5))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm",  color = 'blue', size = 1, se = F) +
  geom_smooth()

################# Monotonic transformations #####################

## A monotonic function is a function for transforming 
## a set of numbers into a different set of numbers so that 
## the rank order of the original set of numbers is preserved. 
## One major family of monotonic functions is the ladder of powers.

library(tidyverse)

data_frame(x = runif(1000, 0, 10),
           cube = x^3,
           square = x^2,
           identity = x,
           sqrt = sqrt(x),
           cubert = x ^ (1/3),
           log = log(x)) %>%
  gather(transform, value, -x) %>%
  mutate(transform = factor(transform,
                            levels = c("cube", "square", "identity", "sqrt", "cubert", "log"),
                            labels = c("X^3", "X^2", "X", "sqrt(X)", "sqrt(X, 3)", "ln(X)"))) %>%
  ggplot(aes(x, value)) +
  geom_line(size = 2, color = "blue") +
  facet_wrap( ~ transform, scales = "free_y", labeller = label_parsed) +
  labs(title = "Ladder of powers transformations",
       x = "X",
       y = "Transformed X")

## In order for power transformations to “work”, 
## all the values of X need to be positive 

## Which transformation should I use?

## This depends on the situation. Typically we use these transformations 
## to induce linearity between Y and one or more predictors X. 
## Tukey and Mosteller suggest a “bulging rule” for 
## power transformations to make things more linear:

fakedataMT <- function(p = 1, q = 1, n = 500, s = .1) {
  X <- seq(1 / (n + 1), 1 - 1 / (n + 1), length = n)
  Y <- (5 + 2 * X ^ p + rnorm(n, sd = s)) ^ (1 / q)
  return(data_frame(x = X, y = Y))
}

bind_rows(`1` = fakedataMT(p = .5, q = 2),
          `2` = fakedataMT(p = 3, q = -5),
          `3` = fakedataMT(p = .5, q = -1),
          `4` = fakedataMT(p = 3, q = 5),
          .id = "id") %>%
  mutate(id = factor(id, levels = 1:4,
                     labels = c("Log X or Square Y", "Square X or Y",
                                "Log X or Y", "Square X or Log Y"))) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  facet_wrap(~ id, scales = "free_y") +
  labs(title = 'Tukey and Mosteller\'s "Bulging Rule" for monotone transformations to linearity',
       x = "X",
       y = "Y") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

## Notice that you are not limited to transforming your predictors X
## you can also transform the response variable Y. 
## You can use simple scatterplots and residual plots to help determine 
## if a transformation is necessary, but these are not a substitute for good theory. 
## Is there a theoretical reason why the relationship should be 
## curvilinear rather than strictly linear? If so, use that as a guide to determine an appropriate transformation.

### Fit the model

fit.q = lm(brdist ~ I(speed^2), data = abd)

summary(fit.q)
confint(fit.q)

### Braking Distance vs. Speed^2

ggplot(abd, aes(y = brdist, x = (speed^2)))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(18, 122, by = 10))+
  scale_y_continuous(breaks = seq(1, 60, by = 5))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm",  se = F)

ggplot(abd, aes(y = brdist, x = speed))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(18, 122, by = 10))+
  scale_y_continuous(breaks = seq(1, 60, by = 5))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm", formula = y ~ I(x^2), se = F)

### Your turn: Do residual analysis 


### Example: Infant Mortality

library(car)
data(Leinhardt)
Leinhardt

### remove missing values

im = 

### remove two outliers 

rownames(im)
im["Libya",]

im = 

library(ggplot2)

ggplot(im, aes(y = infant, x = income))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(0, 6000, by = 500))+
  scale_y_continuous(breaks = seq(0, 400, by = 50))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm",  se = F)

ggplot(im, aes(y = infant, x = income))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(0, 6000, by = 500))+
  scale_y_continuous(breaks = seq(0, 400, by = 50))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm", formula = y ~ I(x^-1), se = F)

### Fit the model

fit = lm(infant ~ I(income^-1), data = im)

summary(fit)

### Scatter plot for the log-log model

ggplot(im, aes(y = log(infant), x = log(income)))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(3, 9, by = 1))+
  scale_y_continuous(breaks = seq(2, 6, by = 1))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm",  se = F)

### Fit the log-log model

loglog = lm(log(infant) ~ log(income), data = im)

summary(loglog)

### Your turn: Plot the residuals 


### Plot the fitted log-log model along with CI and PI 

p1 = data.frame(exp(predict(loglog, interval = "confidence")))
p2 = data.frame(exp(predict(loglog, interval = "prediction")))
p1$interval = "Confidence"
p2$interval = "Prediction"
p1$income = im$income
p1$infant = im$infant
p2$income = im$income
p2$infant = im$infant
dat = rbind(p1, p2)

g = ggplot(dat, aes(x = income, y = infant))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.6) 
g = g + geom_line(data = p1, aes(x = income, y = fit), size = 1, col = "blue")
g = g + geom_point(data = im, aes(x = income, y = infant), size = 5, col = "red")
g

### Log response model: Diabetes Example

dat = read_csv(file.choose())
dim(dat)

dia = dat %>%
      select(spending, age)

### Delete missing value 

dia = na.omit(dia)
dim(dia)

### Delete 0 values 

which(dia$spending == 0)

dia = dia[-c(42, 256, 849, 888),]

### Making scatter plot


library(ggplot2)

ggplot(dia, aes(y = tspending, x = age))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(0, 90, by = 10))+
  scale_y_continuous(breaks = seq(0, 110000, by = 5000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm",  se = F)

ggplot(dia, aes(y = log(tspending), x = age))+
  geom_point(color = "red", size = 6)+
  scale_x_continuous(breaks = seq(0, 90, by = 10))+
  scale_y_continuous(breaks = seq(6, 12, by = 1))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  geom_smooth(method = "lm",  se = F)

### Histogram on the `spending` variable 

ggplot(dia, aes(x = tspending)) +
  geom_histogram(aes(y = ..density..))+ 
  scale_x_continuous(breaks = seq(0, 110000, by = 10000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

ggplot(dia, aes(x = log(tspending))) +
  geom_histogram(aes(y = ..density..))+ 
  scale_x_continuous(breaks = seq(6, 12, by = 1))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

### Fit log-response model

logfit = lm(log(spending) ~ age, data = dia)
summary(logfit)




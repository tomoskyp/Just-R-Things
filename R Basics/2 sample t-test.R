##################################################################
######         Run the probability distribution viwer APP   ######
##################################################################

## Probability related functions in R
## For each well-known distributions (eg, norm, binom, t, etc), 
## we have the following functions.

# dDIST: Probability density or mass function (pdf/pmf).
# pDIST: Cumulative distribution function (cdf). 
# qDIST: Inverse cumulative distribution function. 
# rDIST: (Pseudo) random number generator for the distribution.

library(tidyverse)

tibble(x = seq(-5, 5, by = 0.01),
       y = dnorm(x, mean = 0, sd = 1)) %>%
  ggplot(aes(x = x, y = y)) +
  scale_x_continuous(breaks = seq(-5, 5, by = 0.5))+
  geom_line(size = 1, color = 'red')+
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(face = "italic"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))+
  labs(x = "R", y = "Density",
       title = "The Standard Normal Distribution ")

## we can easily compute the probabilities
## P(X <= 0)

?pnorm

pnorm(0, mean = 0, sd = 1)

## or to plot to verify

mydat = tibble(x = seq(from = -5, to = 5, by = 0.01),
               d = dnorm(x = x, mean = 0, sd = 1))

mydat %>%
ggplot(aes(x = x, y = d)) +
    ## Entire curve
    geom_path(size = 1, color = 'red') +
    ## Fill the desired area < 0. Ribbon gives a colored band.
    geom_ribbon(data = filter(mydat, x <= 0), # Use filtered data for this geom only.
                ## Constants are placed outside aes().
                alpha = 0.5,                   # Semi-transparalent.
                ymin = 0,                      # Lower end at 0.
                ## Variables should be specified in aes().
                mapping = aes(ymax = d))     # Upper end at density.

## P(R > 0)

pnorm(0, mean = 0, sd = 1, lower.tail = FALSE)

## Using the R function `qnorm()`, we can find the quantiles 
## q_0.025, q_0.05, q_0.95, and q_0.975:

a.vals = c(0.025, 0.05, 0.95, 0.975)
qnorm(a.vals, mean = 0, sd = 1)

##################################################################
######                    2-sample T-test                   ######
##################################################################

library(tidyverse)
library(broom)

### Make the data frame

beer = c(27, 19, 20, 20, 23, 17, 21, 24, 31, 26, 28, 20, 27, 19, 
         25, 31, 24, 28, 24, 29, 21, 21, 18, 27, 20)
water = c(21, 19, 13, 22, 15, 22, 15, 22, 20, 12, 24, 24, 21, 19, 
          18, 16, 23, 20)

NoM = c(beer, water)
group = c(rep("beer", length(beer)), rep("water", length(water)))

mosquito = tibble(NoM, group)

mosquito

### How do you visualize this data?



### Perform a 2-sample t test

t.test(NoM ~ group, var.equal   = TRUE, 
                    alternative = "greater", 
                    data        = mosquito) %>% tidy()

### You can get the CI for µb-µw

t.test(NoM ~ group, var.equal = TRUE, data = mosquito) %>% tidy()

##################################################################
######                       Permutation                    ######
##################################################################

obsdiff = mean(NoM[group == "beer"]) - mean(NoM[group == "water"])
obsdiff

resamp_means = function(data, group){
  resample_labels = sample(group)
  resample_diff   = mean(data[resample_labels == "beer"]) - 
                    mean(data[resample_labels == "water"])
  return(resample_diff)
}

nperm = 50000
nulldist = replicate(nperm, resamp_means(NoM, group))
pvalue = sum(nulldist >= obsdiff)/ nperm
pvalue

### Visualize

as_tibble(nulldist) %>%
  ggplot(aes(x = nulldist))+ 
   geom_histogram(aes(y =..density..))+
    scale_x_continuous(breaks = round(c(unname(summary(nulldist)[-3]), obsdiff), 2))+
    geom_vline(aes(xintercept = round(obsdiff, 4)), lty = 2, size = 1)+
    geom_point(x = obsdiff, y = 0, size = 5, col = "red")


###### 2 proportion test ######

group = c(rep("control", 12), rep("seed", 24), 
          rep("control", 4), rep("seed", 10))
yawn = c(rep(0, 36), rep(1, 14))

yawn_myth = tibble(subj = seq(1, 50), group, yawn) 

library(janitor)
yawn_myth %>% 
  tabyl(group, yawn) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns()

### 2-prop test with asymptotic theory

prop.test(x = c(10, 4),
          n = c(34, 16), 
          alternative = "greater",
          correct = FALSE)

### Now the permutation 

obsdiff = mean(yawn[group == "seed"]) - mean(yawn[group == "control"])
obsdiff

resamp_means = function(data, group){
  resample_labels = sample(group)
  resample_diff   = mean(data[resample_labels == "seed"]) - 
                    mean(data[resample_labels == "control"])
  return(resample_diff)
}

nperm = 50000
nulldist = replicate(nperm, resamp_means(yawn, group))
pvalue = sum(nulldist >= obsdiff)/ nperm
pvalue

### Visualize

as_tibble(nulldist) %>%
  ggplot(aes(x = nulldist))+ 
   geom_histogram(aes(y =..density..))+
    scale_x_continuous(breaks = round(c(unname(summary(nulldist)[-3]), obsdiff), 2))+
    geom_vline(aes(xintercept = round(obsdiff, 4)), lty = 2, size = 1)+
    geom_point(x = obsdiff, y = 0, size = 5, col = "red")


##################################################################
######                       Bootstrapping                  ######
##################################################################

### 1. Bootstrap CI for a single mean ### 

### Let's revisit the 'mosquitoes' data

### Recall the original data

beer

### The sample mean

mean(beer)

### sample size

n = length(beer)

### Distribution of the original data

as_tibble(beer) %>%
ggplot(aes(x = beer))+ 
geom_histogram(binwidth = 1)+
geom_point(x = round(mean(beer), 2), y = 0, size = 5, col = "red")+
geom_text(x = round(mean(beer), 2), y = 0, label = paste(round(mean(beer), 2)))

### Resample #1

set.seed(201801)
boot_sample = sample(beer, size = n, replace = TRUE)
mean(boot_sample)

as_tibble(boot_sample) %>%
ggplot(aes(x = value))+ 
geom_histogram(binwidth = 1)+
geom_point(x = round(mean(boot_sample), 2), y = 0, size = 5, col = "red")+
geom_text(x = round(mean(boot_sample), 2), y = 0, label = paste(round(mean(boot_sample), 2)))

### Resample #2



### Now bootstrapping 

### Number of bootstrap samples you want

nboot = 1000

### generate the bootstrap distribution

boot_dist = rep(NA, nboot)

for(i in 1:nboot){
    boot_sample = sample(beer, size = n, replace = TRUE)
    boot_dist[i] = mean(boot_sample)
  }

### Get a Point Estimate

### The mean of the bootstrapped statistics is 
### a point estimate of your parameter.

mean(boot_dist)  

### Percentile Bootstrap Confidence Interval
### If we want a 95% Bootstrap CI

bootCI = quantile(boot_dist, c(0.025, 0.975)) 
bootCI

### Compare to the CLT based Method

t.test(beer, conf.level = 0.95)$conf.int

### Plot the Bootstrap Results

ggplot(as.data.frame(boot_dist), aes(x = boot_dist))+ 
geom_histogram(aes(y =..density..))+
geom_point(x = round(mean(boot_dist), 2), y = 0, size = 5, col = "red")+
scale_x_continuous(breaks = c(unname(summary(boot_dist)[-3]), round(unname(bootCI), 2)))+
geom_vline(aes(xintercept = round(unname(bootCI[1]), 2)), lty = 2, size = 1)+
geom_vline(aes(xintercept = round(unname(bootCI[2]), 2)), lty = 2, size = 1)

### 2. Bootstrap CI for a comparing two mean ###

### We want to construct a CI for \mu_beer - \mu_water

### Number of bootstrap samples you want

nboot = 1000

### generate the bootstrap distribution

nbeer = length(beer)
nwater = length(water)

boot_dist = rep(NA, nboot)

for(i in 1:nboot){
    boot_beer = sample(beer, size = nbeer, replace = TRUE)
    boot_water = sample(water, size = nwater, replace = TRUE)
    boot_dist[i] = mean(boot_beer) - mean(boot_water)
  }

### Get a Point Estimate

### The mean of the bootstrapped statistics is 
### a point estimate of your parameter.

mean(boot_dist)  

### Percentile Bootstrap Confidence Interval
### If we want a 95% Bootstrap CI

bootCI = quantile(boot_dist, c(0.025, 0.975)) 
bootCI

### Plot the Bootstrap Results

ggplot(as.data.frame(boot_dist), aes(x = boot_dist))+ 
geom_histogram(aes(y =..density..))+
geom_point(x = round(mean(boot_dist), 2), y = 0, size = 5, col = "red")+
scale_x_continuous(breaks = c(unname(summary(boot_dist)[-3]), round(unname(bootCI), 2)))+
geom_vline(aes(xintercept = round(unname(bootCI[1]), 2)), lty = 2, size = 1)+
geom_vline(aes(xintercept = round(unname(bootCI[2]), 2)), lty = 2, size = 1)

### 3. Bootstrap CI for a single proportion

### Let's revist the yawning example and 
### construct a bootstrap CI for p_seed

### Extract the original example

seed = yawn[group == "seed"]
seed
n = length(seed)

nboot = 1000

boot_dist = rep(NA, nboot)
  for(i in 1:nboot){
    boot_sample = sample(seed, size = n, replace = TRUE)
    boot_dist[i] = mean(boot_sample)
  }

bootCI = quantile(boot_dist, c(0.025, 0.975)) 
bootCI

ggplot(as.data.frame(boot_dist), aes(x = boot_dist))+ 
geom_histogram(aes(y =..density..))+
geom_point(x = round(mean(boot_dist), 2), y = 0, size = 5, col = "red")+
scale_x_continuous(breaks = c(unname(summary(boot_dist)[-3]), round(unname(bootCI), 2)))+
geom_vline(aes(xintercept = round(unname(bootCI[1]), 2)), lty = 2, size = 1)+
geom_vline(aes(xintercept = round(unname(bootCI[2]), 2)), lty = 2, size = 1)

### 4. Bootstrap CI for comparing two proportions

### Extract the original example

seed = yawn[group == "seed"]
nseed = length(seed)

control = yawn[group == "control"]
ncontrol = length(control)

nboot = 1000

boot_dist = rep(NA, nboot)
  for(i in 1:nboot){
    boot_seed = sample(seed, size = nseed, replace = TRUE)
    boot_control = sample(control, size = ncontrol, replace = TRUE)
    boot_dist[i] = mean(boot_seed) - mean(boot_control)
  }

bootCI = quantile(boot_dist, c(0.025, 0.975)) 
bootCI

ggplot(as.data.frame(boot_dist), aes(x = boot_dist))+ 
geom_histogram(aes(y =..density..))+
geom_point(x = round(mean(boot_dist), 2), y = 0, size = 5, col = "red")+
scale_x_continuous(breaks = c(unname(summary(boot_dist)[-3]), round(unname(bootCI), 2)))+
geom_vline(aes(xintercept = round(unname(bootCI[1]), 2)), lty = 2, size = 1)+
geom_vline(aes(xintercept = round(unname(bootCI[2]), 2)), lty = 2, size = 1)
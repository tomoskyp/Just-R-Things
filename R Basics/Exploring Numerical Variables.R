### Load packages

install.packages('tidyverse')
install.packages('ggthemes')
install.packages('ggrepel')

library(tidyverse)
library(ggthemes)
library(ggrepel)

### Now import the cleaned menu data

menu = read_csv(file.choose())

menu

################################################################
######              One Numerical Variable                ######
################################################################

### The measures that will interest us relate to:

### The **center** of the distribution
### The **spread** of the distribution
### The **shape** of the distribution

###### keto diet ######

### It seems like everyone is praising the keto diet these days.
### The ketogenic diet is a low carb, moderate protein, 
### and high fat diet which puts the body into
### a metabolic state known as ketosis.
### The good fat is the Saturated.Fat

### R provides a small sampling of numerical
### summaries with the `summary()` function

menu %>%
select(Saturated.Fat, Carbohydrates, Protein) %>%
summary()

  #TEST: This does not work:
summary(Saturated.Fat, Carbohydrates, Protein)

### Find the min/max
### which.min()/which.max() Determines the location, 
### i.e., index of the (first) minimum or 
### maximum of a numeric vector. 

which.min(menu$Saturated.Fat) #First one with a zero
  #returns [1] 1
  menu[1,]
  
which.max(menu$Saturated.Fat) #First max value
  menu[120,]

which.min(menu$Carbohydrates)
which.max(menu$Carbohydrates)

which.min(menu$Protein)
which.max(menu$Protein)

### To see all minimum values 

menu %>%
select(Item, Saturated.Fat, Carbohydrates, Protein) %>%
slice(which(Protein == min(Protein))) %>%
View()

### To see all maximum values 

menu %>%
select(Item, Saturated.Fat, Carbohydrates, Protein) %>%
slice(which(Protein == max(Protein))) %>%
View()

### Now in **tidyverse**

menu %>%
 select(Saturated.Fat) %>%
 summarize(n = n(), Mean_of_sat_fat = mean(Saturated.Fat), 
           Med_of_sat_fat = median(Saturated.Fat), SD = sd(Saturated.Fat))

fatstats = menu %>%
 select(Saturated.Fat) %>%
 summarize(n = n(), Mean = mean(Saturated.Fat), 
           Med = median(Saturated.Fat), SD = sd(Saturated.Fat))
fatstats

menu %>%
 select(Saturated.Fat) %>%
 summarize_all(funs(min, max, median, mean, sd))

menu %>%
select(Saturated.Fat, Carbohydrates, Protein) %>%
summarize_all(funs(min, max, median, mean, sd)) %>% View()

?summarize_all
       
######  Visualization ######

### Frequency histgram

menu %>%
 ggplot(aes(x = Saturated.Fat)) +
  geom_histogram()

### Improve the histogram

menu %>%
 ggplot(aes(x = Saturated.Fat)) +
  geom_histogram(fill = "lightblue", color = "grey", bins = 50)+ 
  scale_x_continuous(breaks = seq(min(menu$Saturated.Fat), 
                                  max(menu$Saturated.Fat), by = 1))+
  labs(title    = "Distribution of Saturated Fat",
       subtitle = "this is the subtitle",
              x = "Saturated Fat",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### Density histogram

menu %>%
ggplot(aes(x = Saturated.Fat)) +
  geom_histogram(aes(y = ..density..), fill = "hotpink", color = "white", bins = 50)+ 
  scale_x_continuous(breaks = seq(min(menu$Saturated.Fat), 
                                  max(menu$Saturated.Fat), by = 1))+
  labs(title    = "Distribution of Saturated Fat",
       subtitle = "",
              x = "Saturated Fat",
              y = "Density",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))
 

### Density Plot

menu %>%
 ggplot(aes(x = Saturated.Fat)) +
  geom_density(fill = "mediumseagreen", alpha = .6, color = "white") +
  geom_vline(aes(xintercept = mean(Saturated.Fat)), 
             color = "dodgerblue", lty = 3, size = 1) +
  geom_vline(aes(xintercept = median(Saturated.Fat)), 
             color = "firebrick1", lty = 5, size = 1) +
  scale_x_continuous(breaks = seq(min(menu$Saturated.Fat), 
                                  max(menu$Saturated.Fat), by = 1))+
  annotate("text", label = "Mean", x = mean(menu$Saturated.Fat) + 0.5, 
           y = .07, color = "dodgerblue", size = 6) +
  annotate("text", label = "Median", x = median(menu$Saturated.Fat) - 0.6, 
           y = .07, color = "firebrick1", size = 6) +
  labs(title    = "Distribution of Saturated Fat",
       subtitle = "",
              x = "Saturated Fat",
              y = "Density",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### Your turn:
### Make a density histogram and a 
### density plot for Carbohydrates

#DENSITY HISTOGRAM
menu %>%
  ggplot(aes(x = Carbohydrates)) +
  geom_histogram(aes(y = ..density..), fill = "lightblue", color = "grey", bins = 50)+ 
  scale_x_continuous(breaks = seq(min(menu$Carbohydrates), 
                                  max(menu$Carbohydrates), by = 10))+
  labs(title    = "Distribution of Carbohydrates",
       subtitle = "",
       x = "Carbohydrates",
       y = "Density",
       caption = "Graph by lkxt@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

#DENSITY PLOT

menu %>%
  ggplot(aes(x = Carbohydrates)) +
  geom_histogram(aes(y = ..density..), fill = "lightblue", color = "grey", bins = 50)+ 
  geom_density(fill = "mediumseagreen", alpha = .2, color = "white") +
  geom_vline(aes(xintercept = mean(Carbohydrates)), 
             color = "dodgerblue", lty = 3, size = 1) +
  geom_vline(aes(xintercept = median(Carbohydrates)), 
             color = "firebrick1", lty = 5, size = 1) +
  scale_x_continuous(breaks = seq(min(menu$Carbohydrates), 
                                  max(menu$Carbohydrates), by = 10))+
  #scale_y_continuous(breaks = seq(0, 0.05, by = .01))+
  annotate("text", label = "Mean", x = mean(menu$Carbohydrates) + 0.5, 
           y = .02, color = "dodgerblue", size = 6) +
  annotate("text", label = "Median", x = median(menu$Carbohydrates) - 0.6, 
           y = .02, color = "firebrick1", size = 6) +
  labs(title    = "Distribution of Carbohydrates",
       subtitle = "",
       x = "Carbohydrates",
       y = "Density",
       caption = "Graph by lkxt@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))


##################################################
### Numerical Variable VS Categorical Variable ###
##################################################

### Two options 

### 1. "SMALL MULTIPLE" chart

### For example, we want to look at the distribution 
### of food Saturated.Fat in each category of food

menu %>%
 ggplot(aes(x = Saturated.Fat)) +
  geom_histogram(fill = "hotpink", color = "white", bins = 50)+ 
  scale_x_continuous(breaks = seq(min(menu$Saturated.Fat), 
                                  max(menu$Saturated.Fat), by = 1))+
  labs(title    = "Distribution of Saturated Fat",
       subtitle = "",
              x = "Saturated Fat",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))+
  facet_wrap( ~ Category)

menu %>%
 ggplot(aes(x = Saturated.Fat)) +
  geom_density(fill = "mediumseagreen", alpha = .6, color = "white") +
  facet_wrap( ~ Category)+
  scale_x_continuous(breaks = seq(min(menu$Saturated.Fat), 
                                  max(menu$Saturated.Fat), by = 1))+
  labs(title    = "Distribution of Saturated Fat",
       subtitle = "",
              x = "Saturated Fat",
              y = "Density",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))
  
### Your turn:
### Make small multiple density histogram and 
### density plot for Carbohydrates across Category
### and Protein across Category

menu %>%
  ggplot(aes(x = Carbohydrates)) +
  geom_density(fill = "mediumseagreen", alpha = .6, color = "white") +
  geom_histogram(fill = "hotpink", color = "white", bins = 50)+ 
  scale_x_continuous(breaks = seq(min(menu$Carbohydrates), 
                                  max(menu$Carbohydrates), by = 10))+
  labs(title    = "Distribution of Carbohydrates Fat",
       subtitle = "",
       x = "Carbohydrates",
       y = "Count",
       caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))+
  facet_wrap( ~ Category)

menu %>%
  ggplot(aes(x = Carbohydrates)) +
  geom_density(fill = "mediumseagreen", alpha = .6, color = "white") +
  facet_wrap( ~ Category)+
  scale_x_continuous(breaks = seq(min(menu$Carbohydrates), 
                                  max(menu$Carbohydrates), by = 20))+
  labs(title    = "Distribution of Carbohydrates",
       subtitle = "",
       x = "Saturated Fat",
       y = "Density",
       caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))



### 2. Side-by-side boxplot ###

### Very Basic one

menu %>%
ggplot(aes(x = Category, y = Carbohydrates)) +
 geom_boxplot()

### improved one

menu %>%
ggplot(aes(x    = reorder(Category, Carbohydrates), 
           y    = Carbohydrates, 
           fill = Category)) +
  geom_boxplot() +
  guides(fill = FALSE)+
  coord_flip() +
  labs(title    = "Distribution of Carbohydrates Within Each Category",
       subtitle = "",
              x = "",
              y = "Carbohydrates",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))


### Your turn:
### Make a side-by-side boxplot to show the distribution
### of Protein within each Category

menu %>%
  ggplot(aes(x    = reorder(Category, Protein), 
             y    = Protein, 
             fill = Category)) +
  geom_boxplot() +
  guides(fill = FALSE)+
  coord_flip() +
  labs(title    = "Distribution of Protein Within Each Category",
       subtitle = "",
       x = "",
       y = "Protein",
       caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))


##################################################
### Numerical Variable VS Numerical Variable   ###
##################################################

### Since Saturated Fat, protein and carbohydrate content 
### are very important to a 'Keto diet' 
### Let's analyze the relationship/tradeoff among the three
### I am going to take all the items in the 
### Beef & Pork, Chicken & Fish, Salads categories.

### A very basic scatter plot

menu %>% 
  select(Category, Item, Saturated.Fat, Carbohydrates, Protein) %>%
  filter(Category == "Beef & Pork" | 
         Category == "Chicken & Fish" |
         Category == "Salads") %>%
  ggplot(aes(x = Saturated.Fat, y = Carbohydrates))+ 
  geom_point(size = 6)


menu %>% 
  select(Category, Item, Saturated.Fat, Carbohydrates, Protein) %>%
  filter(Category == "Beef & Pork" | 
         Category == "Chicken & Fish" |
         Category == "Salads") %>%
  slice(-41) %>%
  ggplot(aes(x = Saturated.Fat, y = Carbohydrates))+ 
  geom_point(size = 6)

### Add Protein and Food Category 

menu %>% 
  select(Category, Item, Saturated.Fat, Carbohydrates, Protein) %>%
  filter(Category == "Beef & Pork" | 
         Category == "Chicken & Fish" |
         Category == "Salads") %>%
  slice(-41) %>%
  ggplot(aes(x = Saturated.Fat, y = Carbohydrates, color = Category))+ 
  geom_point(aes(size = Protein))


### Now let's try to find the items that 

### to choose

menu %>% 
  select(Category, Item, Saturated.Fat, Carbohydrates, Protein) %>%
  filter(Category == "Beef & Pork" | 
         Category == "Chicken & Fish" |
         Category == "Salads") %>%
  slice(-41) %>%
  ggplot(aes(x = Saturated.Fat, y = Carbohydrates, color = Category))+ 
  geom_point(aes(size = Protein)) +
  geom_label_repel(aes(label = ifelse(Saturated.Fat > 10 & Protein >= 20 & 
                                      Protein <= 30 & Carbohydrates < 50, Item,"")), 
                   fontface      = 'bold', 
                   size          = 4, 
                   color         = '#bf0f1e', 
                   segment.color = 'grey50',
                   force         = 100) +
  scale_color_manual(name   = "", 
                     values = colorRampPalette(c("#ffc300", "#dd1324"))(3))+
  ggtitle('The items to choose...')

### to avoid 

menu %>% 
  select(Category, Item, Saturated.Fat, Carbohydrates, Protein) %>%
  filter(Category == "Beef & Pork" | 
         Category == "Chicken & Fish" |
         Category == "Salads") %>%
  slice(-41) %>%
  ggplot(aes(x = Saturated.Fat, y = Carbohydrates, color = Category))+ 
  geom_point(aes(size = Protein)) +
  geom_label_repel(aes(label = ifelse(Saturated.Fat < 10 & Protein > 30 & Carbohydrates > 50, Item,"")), 
                   fontface      = 'bold', 
                   size          = 4, 
                   color         = '#bf0f1e', 
                   segment.color = 'grey50',
                   force         = 100) +
  scale_color_manual(name   = "", 
                     values = colorRampPalette(c("#ffc300", "#dd1324"))(3))+
  ggtitle('and the items to avoid...')

### Your turn: look at the full menu
### identify the items you would suggest



### Moral of the Story: The amount of carbs one can 
### easily consume by just drinking it shows 
### that McDonald's is really not the place for keto dieters. 
### Even a large Diet Coke or a large Sweet Tea 
### can wreck havoc on your nutrition plan.


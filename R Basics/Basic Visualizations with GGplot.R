### Load packages

library(tidyverse)

### Now import the cleaned menu data

menu = read_csv(file.choose())

menu

### Get a glimpse of your data

menu %>% glimpse()

################################################################
######            One Categorical Variable                ######
################################################################

### Summarizing categorical variables numerically is 
### mostly about building **tables**, and calculating 
### **proportions** or **percentages**

### Let's build a table to describe the 
### distribution of food categories

menu %>%
select(Category) %>%
table()

### the main tools we have for augmenting tables are
### 1. adding in marginal totals 
### 2. working with proportions/percentages 

### adding totals 

menu %>%
select(Category) %>%
table() %>%
addmargins()

### want proportions 

menu %>%
select(Category) %>%
table() %>%
prop.table()

### can also do

menu %>%
select(Category) %>%
table() %>%
prop.table()%>%
addmargins()

menu %>%
select(Category) %>%
table() %>%
prop.table()%>%
"*" (100) 

### Now in **tidyverse**

menu %>% count(Category)

menu %>% count(Category, sort = TRUE) 

menu %>% count(Category, sort = TRUE) %>% add_tally()

foodtable = menu %>% 
             count(Category, sort = TRUE) %>%
             rename(freq = n) %>%
             mutate(prop = freq/sum(freq),
                    perc = freq/sum(freq) *100)

        
######  Visualization ######

### Usually, a **bar chart** is the best choice 

ggplot(data = menu, aes(x = Category)) +
geom_bar() 

### since we have %>%, we can do 

menu %>% 
  ggplot(aes(x = Category)) +
  geom_bar() 

### Now let's work with the foodtable

foodtable %>% 
  ggplot(aes(x = Category, y = freq)) +
  geom_bar(stat = "identity") 

### reorder the bars by count from lowest to highest

foodtable %>% 
  ggplot(aes(x = reorder(Category, freq), y = freq)) +
  geom_bar(stat = "identity") 

### or from highest to lowest

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq)) +
  geom_bar(stat = "identity") 

### Change the bar color

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "white") 

### Change the bar color --- more colorful

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq, fill = Category)) +
  geom_bar(stat = "identity", color = "white") 

### don't want the legend

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq, fill = Category)) +
  geom_bar(stat = "identity", color = "white")+ 
  guides(fill = FALSE)

### Change the lables --- contents

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq, fill = Category)) +
  geom_bar(stat = "identity", color = "white")+ 
  guides(fill = FALSE)+
  labs(title    = "Distribution of Food Categories on Mc Menu",
       subtitle = "9 types of food and drink",
              x = "Categories",
              y = "Count",
        caption = "Graph by ycao@iup.edu")

### Change the lables --- font size and positions

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq, fill = Category)) +
  geom_bar(stat = "identity", color = "white")+ 
  guides(fill = FALSE)+
  labs(title    = "Distribution of Food Categories on Mc Menu",
       subtitle = "9 types of food and drink",
              x = "Categories",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### Adjust the scale of y axis

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq, fill = Category)) +
  geom_bar(stat = "identity", color = "white")+ 
  scale_y_discrete(limits = seq(0, 95, 3))+
  guides(fill = FALSE)+
  labs(title    = "Distribution of Food Categories on Mc Menu",
       subtitle = "9 types of food and drink",
              x = "Categories",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### lable numbers on the bars --- lable freq

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq, fill = Category)) +
  geom_bar(stat = "identity", color = "white")+ 
  scale_y_discrete(limits = seq(0, 95, 5))+
  guides(fill = FALSE)+
  geom_text(aes(label = freq), 
            vjust = 0, 
            colour = "black", 
            size = 5)+
  labs(title    = "Distribution of Food Categories on Mc Menu",
       subtitle = "9 types of food and drink",
              x = "Categories",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### lable numbers on the bars --- lable percentage

foodtable %>% 
  ggplot(aes(x = reorder(Category, -freq), y = freq, fill = Category)) +
  geom_bar(stat = "identity", color = "white")+ 
  scale_y_discrete(limits = seq(0, 95, 5))+
  guides(fill = FALSE)+
  geom_text(aes(label = paste0(round(perc, 1), "%")), 
            vjust = 0, 
            colour = "black", 
            size = 5)+
  labs(title    = "Distribution of Food Categories on Mc Menu",
       subtitle = "9 types of food and drink",
              x = "Categories",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### Make horizontal bar plot

foodtable %>% 
  ggplot(aes(x = reorder(Category, freq), y = freq, fill = Category)) +
  geom_bar(stat = "identity", color = "white")+ 
  scale_y_continuous(breaks = seq(0, 100, 5))+
  guides(fill = FALSE)+
  geom_text(aes(label = paste0(round(perc, 1), "%")), 
            vjust = 0, 
            colour = "black", 
            size = 5)+
  labs(title    = "Distribution of Food Categories on Mc Menu",
       subtitle = "9 types of food and drink",
              x = "Categories",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))+
  coord_flip()

###### Your turn ######
### Make a bar plot for the 'Item' 
### with the hight of the bars to be the Calories


menu %>% 
  ggplot(aes(x = reorder(Item, Calories), y = Calories, fill = Category, label = Calories)) +
  geom_bar(stat = "Identity")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "top")


### Well, there are too many items!!! 
### So, let us just plot the items with 
### the top 20 calories



menu %>% 
  arrange(desc(Calories)) %>%
  head (20) %>%
  ggplot(aes(x = reorder(Item, Calories), y = Calories, fill = Category, label = Calories)) +
  geom_bar(stat = "Identity")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "top")
  geom_text(hjust = 1.3, colour = "white")+
  xlab("")+
  ggtitle("Top 20 products with the most calories")



################################################################
######       Two or more Categorical Variables            ######
################################################################

### Contingency tables provide a way to display the 
### frequencies and relative frequencies of 
### observations, which are classified according 
### to two categorical variables. 
### The elements of one category are displayed 
### across the columns; the elements of the other 
### category are displayed over the rows.

### As an example, let's see if there is a relationship
### between Food Category and Food type

menu %>%
select(Category, Type) %>%
table()

### adding totals 

menu %>%
select(Category, Type) %>%
table() %>%
addmargins()

### To get row percentage 

menu %>%
select(Category, Type) %>%
table() %>%
prop.table(1)%>%
"*" (100) %>%
round(2)

### For column percentage 

menu %>%
select(Category, Type) %>%
table() %>%
prop.table(2)%>%
"*" (100) %>%
round(2)

### Use the janitor package

install.packages('janitor')

library(janitor)

menu %>%
tabyl(Category, Type) %>%
adorn_percentages() %>%
adorn_pct_formatting() %>%
adorn_ns()

### Now in **tidyverse**

menu %>% count(Category, Type)

### visualizing 

menu %>% 
ggplot(aes(x = Category, fill = Type))+ 
  geom_bar()+
  labs(title    = "Stacked Bar Chart: Distribution of Food Categories over Food Type",
              x = "Categories",
              y = "Count",
        caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

### Or the other way around
### Now change x to Type 
### fill to Category

menu %>% 
  ggplot(aes(x = Category, fill = Type))+ 
  geom_bar()+
  labs(title    = "Stacked Bar Chart: Distribution of Food Categories over Food Type",
       x = "Categories",
       y = "Count",
       caption = "Graph by ycao@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))


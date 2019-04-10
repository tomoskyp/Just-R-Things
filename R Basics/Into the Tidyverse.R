### Getting started in tidyverse: https://www.tidyverse.org/ ###

### To use an R package, you must install it (once per computer):

# install.packages("tidyverse")

### This is similar to "buying" a book from a bookstore and 
### adding it to your personal library (e.g. bookshelf).

### To use the functions within an R package, you should load it 
### into your working environment (once for every R session). 
### The best way to do this is to use the 
### library() function like so:

library(tidyverse)

### This is the equivalent of doing:
### I have a book in my library title 'tidyverse'. 
### Good sir, would you retrieve it and, then, 
### open it to the first page?

#####################################################
######            pipe operator               #######
#####################################################

### Piping is the act of taking one value and 
### immediately placing it into another function 
### to form a flow of results.

###### object %>% function(_, ...) ######
### Take the left object, and then, 
### pass it to the first argument in the right function

4 %>%  # Take the number four and, then
sqrt() # find the square root

### Same as
### sqrt(4)

c(7, 42, 1, 25) %>%   # Combine four elements and, then
          log() %>%   # take the natural log and, then
       round(2) %>%   # round to the second decimal and, then
         diff()       # take the difference between consecutive elements

### Same as
# diff(round(log(c(7,42,1,25)), 2))

### Argument Order ###

### piping to a parameter other than 
### the first using a period, `.`

my_func = function(x, y) {x - y}

x = 3
y = 1

my_func(x, y)

x %>% my_func(y)    # Default, e.g. my_func(x, y)

x %>% my_func(., y) # Default, e.g. my_func(x, y)

y %>% my_func(x, .) # Pipe y to second argument, e.g. my_func(x, y)

x %>% my_func(y, .) # Pipe x to second argument, e.g. my_func(y, x)

###############################################################

### Importing data ###

menu = read_csv(file.choose())

menu

### After importing data in R, there’s a bunch of
### functions to inspect a data frame object

### accessing the type of the object

menu %>% class()

### Get a glimpse of your data

menu %>% glimpse()

### Print out more rows 

menu %>% print(n = 20)

### head() function returns the first 6 rows of any dataframe.

menu %>% head()

menu %>% head(20)

### tail() function returns the last 6 rows of any dataframe.

menu %>% tail()

### finding out the dimensions of dataframe using dim()

menu %>% dim()

### finding out the row count of dataframe using nrow()

menu %>% nrow()

### finding out the column count of dataframe using ncol()

menu %>% ncol()

### column summaries

menu %>% summary()

### An alternative function for summarizing the data

install.packages('skimr')
library(skimr)

menu %>% skim()

### memory size

menu %>% object.size()

menu %>% object.size() %>% print(units = "Mb")

### Look at the whole data in a seperate window

menu %>% View()

### Renaming some columns to reflect Percent Daily Value (PDV)

### Extract variable names

names(menu)

?gsub

### create a vector contains the new names 

new_names = gsub(pattern     = "*....Daily.Value.", 
                 replacement = ".PDV", 
                 names(menu))

new_names

### Replace old names with the new_names we just created 

names(menu) = new_names
names(menu)

### get rid of the space in names by replacing it with a '.'

names(menu) = make.names(names(menu), unique = TRUE)

### We want to look at the relationships between serving size 
### and various nutritional values, but serving size is currently 
### recorded in a very unfriendly manner, mixing ounces and grams 
### in a single column. 
### We'll convert it into a single numeric variable represented 
### by grams (for solid food - food.g), 
### and milliliters (for drinks - drinks.ml).

### drinks - select only fields that contain "fl oz" string 
### and sperately 'carton' string

menu %>% select(Serving.Size)

drinks.oz = menu %>%
             filter(str_detect(Serving.Size, " fl oz.*"))

drinks.oz %>% View()
drinks.ml %>% View()

drinks.ml = menu %>%
             filter(str_detect(Serving.Size, 'carton'))

## drinks - keep the numbers and convert ounces to mililiters (1oz = 29.5735ml)
## round the values to zero decimal places 

drinks.oz$Serving.Size = 
  round(as.numeric(gsub(" fl oz.*", "", drinks.oz$Serving.Size))*29.5735, 0)

drinks.ml$Serving.Size = 
  round(as.numeric(gsub(".*\\((.*)\\ ml).*", "\\1", drinks.ml$Serving.Size)), 0)

## food - select only fields that contain "g" string
## keep the numbers and round the values to zero decimal places

### Your turn

food.g = menu %>% 
          filter(str_detect(Serving.Size, 'g'))

food.g$Serving.Size = 
  round(as.numeric(gsub(".*\\((.*)\\ g).*", "\\1", food.g$Serving.Size)), 0)

## combine all those data frames by rows into new data frame
## create new column with Type of Item as either 'drink' or 'food'

menu2 = rbind(drinks.oz, drinks.ml)
menu2

menu2$Type = rep("drinks.ml", nrow(menu2))

food.g$Type = rep("food.g", nrow(food.g))

menu2 = rbind(menu2, food.g)



### Similar to the 'read_csv()' function used for reading 
### CSV files into R, there is a 'write_csv()' function 
### that generates CSV files from data frames.

write_csv(menu2, "menu_cleaned.csv")


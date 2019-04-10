### Load packages

library(tidyverse)
library(ggrepel)
library(rvest)

### Define the url once.

URL = "http://www.ratemyprofessors.com/ShowRatings.jsp?tid=1921840"

### The html_nodes() function turns each HTML 
### tag into a row in an R dataframe.

my_html = read_html(URL)
my_html

### It is a table, to get it let's run

my_html %>% 
   html_table() 

my_html %>% 
   html_table(fill = TRUE) 

my_html %>% 
   html_table(fill = TRUE) %>% .[[1]]

### save it in to an object called, say "tbl"

tbl = my_html %>% html_table(fill = TRUE, header = TRUE) %>% .[[1]]

tbl

### Look at its class

tbl %>% class()

### Look at its dimension

tbl %>% dim()

### What's in column 1???

tbl[,1]

### tbl[,1] is a character vector

tbl[,1] %>% class()

tbl[,1] %>% length()

### So, if there is nothing in a character, the length will be 0

tbl[,1] %>% nchar()

### Delete the empty characters 

tbl1 = tbl[,1]

tbl1 = tbl1[nchar(tbl1) > 0]

### Now every row is a very long string, 
### we need to split each into columns 
### that contain the values we want

tbl1 = stringr::str_split(tbl1, "\r\n                ")

### Now tbl1 is a list

tbl1 %>% class()

tbl1 %>% length()

### each element is a vector of length 17

tbl1[[1]]

### Make a container 

temp1 = tbl1[[1]]

for (i in 2:length(tbl1)) {temp1 = rbind(temp1, tbl1[[i]])}

temp1

### temp1 becomes a matrix now

temp1 %>% class()

### convert it into tibble

temp1 = temp1 %>% 
         as_tibble() %>%
         select(5, 10, 16)

names(temp1) = c("rating", "quality", "difficulty")

temp1 = temp1 %>%
         mutate(rating = gsub(".*\\s", "", rating),
                quality = as.numeric(quality),
                difficulty = as.numeric(difficulty))

temp1

tbl2 = tbl[,2] 

tbl2 = tbl2[nchar(tbl2) > 0]

### Now split 

tbl2 = stringr::str_split(tbl2, "\r\n                ")

temp2 = tbl2[[1]]

for (i in 2:length(tbl2)) {temp2 = rbind(temp2, tbl2[[i]])}

temp2

temp2 = temp2 %>% 
         as_tibble() %>%
         select(1, 6)

names(temp2) = c("course", "grade")

temp2 = temp2 %>%
         mutate(grade = gsub("Grade Received: ", "", grade)) %>%
         mutate(grade = gsub("[^A-Za-z0-9,;._-]", "", grade))
               
temp = bind_cols(temp1, temp2)

#######################################
### wrap everything into a function ###
#######################################

ratemyprof20 = function(URL) {

}

instructor = ratemyprof20("http://www.ratemyprofessors.com/ShowRatings.jsp?tid=1921840")



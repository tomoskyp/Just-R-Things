                    ##--------------------------------------------------------------##
                    ##                         Learned so far                       ##
                    ##                                                              ##
                    ##                         Paul Tomosky                         ##
                    ##                                                              ##
                    ##--------------------------------------------------------------##

############################################################################################################################
# LIBRARIES ##################################################################################################################
############################################################################################################################

install.packages('tidyverse')
install.packages('ggthemes')
install.packages('ggrepel')
install.packages('janitor')
install.packages('skimr')
install.packages('rvest')                # Scrape data from websites
install.packages('stringr')
install.packages('pandoc')
install.packages('knitr')

library(tidyverse)
library(ggthemes)
library(ggrepel)
library(janitor)
library(skimr)
library(rvest)
library(stringr)

############################################################################################################################
# BASICS ##################################################################################################################
############################################################################################################################

?options
options()


############################################################################################################################
# FUNCTIONS ##################################################################################################################
############################################################################################################################

my_func = function(x, y) {x - y}
x = 3
y = 1
my_func(x, y)
x %>% my_func(y)    # Default, e.g. my_func(x, y)
x %>% my_func(., y) # Default, e.g. my_func(x, y)
y %>% my_func(x, .) # Pipe y to second argument, e.g. my_func(x, y)
x %>% my_func(y, .) # Pipe x to second argument, e.g. my_func(y, x)


linear.growth.rate = (N2017 - N2007) / 10
N2018.l = N2017 + linear.growth.rate


calculatePercentage = function(x, y, d) {
  decimal = x / y  # Calculate decimal value
  percent = round(100 * decimal, d)  # Convert to % and round to d digits
  return(paste0(percent, "%"))
}
calculatePercentage(2.099999, 10, 4)


createPatientRecord = function(full.name, weight, height) {
  name.list = strsplit(full.name, split =" ")[[1]]
  first.name = name.list[1]
  last.name = name.list[2]
  weight.in.kg = weight / 2.2
  height.in.m = height * 0.0254
  bmi = weight.in.kg / (height.in.m ^ 2)
  list(first.name = first.name, 
       last.name = last.name, 
       weight = weight.in.kg, 
       height = height.in.m,
       bmi = bmi)
}
createPatientRecord('Paul Tomosky', 150, 72)
createPatientRecord('Calvin Hobbes', 95, 50)


Pea.corr = function(x, y) {
  # calculate the means
  Average.x = mean(x)
  Average.y = mean(y)
  # numerator & denominator
  numerator =  sum((x-Average.x) * (y - Average.y))
  demonimator = sqrt(sum((x-Average.x)^2) * sum((y - Average.y)^2))
  # compute the actual correlation
  r = numerator / demonimator
  return(r)
}
x = 1:10
y = 2:11
Pea.corr(x,y)


calculateLetterGrade = function(x){
  n_obs = length(x)
  letter.grade = character(n_obs)
  for (i in seq_len(n_obs)){
    letter.grade[i] =
      if(x[i] >=90){"A"
      } else if (x[i] >= 80) {"B"
      } else if (x[i] >= 70) {"C"
      } else {"F"
      }
  }
  return(letter.grade)
}
# test function for the follwing scores 
course.grades = c(92, 78, 87, 91, 62)
calculateLetterGrade(course.grades)

############################################################################################################################
# MATH ######################################################################################################################
############################################################################################################################

4 %>%  # Take the number four and, then
  sqrt() # find the square root

()                     #change the order of operations
log(), exp()           #logarithms and exponents (ex: log(10) = 2.302)
cos(), sin(), tan()    #trigonometry (ex: sin(pi/2))
sqrt()                 #square root
abs()                  #absolute value
floor(), ceiling()     #round down or round up
round()                #round to the nearest whole number (ex: round(2.3)

NULL =                 #null object
NA =                   #Not Available (missing value)
Inf =                  #infnity
NaN =                  #Not a Number (different from NA)


c(7, 42, 1, 25) %>%   # Combine four elements and, then
  log() %>%   # take the natural log and, then
  round(2) %>%   # round to the second decimal and, then
  diff()       # take the difference between consecutive elements


############################################################################################################################
# IF / ELSE ##################################################################################################################
############################################################################################################################

test_obs_value = 120
BP_Type = ""
if (test_obs_value < 120)
{
  BP_Type = 'Normal'
} else if (test_obs_value >= 120 & test_obs_value <= 129) {
  BP_Type = 'Elevated'
} else if (test_obs_value >= 130 & test_obs_value < 139) {
  BP_Type = 'Stage 1 / Hypertension'
} else {
  BP_Type = 'Stage 2 / Hypertension'
}


income = c(20, 210, 99, 387, 101) #COMPACT
income < 100
tax = ifelse(income < 100, income*0.20, 100*0.20 + (income-100)*0.30)


############################################################################################################################
# LOOPS ##################################################################################################################
############################################################################################################################


fruit = c('Apple', 'Orange', 'Passion fruit', 'Banana')
for (i in 1:length(fruit)){ 
  print(fruit[i])
}
for (i in fruit){ 
  print(i)
}


for (r in 1:nrow(mat)){  
  for (c in 1:ncol(mat)) {  
    print(paste("Row", r, "and column",c, "have value of", mat[r,c]))}
}


############################################################################################################################
# VECTORS ##################################################################################################################
############################################################################################################################


w = c(9.5, -3.14, 88.9999, 12.0, 411)
x = c(1L, 2L, 3L, 4L, 5L)
y = c(TRUE, FALSE, FALSE, TRUE, FALSE)
z = c("a", "b", "c", "d", "e")

x2 = c(0:9)
average = mean(x2)
AboveAverage = x2[x2 > average]
Output3 = list(average, AboveAverage)
Output3

# VECTOR FUNCTIONS ##################################################################################################################

length(vec)               # number of elements in vec
sum(vec)                  #sums up all the elements of vec
mean(vec)                 #mean of vec
median(vec)               #median of vec
min(vec), max(vec)        #the largest or smallest element of vec
range(vec)                #returns the smallest and largest values together
sd(vec), var(vec)         # the standard deviation and variance of vec
sort(vec)                 # returns the vec in sorted order
order(vec)                # returns the index that sorts the vector vec
unique(vec)               # lists the unique elements of vec
summary(vec)              # gives a five-number summary
any(vec), all(vec)        # useful on Boolean vectors
which.max(vec)            # The index of the maximum value (frst index if ties)
which.min(vec)            # The index of the minimum value (frst index if ties)
duplicated(vec)           # lists the duplicated elements of vec


# TWO VECTORS ##################################################################################################################


dim(A)                    # returns the number of rows and columns of A
nrow(A)                   #  returns the number of rows of A
ncol(A)                   #  returns the number of columns of A
A * B                     # Element-wise multiplication
A %*% B                   # Matrix multiplication
A %o% B                   # Outer product. AB0
crossprod(A,B)            # A'B
crossprod(A)              #  A'A
t(A)                      #  Transpose
diag(x)                   #  Creates diagonal matrix with elements of x inthe principal diagonal
diag(A)                   #  Returns a vector containing the elements of the principal diagonal
diag(k)                   #  If k is a scalar, this creates a k × k identity matrix. Go gure.
solve(A, b)               #  Returns vector x in the equation b = Ax (i.e., A ??? 1b)
solve(A)                  #  Inverse of A where A is a square matrix.
ginv                      #  Moore-Penrose Generalized Inverse of A. (requires loading the MASS package)
cbind(A,B,...)            # Combine matrices(vectors) horizontally.Returns a matrix.
rbind(A,B,...)            # Combine matrices(vectors) vertically. Returns a matrix.
rowMeans(A)               #  Returns vector of row means.
rowSums(A)                #  Returns vector of row sums.
colMeans(A)               #  Returns vector of column means.
colSums(A)                #  Returns vector of column sums.
R = chol(A)               #  Choleski factorization of A. Returns the upper triangular factor, such that R'R = A.
y = qr(A)                 # QR decomposition of A.
y = eigen(A)              #  y$val contains eigenvalues and y$vec containseigenvectors
y = svd(A)                #  Single value decomposition of A.


############################################################################################################################
# SEQUENCE ##################################################################################################################
############################################################################################################################


seq(5.5, 10.2, length = 10)
seq(10, 20, by = 2)
y = c(x, seq(10, 4, by = -2), rep(x, each = 2), TRUE, FALSE)


#############################################################################################################################
# LISTS #####################################################################################################################
############################################################################################################################


x = list("rope", 72, c(5, 7, 10), TRUE); 
class(x)
mylist = list(L1 = "R intro", L2 = "data science", L3 = 12345, 
              L4 = 1:5, L5 = TRUE, L6 = matrix(runif(10), 5, 2))
mylist[[2]]
mylist[1] # SUB-LIST


############################################################################################################################
# INDEXING ##################################################################################################################
############################################################################################################################


x = c(seq(1, 10, 2), NA, seq(5, 15, 2), NA, 100:95)
x
x[]
x[c(1,1,1,2,2,2)]
  #NOT !
x[-1]
x[-(1:5)]


############################################################################################################################
# CHECKING ##################################################################################################################
############################################################################################################################


which(is.na(x))
na.omit(x)
x[!is.na(x)]


############################################################################################################################
# MATRIX ##################################################################################################################
############################################################################################################################


m = matrix(1:12, nrow = 3, ncol = 4)
m1 = matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
cbind(m, 101:103) #rbind

  
# R AND C ##################################################################################################################

  m[1]
  m[1,]
  m[,1]
  
  
leslie = rbind(c(0,1,5),
               c(0.3,0,0),
               c(0,0.5,0))


A = matrix(seq(1, 6), nrow = 2, ncol = 3)
A
B = matrix(seq(1, 6), nrow = 3, ncol = 2)
B
C = A %*% B
C


leftSide = matrix(c(3, -2, -1,
                    2, -2, 4,
                    -1, 0.5, -1), 3, 3, byrow=TRUE) 
colnames(leftSide) = c('X','Y','z' )              
rightSide = c(1, -2, -0)                  
solve(leftSide, rightSide)


############################################################################################################################
# DATA #####################################################################################################################
############################################################################################################################

menu = read_csv(file.choose())
menu

menu %>% class()
menu %>% glimpse()
menu %>% print(n = 20)
menu %>% summary()


menu %>%
  select(Category) %>%
  table() %>%
  addmargins()


menu %>%
  select(Category) %>%
  table() %>%
  prop.table()%>%
  "*" (100) 


menu %>% count(Category, sort = TRUE) %>% add_tally()


foodtable = menu %>% 
  count(Category, sort = TRUE) %>%
  rename(freq = n) %>%
  mutate(prop = freq/sum(freq),
         perc = freq/sum(freq) *100)


menu %>%
  select(Saturated.Fat, Carbohydrates, Protein) %>%
  summarize_all(funs(min, max, median, mean, sd)) %>% View()

fatstats = menu %>%
  select(Saturated.Fat) %>%
  summarize(n = n(), Mean = mean(Saturated.Fat), 
            Med = median(Saturated.Fat), SD = sd(Saturated.Fat))

# data manipulation ##################################################################################################################


names(menu)

new_names = gsub(pattern     = "*....Daily.Value.", 
                 replacement = ".PDV", 
                 names(menu))
names(menu) = new_names

names(menu) = make.names(names(menu), unique = TRUE)         # remove spaces


menu %>% select(Serving.Size)                                # remove strings ---> to get int
drinks.oz = menu %>%
  filter(str_detect(Serving.Size, " fl oz.*"))

drinks.oz %>% View()
drinks.ml %>% View()
drinks.ml = menu %>%
  filter(str_detect(Serving.Size, 'carton'))

drinks.oz$Serving.Size =                                      # Converting units
  round(as.numeric(gsub(" fl oz.*", "", drinks.oz$Serving.Size))*29.5735, 0)
drinks.ml$Serving.Size = 
  round(as.numeric(gsub(".*\\((.*)\\ ml).*", "\\1", drinks.ml$Serving.Size)), 0)

menu2 = rbind(drinks.oz, drinks.ml)                          # recombine data
menu2$Type = rep("drinks.ml", nrow(menu2))
food.g$Type = rep("food.g", nrow(food.g))
menu2 = rbind(menu2, food.g)

write_csv(menu2, "menu_cleaned.csv")                         # write to file

#Table Comparison ##################################################################################################################


menu %>%
  select(Category, Type) %>%
  table()

# adding totals 
menu %>%
  select(Category, Type) %>%
  table() %>%
  addmargins()

# To get row percentage
menu %>%
  select(Category, Type) %>%
  table() %>%
  prop.table(1)%>%
  "*" (100) %>%
  round(2)

# For column percentage
menu %>%
  select(Category, Type) %>%
  table() %>%
  prop.table(2)%>%
  "*" (100) %>%
  round(2)

# Use the janitor package
install.packages('janitor')
library(janitor)

menu %>%
  tabyl(Category, Type) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

# Now in **tidyverse**
menu %>% count(Category, Type)

# visualizing 
menu %>% 
  ggplot(aes(x = Category, fill = Type))+ 
  geom_bar()+
  labs(title    = "Stacked Bar Chart: Distribution of Food Categories over Food Type",
       x = "Categories",
       y = "Count",
       caption = "Graph by lkxt@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))

menu %>% 
  ggplot(aes(x = Category, fill = Type))+ 
  geom_bar()+
  labs(title    = "Stacked Bar Chart: Distribution of Food Categories over Food Type",
       x = "Categories",
       y = "Count",
       caption = "Graph by lkxt@iup.edu")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 1, size = 12))



#Bar chart ##################################################################################################################

ggplot(data = menu, aes(x = Category)) +
  geom_bar() 

foodtable %>% 
  ggplot(aes(x = reorder(Category, freq), y = freq, fill = Category)) + # reorder the bars by count from lowest to highest
  geom_bar(stat = "identity", color = "white")+                         # Change the bar color
  scale_y_continuous(breaks = seq(0, 100, 5))+                          # Adjust the scale of y axis
  guides(fill = FALSE)+                                                 # don't want the legend
  # geom_text(aes(label = freq)                                         # lable numbers on the bars --- lable freq
  geom_text(aes(label = paste0(round(perc, 1), "%")),                   # lable numbers on the bars --- lable percentage
            vjust = 0,                                                  #
            colour = "black",                                           #
            size = 5)+                                                  #
  labs(title    = "Distribution of Food Categories on Mc Menu",         # Change the lables --- contents
       subtitle = "9 types of food and drink",                          #
       x = "Categories",                                                #
       y = "Count",                                                     #
       caption = "Graph by ycao@iup.edu")+                              #
  theme(axis.text.x = element_text(size = 12),                          # Change the lables --- font size and positions
        axis.text.y = element_text(size = 12),                          #
        axis.title.x = element_text(size = 15),                         #
        axis.title.y = element_text(size = 15),                         #
        plot.title = element_text(hjust = 0, size = 16),                #
        plot.subtitle = element_text(hjust = 0, size = 12),             #
        plot.caption = element_text(hjust = 1, size = 12))+             # 
  coord_flip()                                                          # Make horizontal bar plot


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

#Histogram ##################################################################################################################

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


#density histogram ##################################################################################################################

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

#box plot ##################################################################################################################


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


#scatter plot  ##################################################################################################################


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
  ggplot(aes(x = Saturated.Fat, y = Carbohydrates, color = Category))+ 
  geom_point(aes(size = Protein))

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


menu %>% 
  select(Category, Item, Saturated.Fat, Carbohydrates, Protein) %>%
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
                     values = colorRampPalette(c("#ffc300", "#dd1324"))(9))+
  ggtitle('All menu items')



###########################################################################################################################
# STRINGS ##################################################################################################################
############################################################################################################################

double_quote = "Hello World!"
single_quote = 'Hello World!'
complex_string = "It's happening!"
escape_string = 'It\'s happening! \a'
white_space = " "
empty_string = ""
paste("Hello World to you", your_name, "!", sep = "_")
text = paste("Tree names include \"oak.\"\nOak weighs ", oak, " lbs/ft^3.\n", sep = "")
cat(text)

# SEARCH ##################################################################################################################

x = c("did you lie to me?",
      "all lies!",
      "are you lying?",
      "lying on the couch")
str_detect(x, pattern = "lie")
str_detect(x, pattern = "lie|you")

# REPLACE ##################################################################################################################

str_replace(x, pattern = "[Cc]ake",
            replacement = "Pizza")




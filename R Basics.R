
##################################################################
### Basic operations and assignment                            ###
##################################################################

### In 2017 around 7.5 billion live on planet earth. 
### Ten years earlier the number was 6.6 billion. 
### We feed this information into R by assigning it 
### to an object via the assignment operator "=" or "<-".

N2017 = 7.5e9
N2007 <- 6.6e9

N2017

N2007

### Now that we know the world population at two points 
### in time we can calculate the average absolute annual
### growth rate. 

linear.growth.rate = (N2017 - N2007) / 10

### If we want to model linear growth, our projections 
### for the population numbers in 2018 and 2019 are

### 2018 population 

N2018.l = N2017 + linear.growth.rate

### 2019 population  

N2019.l = N2017 + 2*linear.growth.rate
	##This is because we are useing 2017 instead of 2018

### Geometric growth ###

geo.growth.rate = ((N2017/N2007)^(1/10))-1

### 2018 population 

N2018.g = N2017*(1+geo.growth.rate)^1

### 2019 population  

N2019.g = N2017*(1+geo.growth.rate)^2

### Exponential growth ###

exp.growth.rate = log(N2017/N2007)/10

### 2018 population 

N2018.e = N2017*exp(exp.growth.rate)

### 2019 population  

N2019.e = N2017*exp(exp.growth.rate *2)


##################################################################
###                        Vectors                             ###
##################################################################




### `c()` function ###
### Concatenation/combine expressions to 
### form a larger collection


### Vector of numeric elements

w = c(9.5, -3.14, 88.9999, 12.0, 411)

### Vector of integer elements

x = c(1L, 2L, 3L, 4L, 5L)

### Vector of logical elements

y = c(TRUE, FALSE, FALSE, TRUE, FALSE)

### Vector of character elements

z = c("a", "b", "c", "d", "e")

### How do we know that?


(w)
### Other Commands
### length(w)
### is.vector(w)
### is.matrix(w)
### is.logical(w)


### Bigger

wx = c(w, x)

### The colon, :, operator ###
### a:b --- creates integer sequence from $a$ to $b$

1:10

-5:5

10:1 # blast-off; it recognizes direction!

### Colons take precedence over arithmetic:

2+1:5

(2+1):5

### `seq()` function ###
### sequence from a to b of length n
### or by increments of size d
### Length is an equal amount between the range

seq(5.5, 10.2, length = 10)

seq(10, 20, by = 2)

seq(100, 0, by = -25) 

### `rep()` function ###
### replicate a vector n times 
### or each element n times 

x = 1:5

rep(x, times = 3)

rep(x, each = 3)

### Your turn

# 1. Without running it, can you determine what `y` is: 

x = 1:4
y = c(x, seq(10, 4, by = -2), rep(x, each = 2), TRUE, FALSE)

# 2. Without running it, can you determine what `z` is: 

n = 5
z = 1:n-1

###### Subsetting and Indexing ######

x = c(seq(1, 10, 2), NA, seq(5, 15, 2), NA, 100:95)

x
x[]

### take the 11th element of x

x[11]

### take 2nd, 5th and 11th elements of x

x[c(2, 5, 11)]

### take all elements of x between the 5th and 9th positons

x[c(5:9)]

### take the 1st and 2nd elements each 3 times

x[c(1,1,1,2,2,2)]

### By exclusion: if you index with with "negative numbers", 
### it returns everything except those indices.

x[-1]

x[-(1:5)]

### By logical vector: You can index with a logical vector. 
### It will return every index that is "TRUE".

### take all the elements of x which are bigger than 50

x[x > 50]

x > 50

### Other two examples with different relational operators:

x[x == 15]

x[x != 5]

### Note that R extracts also NA elements from the vector 
### since the logical operator is not able
### to discriminate between TRUE and FALSE.

### is.na() can be used to check for missing cases

is.na(x)

### How do you find the number of NAs in a vector?

sum(is.na(x))

### How do you find the index(position) of NAs in a vector?

which(is.na(x))

### How to exclude the NA values 

x[!is.na(x)]

### Or

na.omit(x)

### Assignment: elements can be assigned 
### different values with indexing:


[1] = 999

### Your turn: Can you replace the NAs in x with 0s

x[is.na(x)] = 0

###### Vectorization ######

### Example: binary arithmetic operator ###

### In the following we present a way of computing the
### Crude Death Rates (CDR) for different populations. 
### In general, for a single year, CDR is defined as

### CDR = D[t, t+1] / PY[t, t+1] ###

### where D[t, t+1] are the number of deaths between 
### the start of year t and the start of year t + 1 
### (practically, Jan. 1 and Dec. 31 of the same year), 
### and PY[t, t+1] denotes the person-years lived in the same period,
### which one might approximate as follows:

### PY[t, t+1] = [N(t+1)-N(t)]/[log(N(t+1)/N(t))] ###

### with N(t+1) and N(t) are the number of persons 
### alive on Jan. 1 and Dec. 31, respectively. 

### Here we compute the CDR for four female populations in 2008. 
### First we create three vectors with
### D[t, t+1], N(t) and N(t+1) where t and t+1 are years 
### 2008 and 2009 (both Jan. 1), for Sweden, USA,
### France and Japan, respectively:

D01 = c(47389, 1245787, 260434, 533696)
N0 = c(4618852, 153833535, 32048040, 64563312)
N1 = c(4652510, 155243324, 32217830, 64512583)

### Then we compute the person-years:

PY01 = (N1-N0)/(log(N1/N0))

### CDR (per thousand) can be calculated for all three populations:

CDR = D01 / PY01
CDR1000 = CDR * 1000

### Example: Comparison ###

42 == 42 # Is 42 equal to 42? Yes or No?

42 != 32 # Is 42 not equal to 32? Yes or No?

12 < 38 # Is 12 less than 38? Yes or No?

12 > 38 # Is 12 greater than 38? Yes or No?

38 > 38 # Is 38 greater than 38? Yes or No?

38 >= 38 # Is 38 greater than or equal to 38? Yes or No?

c(3, 2, 1, -5) > 2 # Are values greater than 2? Yes or No?

2 %in% c(3, 2, 1, -5) # Does 2 belong to any values? Yes or No?

2 %in% c(3, 1, -5) # Does 2 belong to any values? Yes or No?

c(3, 1, -5) %in% 2 # Are any values equal to 2? Yes or No?

###### Character and String ######

### constructing strings ###

double_quote = "Hello World!"

single_quote = 'Hello World!'

complex_string = "It's happening!"

escape_string = 'It\'s happening! \a'

white_space = " "

empty_string = ""

### String Operators ###

### Total number of elements

length("Paul")

### How many letters per element?

nchar("Paul")

### String Vector

ex_string = c("Paul", "rocks", "R")

length(ex_string)

nchar(ex_string)

### Concatenating Strings ###

your_name = "Mr. Meatballs"

### paste(..., sep = " ") 
### makes a string from its arguments, separated by sep. e.g.

paste("Hello World to you", your_name, "!")

paste("Hello World to you", your_name, "!", sep = "_")

paste("Hello World to you ", your_name, "!", sep = "")
# paste0("Hello World to you", your_name, "!")

### cat(..., sep = " ") 
### pastes and writes to console, interpreting escape sequences. e.g.

oak = 70

cat("oak=", oak, "\n", sep = "") 

text = paste("Tree names include \"oak.\"\nOak weighs ", oak, " lbs/ft^3.\n", sep = "")
cat(text)

### Vectorized Concatenation

subject_ids = seq_len(5)

paste0("S", subject_ids)

paste0("S", subject_ids, collapse = "")

###### Using Regex ######

### Using stringr from the tidyverse package

library(tidyverse)

### Searching for Patterns ###

### Sample String Data

x = c("did you lie to me?",
      "all lies!",
      "are you lying?",
      "lying on the couch")

str_detect(x, pattern = "lie")

str_detect(x, pattern = "you")

### Searching for Multiple Patterns

str_detect(x, pattern = "lie|you")
str_detect(x, pattern = "lie&you")
str_detect(x, pattern = "you lie")

### Replacing Patterns ###

### Sample String Data

x = c("lower case values",
      "UPPER CASE VALUES",
      "MiXtUrE oF vAlUeS")

### Lower case values for a b c

str_replace(x, pattern = "[abc]", replacement = "!")

### Lower case values for a b c

str_replace_all(x, pattern = "[abc]", replacement = "!")

### Replace UPPER case values for A B C

str_replace_all(x, pattern = "[ABC]", replacement = "!")

### Replace all lower case values

str_replace_all(x, pattern = "[a-z]", replacement = "!")

### Sample String Data

x = c("I dislike cake. Any cake really.",
      "Cake is okay...",
      "I love cake... Cake... Cake...",
      "I prefer to have pie over cake",
      "Mmmm... Pie.")
x

### Replacing first instance of cake per string

str_replace(x, pattern = "[Cc]ake",
               replacement = "Pizza")

### Replacing ALL instances of cake

str_replace_all(x, pattern = "[Cc]ake",
                   replacement = "Pizza")


##################################################################
###                           Matrix                           ###
##################################################################

###### Creating matrices ######

### Create a matrix from vector data with 
### matrix(data = , nrow =, ncol = , byrow = FALSE, dimnames =  NULL), 
### where `byrow` tells whether to fill the matrix 
### from data by row (or by column, the default), and 
### dimnames is NULL or a list of two vectors 
### containing row and column names. e.g.

m = matrix(1:12, nrow = 3, ncol = 4)
m

m1 = matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
m1

### Two other ways to create matrices are by 
### combining columns with `cbind(...)` or 
### rows with `rbind(...)`, getting data 
### from vector, matrix, or data frame arguments in ...:

cbind(m, 101:103)

rbind(m, 101:104)

###### Matrix indexing ######

### A matrix is indexed by `m[i,j]` returns 
### the $i$th rows and $j$th columns

### In the below example we are accessing 
### 1st, 2nd, 6th element of matrix m.

length(m)

m[1]
m[2]
m[6]

###  accessing 2nd row 3rd column element



### accessing all elements of rows 
### of the matrix m shown below. 

m[1,]
m[2,]

m1[1,]

### accessing all elements of each column

m[,1]
m[,2]

### Your turn Example: Population projections using the Leslie-Matrix



### first construct a matrix called "leslie" as below

### [0 1 5]
### [0.3 0 0]
### [0 0.5, 0]

leslie = rbind(c(0,1,5),
               c(0.3,0,0),
               c(0,0.5,0))

leslie

### make a vector, N0, for the initial population distribution
### which contains 100, 0, 0 

N0 = c(100, 0, 0)

### calculate the population after single time step, N1,
### where N1 = leslie*N0

N1 = leslie %*% N0
N1

### population after 2 time steps, N2

N2 = leslie %*% N1
N2

##################################################################
###                           Lists                            ###
##################################################################

###### Creating lists ######

### We create list objects using list() function
### or coerce other objects using as.list()

x = list("rope", 72, c(5, 7, 10), TRUE); 

x

class(x)
str(x)
length(x)

### List components can be named via `name=value` pairs in list()

mylist = list(L1 = "R intro", L2 = "data science", L3 = 12345, 
              L4 = 1:5, L5 = TRUE, L6 = matrix(runif(10), 5, 2))
mylist

mylist $L2

mylist[[2]]

### coerce

x = 1:10

as.matrix(x)
as.character(x)

x = as.list(x)

unlist(x)

###### list indexing ######

### indexing with single brackets returns a sub-list

mylist[1]

### indexing with double brackets or 
### $ returns one component, dropping names

mylist[[1]]

mylist$L1


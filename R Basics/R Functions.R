##################################################################
###                 Conditional Execution                      ###
##################################################################

#############################
# if (CONDITION) {
#    EXPRESSION
# }
############################

### code before `if`

### case 1

quantity = 25

if (quantity > 30) { 
   print('You sold a lot!')
}

### case 2

quantity = 35

if (quantity > 30) { 
   print('You sold a lot!')
}

###########################
## if (CONDITION) {
##    TRUE.EXPRESSION
##  } else {
##     FALSE.EXPRESSION
##    }
###########################

quantity =  25

if (quantity > 30) {
    print('You sold a lot!')
} else {
    print('Not enough for today')  
}

#################################
## if (CONDITION_1) {
##     EXPRESSION_1
## } else if (CONDITION_2) { 
##      EXPRESSION_2 # ...
##   } 
##   ... 
##
##   } else { 
##     DEFAULT_EXPRESSION
##     }
#################################

quantity = (10:50)

if (quantity < 20) {
      print('Not enough for today')
} else if (quantity > 20  & quantity <= 30) {
     print('Average day')
} else {
      print('What a great day!')
}

###### VAT Example ######

    product = "beverage"
    price   = 20

### First get category 

    category = ""
    
    if (product %in% c("book", "magazine", "newspaper")) {
      category = "A" 
    } else if (product %in% c("vegetable", "meat", "beverage")){
      category = "B" 
    } else {
      category = "C"
    }
    
    category

### calculate total price 

    if (category == 'A'){
      cat('A vat rate of 8% is applied.','The total price is', price *1.08)  
    } else if (category =='B'){
      cat('A vat rate of 10% is applied.','The total price is', price *1.10)  
    } else {
      cat('A vat rate of 20% is applied.','The total price is', price *1.20)  
    }

###### Blood Pressure Example ######

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
      
    BP_Type

### None of the three constructs above works if 
### its CONDITION has length greater than one.
### However, x = ifelse(test, yes, no) sets 
### x to be a vector with the same length 
### as test filled with elements from yes or no 
### depending on the logical values in test.

### Example: Tax Rates. ###

### Suppose a simple graduated tax rate of 20% 
### on income less than $100K/year and 30% for 
### the portion of incomes that exceeds $100K

income = c(20, 210, 99, 387, 101) # income in thousands

income < 100

tax = ifelse(income < 100, income*0.20, 100*0.20 + (income-100)*0.30)

tax

tax/income # overall tax rate

### Now create a vector called class which has a value "poor" 
### if income < 100, and "rich" otherwise.

class = ifelse(income < 100, 'poor', 'rich')

class

##################################################################
###                 Repetitive Execution                       ###
##################################################################

##################################
# for (VARIABLE in SEQUENCE) {
#       EXPRESSION
#   }
##################################

for (i in 1:10){
print(i)
}

for (i in 1:10){
print("Hello World")
}

### Create fruit vector

fruit = c('Apple', 'Orange', 'Passion fruit', 'Banana')

for (i in 1:length(fruit)){ 
 print(fruit[i])
}

### or 

for (i in fruit){ 
 print(i)
}

### Your turn: BP Type revisit
### create a character `bp_type` vector 
### for the follwing systolic values with a for loop

systolic = c(110, 141, 125, 168, 115, 122, 135)

### Count number of observations

n_obs = 0

for (i in systolic){
  n_obs = n_obs + 1
}
print(n_obs)

  #OR#

n_obs = length(systolic)
n_obs  

### Create an empty character vector 

classify_vector = character(n_obs) 
classify_vector

### Iterate through a vector containing positional indexes

##not working correctly

for (i in seq_len(n_obs)) {

  classify_vector[i] =
    if(systolic[i] < 120){
      classify_vector = 'Normal'
    } else if (systolic[i] >= 120 & systolic[i] <= 129){
      classify_vector = 'Elevated'
    } else if (systolic[i] >= 130 & systolic[i] < 139) {
      classify_vector = 'Stage 1 / Hypertension'
    } else {
      classify_vector = 'Stage 2 / Hypertension'
    }
      
}

classify_vector



### For loops may be nested
### example: For Loop over a matrix
### A matrix has 2-dimension, rows and columns. 
### To iterate over a matrix, we have to define 
### two for loop, namely one for the rows and 
### another for the column.

### Create a matrix

mat = matrix(data = seq(10, 21, by = 1), nrow = 6, ncol =2)
mat

### Create the loop with r and c 
### to iterate over the matrix

for (r in 1:nrow(mat)){  
    for (c in 1:ncol(mat)) {  
         print(paste("Row", r, "and column",c, "have value of", mat[r,c]))}
}

### Your turn: multiplying two matrices

A = matrix(seq(1, 6), nrow = 2, ncol = 3)
A
B = matrix(seq(1, 6), nrow = 3, ncol = 2)
B
C = A %*% B
C

### now realize matrix C by using a nested iteration

C = matrix(0, nrow = nrow(A), ncol = ncol(B))
C

if (ncol(A) == nrow(B)) {
 for (i in seq_len(nrow(A))){
   for (j in seq_len(ncol(B))){
     for (k in seq_len(ncol(A))){
       C[i,j] = C[i,j] + A[i,k] * B[k,j]
     }
   }
 }
} else {
stop("matrices 'A' and 'B' dimensions are improper")
}

C

##################################################################
###                          Functions                         ###
##################################################################

### Let's look at one of R's base function

sample

?sample

set.seed(411) #Get a consistint random number generated

data = 1:10
sample(size = 5, x = data, r = T)
sample(size = 5, x = data, r = F)

###### Write function in R ######

### Example 1: Here's a function that returns a 
### % given a numerator, denominator, and 
### desired number of decimal values

calculatePercentage = function(x, y, d) {
  decimal = x / y  # Calculate decimal value
  percent = round(100 * decimal, d)  # Convert to % and round to d digits
 return(paste0(percent, "%"))
}

calculatePercentage(2.099999, 10, 4)
calculatePercentage(1, 3, 4)
calculatePercentage(23.099999, 10, 4)

### Example 2: Here's a function that takes a person's 
### full name (FirstName LastName), weight in lb and 
### height in inches and converts it into a list with 
### the person's first name, person's last name, 
### weight in kg, height in m, and BMI.

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

### Practice 1: "Handmade" Pearson correlation, 
### normally denoted by "r"
  
x = 1:10
y = 2:11  
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

Pea.corr(x,y)

### Test your function with 

x = 1:10
y = 2:11

or

x = rnom(10)
y = rnom(20) #broken idk why: rnom should generate random numbers somehow


### Practice 2: calculate letter grade

calculateLetterGrade = function(x){
  
  n_obs = length(x)
  letter.grade = character(n_obs)
  
  for (i in seq_len(n_obs)){
    
    letter.grade[i] =
      
      if(x[i] >=90){
        "A"
      } else if (x[i] >= 80) {
        "B"
      } else if (x[i] >= 70) {
        "C"
      } else {
        "F"
      }
  }
  return(letter.grade)
}
### test your function for the follwing scores 

course.grades = c(92, 78, 87, 91, 62)

calculateLetterGrade(course.grades)





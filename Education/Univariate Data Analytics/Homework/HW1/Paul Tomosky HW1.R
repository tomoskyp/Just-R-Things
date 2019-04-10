#########################################
# This code was created by Paul Tomosky #
# MATH 411                              #
# Homework 1                    9/17/18 #
#########################################

### Problem 1: Vector

  i = c(1:100)
  total = sum(i^2 + 4/i)
  total
  
  
  
### Problem 2: Vector & function

   naCount = function(x){
    xCountNA = (sum(is.na(x)))
    xListNA =  (which(is.na(x)))
    output1 = list(xCountNA, xListNA)
    return (output1)
  }
  naCount(c(1, NA, NA, 2, 3, NA, 4, 5))
  
  
  
### Problem 3: Vector & function
  subelements = function(x2){
    average = mean(x2)
    AboveAverage = x2[x2 > average]
    Output2 = list(average, AboveAverage)
    return (Output2)
  }
  subelements(c(0:9))
  
  
### Problem 4: Matrix
 
  leftSide = matrix(c(3, -2, -1,
                      2, -2, 4,
                     -1, 0.5, -1), 3, 3, byrow=TRUE) 
  colnames(leftSide) = c('X','Y','z' )              
  rightSide = c(1, -2, -0)                  
  solve(leftSide, rightSide)
  

### Problem 5: Matrix & function

  mat = function(n){
    M = matrix (0, n, n)
      diag(M) = 2
      diag(M)1:(n-1), (2:n) = 1
      diag(M)2:(n=1), 
      
      #### lost it here 
    #diag function
    
    return ()
  }
  
  mat = matrix(nrow = 5, ncol =5)
  mat 
  
  output5
  
  # Intended output:
  
  # 2 1 0 0 0 
  # 1 2 1 0 0
  # 0 1 2 1 0
  # 0 0 1 2 1 
  # 0 0 0 1 2 
  
### Problem 6: Character & sampling & function

  dominion = function(x){
    
    set.seed(2018411)
    cardNames = c('Adventurer', 'Bureaucrat','Cellar','Chancellor','Chapel','Council Room','Feast','Festival',
                     'Labratory','library','Market', 'Militia','Mine','Moat','Money Lender','Remodel', 'Smithy',
                     'Spy', 'Thief','Throne Room','Village','Witch','Woodcutter', 'Workshop', 'Gardens')
   
    
    cardSelection = sample(cardNames, x)
    return(cardSelection)
  }
  dominion(10)
  
  
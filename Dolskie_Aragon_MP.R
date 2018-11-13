#############################################
###                                          ### 
###    Computing for Analytics - MAB2111       ###
###                                             ###
###    Dolskie S. Aragon                        ###
###                                             ###
###    Machine Problem (aka R Assingment 2)    ###
###                                           ###
##############################################



# No. 1 Define an R function removes NA values from a vector
  
aliskana <- function(a) {
  new_a <- c()
  
  for(i in 1:length(a)) {
    if(!is.na(a[i])) {new_a <- c(new_a,a[i])
    }
  }
  
  return(new_a)
}
testdata <- c(8,5,7,2,8,34,NA, 5, 8)

aliskana(testdata)


# No. 2 Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1
  # Assumption is based on "given an integer argument". 

factorial <- function(number) {
  num = 1
  for(i in 1:number) {
  num <- num * i
}
  return(num)
  }

factorial(1)
factorial(8)

# No. 4 Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. 
# It should accept both numeric or character vectors.

sort_data <- function(v) {
  
  for (i in 1:(length(v) - 1)) {
    for (j in (i + 1): length(v)) {
      if (v[i] < v[j]) {
        temp <- v[i]
        v[i] <- v[j]
        v[j] <- temp
      }
    } 
  } 
  return(v)
}

testData <- c("bb", "cc", "rr", "aa")
sort_data(testData)
testData <- c(5, 10, 15, 2, 29)
sort_data(testData)



# No. 6 Create a function to compute for your net pay at work.

## The Gross salary is based on the my actual salary as project based employee in reference to what is being asked "Your net pay at work".

compute_netpay <- function(n) {
  wtax = 0
  
  # Computation of withholding tax is based on the new computation published by Department of Finance
  
  if(n >= 33333 & n <= 66666) {
    wtax = 2500
  }
  # Values below are based on actual computation issued by DOF given the 45,000 salary. Values of Contributions are fixed according to the 
  #gross mothly pay (DOF Tax calculator).
  
  sss = 581.30
  philhealth = 412.50
  pagibig = 100.00
  
  total_deduction = wtax + sss + philhealth + pagibig 
  
  return (n - total_deduction)
}
basicpay = 45000
compute_netpay(basicpay)


# No. 7 Create a function that accepts a vector and and integer n and returns nth highest number

nth_highest <- function(v, n) {
  new_v <- sort(v, decreasing = TRUE)
  return(new_v[n])
}

testData <- c(25, 1, 24, 36, 22, 5)

n = 3
nth_highest(testData, 3)
n = 2
nth_highest(testData, 2)

## This is just a practice. I opted to include this code for better appreciation of my efforts since I have zero background in programming. 

# No. 8 Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.

compound <- function(p, r, n, t) {
  
  amnt = p * (1 + r / n)^(n*t)
  
  return(amnt)
}
 compound(20000, 0.06, 1, 10)
  

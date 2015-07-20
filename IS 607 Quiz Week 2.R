
# Week 2 Quiz solution

# 1. Create a vector that contains 20 numbers. (You may choose whatever numbers you like, but make sure
# there are some duplicates.)

numberVector <- c(1:10,5:14)
numberVector

# 2. Use R to convert the vector from question 1 into a character vector.
characterVector <- as.character(numberVector)
characterVector

# 3. Use R to convert the vector from question 1 into a vector of factors.
factorVector <- as.factor(numberVector)
factorVector

# 4. Use R to show how many levels the vector in the previous question has.
nlevels(factorVector)

# 5. Use R to create a vector that takes the vector from question 1 and performs on it the formula
# 3x^2 - 4x + 1

formulaVector <- 3 * (numberVector ^ 2) - (4 * numberVector) + 1
formulaVector

# 6. Implement ordinary least-squares regression in matrix form

X <- c(1,1,1,1,1,1,1,1,5,4,6,2,3,2,7,8,8,9,4,7,4,9,6,4)
X <- matrix(X, ncol = 3)
X

Y <- c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1)
Y <- matrix(Y, ncol = 1)
Y

beta <- solve(t(X) %*% X) %*% t(X) %*% Y
beta

# 7. Create a named list. That is, create a list with several elements that are each able to be referenced by name.

namedList <- list(scientists = c("Einstein", "Newton", "Feynman"))

namedList$scientists[3]

# 8. Create a data frame with four columns - one each character, factor (with three levels), numeric, and date.
# Your data frame should have at least 10 observations (rows).

fauna <- as.character(c("Deer","Lion", "Vulture",
             "Shark", "Goldfish",
             "Starfish", "Trout", "Wolf",
             "Pigeon", "Elephant"))
species <- c("Mammal", "Mammal", "Bird",
             "Fish", "Fish", "Fish", "Fish", "Mammal", "Bird", "Mammal")
zoosight <- as.numeric(c(3, 4, 2, 2, 10, 6, 4, 8, 5, 7))
zoodate <- as.Date(c("2005-1-1", "2005-10-1", "2006-3-10", "2006-4-8", "2007-12-13",
          "2007-11-24", "2008-6-30", "2009-1-18", "2012-7-8", "2014-9-23"))

df <- data.frame(fauna, species, zoosight, zoodate)

df$fauna <- as.character(df$fauna)
df

# 9. Illustrate how to add a row with a value for the factor column that isn't already in the list of levels. (Note:
# You do not need to accomplish this with a single line of code.)

# Add "Gila Monster", "Lizard", 1, "2015-3-27"

dfrow <- data.frame(fauna=as.character(c("Gila Monster")), species="Lizard", 
                  zoosight=1, zoodate=as.Date(c("2015-3-27")))

df <- rbind(df, dfrow)
df

# 10. Show the code that would read in a CSV file called temperatures.csv from the current working directory.

df <- read.csv(file = "temperatures.csv", header = TRUE)


# 11. Show the code that would read in a TSV file called measurements.txt from a directory other than the
# working directory on your local machine.

df <- read.csv(file = "C:/IS607/clickstream.txt", header = TRUE)

#12.  Show the code that will read in a delimited file with a pipe separator (the "|" symbol) from a website
# location. (You may make up an appropriate URL.)

url <- "http://portal.tamu.edu/aks5738/accounts.csv"
df <- read.csv(file = url, sep = "|")

# 13. Write a loop that calculates 12-factorial.
i <- 1
factorial <- 1
ubound = 12

while (i <= ubound)
{
  factorial <- factorial * i
  i <- i + 1
}
factorial

#14. Use a loop to calculate the final balance, rounded to the nearest cent, in an account that earns 3.24%
# interest compounded monthly after six years if the original balance is $1,500.

principal <- 1500
monthly_interest <- 0.0324/12
total_compound_periods <- 72 

for (i in 1:72)
{
  principal <- principal + (principal * monthly_interest)
}
print (principal)
sprintf("%.2f", principal)

# 15. Create a numeric vector of length 20 and then write code to calculate the sum of every third element of the
# vector you have created.

numvector <- c(1:20)
sum(numvector[numvector %% 3 == 0])

# 16. Use a for loop to calculate summation of 2 ^ i for i ranging from 1 to 10 

sum <- 0
for (i in 1:10)
{
  sum <- sum + 2 ^ i
}
print (sum)

# 17. Use a while loop to accomplish the same task as in the previous exercise.

sum <- 0
i <- 1
while (i <= 10)
{
  sum <- sum + 2 ^ i
  i <- i + 1
}
print (sum)

# 18. Solve the problem from the previous two exercises without using a loop.

print(sum(2^c(1:10)))

# 19. Show how to create a numeric vector that contains the sequence from 20 to 50 by 5.

seq(20,50,5)

# 20. Show how to create a character vector of length 10 with the same word, "example", ten times.

characterVector <- rep("example", 10)
characterVector

# 21. Show how to take a trio of input numbers a, b, and c and implement the quadratic equation.

# Solve ax^2 + bx + c = 0 for real x

roots <- function (a, b, c)
{
  x1 <- (-1 * b + (( b ^ 2 - (4 * a * c))^.5)) / (2 * a)
  x2 <- (-1 * b - (( b ^ 2 - (4 * a * c))^.5)) / (2 * a)
  return(c(x1,x2))
}

roots(3,-8,5)

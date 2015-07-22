# Week 2 Assignment solution

# 1. Suppose that you have five customers - James, Mary, Steve, Alex, and Patricia - in line at a store. Use R
# operations to perform the following tasks in sequence.

# a. Assign the five individuals to a vector called queue.

queue <- c("James","Mary","Steve","Alex","Patricia")
queue

# b. Update the queue for the arrival of a new patron named Harold.

queue <- c(queue, "Harold")
queue

# c. Update the queue to reflect the fact that James has finished checking out.

queue <- queue[-1]
queue

# d. Update the queue to reflect the fact that Patricia has talked her way in front of Steve with just one item

queue <- queue[c(1,4,2,3,5)]
queue

# e. Update the queue to reflect the fact that Harold has grown impatient and left.

queue <- queue[1:4]
queue

# f. Update the queue to reflect the fact that Alex has grown impatient and left. (Do this as if you do not
# know what slot Alex currently occupies by number.)

queue <- queue[(queue!="Alex")]
queue

# g. Identify the position of Patricia in the queue.

match("Patricia",queue)

# h. Count the number of people in the queue.

length(queue)



# 2. Modify your answer to quiz exercise 21 so that when you implement the quadratic equation, meaningful
# output is given whether there are one, two, or no solutions. (Hint: Use the discriminant.)

roots <- function (a, b, c)
{
  discr <- b ^ 2 - (4 * a * c)
  if (discr == 0)
  {x <- -b/(2 * a)
  print ("One unique solution:")
  return(c(x))
  }
  else if (discr > 0)
  {
  x1 <- (-1 * b + (( b ^ 2 - (4 * a * c))^.5)) / (2 * a)
  x2 <- (-1 * b - (( b ^ 2 - (4 * a * c))^.5)) / (2 * a)
  print ("Two solutions:")
  return(c(x1,x2))
  }
  else
  {
    print ("No solutions")
  }
}

roots(3,-8,5)

# 3. Use R to determine how many numbers from 1 to 1000 are not divisible by any of 3,7, and 11.

sum((seq(1,1000,1) %% 3 != 0) & (seq(1,1000,1) %% 7 != 0) & (seq(1,1000,1) %% 11 != 0))

# 4. Write R code that takes three input constants f, g, and h and determines whether they form a Pythagorean
# Triple (such that the square of the largest input is equal to the sum of the squares of the other two
#        constants).

isPyth <- function (f, g, h)
{
  allsides <- c(f,g,h)
  hypo <- max(allsides)
  sides <- allsides[(allsides) != hypo]
  if (hypo ^ 2 == sides[1] ^ 2 + sides[2] ^ 2) {
   print ("The constants form a Pythagorean Triple")
  } else {
     print ("The constants do not form a Pythagorean Triple")
  }
}

isPyth(3,4,5)

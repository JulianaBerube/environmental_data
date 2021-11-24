#Lab Assignment 2: Due Sept. 19
#Some logical test examples:
  #Test for equality: ==
  #Test for strict inequality: > and <
  #Test for equal or greater/less than: >= and <=
  #Test for non-equality !=
  #Some important logical operators are:
  #The NOT operator: !
  #You can think of this as flipping the polarity of a TRUE to a FALSE and vice versa.
  #The AND operator: &
    #This one returns a value of TRUE only if both elements are TRUE.
  #The OR operator: |
    #This one returns a value of TRUE if at least one of the elements is TRUE.
    #It will only return a FALSE when both of the test elements are FALSE.

#Creating a larger vector
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

#Creating a boolean value
#Q. 1 & 2
vec_2<-vec_1==3
vec_1[vec_2]

#Q. 3
length(vec_1)
sum(vec_1 == 3)
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

#Q.6: loops
for (i in 1:10)
{
  print (
    paste0("This is loop iteration: ", (i))
  )
}

#Q.7

n=77

for (i in 1:n){print(i)}

#Q.8
#Part 1: creating a vec
n=17
vec_1 = sample(10, n, replace=TRUE)
vec_1

#Part 2: creating a loop
#for (i in 1:17)
{print (paste0("The element of vec_1 at index ", (1:n), " is ", (vec_1) ))}


#Q.9

create_and_print_vec = function(n, min =1 , max =10)
{
  vec_3=sample(min:max, n, replace = TRUE)
  for (i in 1:n)
  print (paste0("The element at index ", i, " is ", vec_3[i] ))
}
create_and_print_vec(77, min=1, max=10)


#Loop lecture Code Practice Sept. 22
for (i in 1:2) #can be any numbers, doesn't need to be increasing or starting at 1
{print (i)}
#try not to name other variables i when using loops, but i can be named other things 
#loop sequence of other things - print these numbers
for (i in c(3, 6, 77))
{print(i)}

#loop types
  #for loop: runs determiend # of times - however many you define
  #While-loops: executes until a test is true - can be infinate 

#Repeats 1-3 twices
for (i in rep(1:3, 2))
{print(i)}







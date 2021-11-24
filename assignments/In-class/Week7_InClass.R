#In Class Assignment: Probability Calculations and Confidence Intervals Oct. 14

#Probablity distributions 

#Ex. 
dpois(x = 7, lambda = 10.4)

#Q. 1 - probabilty of observing four out of 6 with a 2/3 probabilty of observation
dbinom(4,6, 2/3)

#Q.2 - probablity of observing 0
dbinom(0, 6, 2/3)

#Ex. Obersving fewer than 7, observing 7 or greater
ppois(q = 7, lambda = 10.4)
1 - ppois(q = 7, lambda = 10.4)

#Q.4 - 4 or fewer
pbinom(4, 6, 2/3)

#Q.5 - 4 or greater 
1-pbinom(3, 6, 2/3)


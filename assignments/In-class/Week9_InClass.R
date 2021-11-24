#Week9 In Class: Confidence Intervals Oct. 28

#I can use the qnorm() function to find the 0.025% and 0.975% quantiles of the standard normal:
qnorm(c(0.025, 0.975))

#Q.1 ?
qnorm(c(0.05, 0.95))

#Q2
qt(c(.025, .975), df=10)

#Q3
qt(c(.025, .975), df=100)

#Q4
qt(c(.025, .975), df=100000)
qnorm(c(0.025, 0.975))

#Q5
qt(c(.025, .975), df=49)

#Q6

10-error
10+error

error=qnorm(0.975)*3.14/sqrt(50)
CI=(qt(.975, df=49))*error
10+CI
10-CI

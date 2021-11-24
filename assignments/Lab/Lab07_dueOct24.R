#Lab 07: The Bootstrap: due Oct. 24

#Walkthrough
# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

#Minimum and maximum values in each row
apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)
#Note that I used MARGIN = 1 to indicate that I wanted apply() to look at columns
#I used FUN = mean to tell apply() to find the mean value in each column
#datacamp for apply: https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)

#Bootstrapping creates a kind of alternative distribution, while Monte-Carlo resampling can characterize a null distribution.
  #The alternative distribution corresponds to a range of possibilities given associations in the data we actually observed.
  #The null distribution tells us something about what we could expect to see if there were no associations.


#Making a boostrap confidence interval
m = 10000
# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}
mean(result)
quantile(result,c(0.025,0.975))


#Package
#install.packages("boot")
require(boot)
#Syntax
boot(data, statistic, R)

#Calculate mean
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
#Boostrap in data
myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)
str(myboot)

mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)
#BS confidence interval
quantile(
  myboot$t,
  c(0.025, 0.975))

#graphing

moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

#Whole bootstrap function
n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

#Second function?
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#3Function

#rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]
n = nrow(moth_dat)

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 10000)
head(rarefact)

##To test function - FIXED
# This clears the current R session's environment
#rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))

rarefaction_sampler = function(input_dat, n_iterations)
{
  n= nrow(input_dat)
  
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)
rarefact

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

#making a plot
matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Lab Questions

#Q1

# Student T-Distribtuon
#qt function, qt()

#Load data
require(palmerpenguins)

#sse mean function
sse_mean = function(x)
{
  sd(x, na.rm = TRUE)/(sqrt(length(x[!is.na(x)])))
}

#Sample standard deviation - Gentoo
(sd(penguins[[penguins$species=='Gentoo']],na.rm = T))
#gentoo3<-penguins[[penguins$species=='Gentoo']], na.rm = T)

#Subset out gentoo penguins 
penguins  
gentoo=penguins[which(penguins$species=='Gentoo'),]
  #remove NA
gentoo2=gentoo[complete.cases(gentoo$bill_length_mm),]

#Q1. Number of rows
nrow(gentoo2)

#Q2. sd of gentoo penguins and bill length 
sd(gentoo2$bill_length_mm)

#Q3.
qt(0.05, 122)
qt(c(.025, .975), df=122)
   
#Q4. sse mean (standard error) of gentoo and bill length
sse_mean(gentoo2$bill_length_mm)

#Q5. Confidence Intervals 
qt(c(.025, .975), df=122)
sse_mean(gentoo2$bill_length_mm)

(qt(.975, df=122))* (sse_mean(gentoo2$bill_length_mm))

mean(gentoo2$bill_length_mm)

47.50 + 0.55
47.50 - 0.55

#another way to get CI
#quantile(gentoo2$bill_length_mm,c(0.025,0.975))

#Q.6-8
require(boot)
#boot(data, statistic, R)

#Finding boot mean
myboot = 
  boot(
    data = gentoo2$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(myboot)

#Finding boot CI
quantile(
  myboot$t,
  c(0.025, 0.975))

#Q. 9-10
#rarefaction_sampler

#Q.11
rarefact = rarefaction_sampler(moth_dat, 10000)
head(rarefact)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Moth Species richness',
  main='Rarefaction Curve:\n Number of Species Found per Number of Sampling Plots',
  lty=c(1,6,4),col=c(1,"aquamarine3","coral1"))


legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,6,4),col=c(1,"aquamarine3","coral1"), inset=c(.1,.1))


#In-class aggregate and function

#split flipper length into species and calculate mean indivudally & remove NA
aggregate(penguins$flipper_length_mm, list(penguins$species), FUN=mean, na.rm=TRUE)

#Shorter notation (explain flipper length in terms of species)
aggregate(flipper_length_mm~species, data=penguins, FUN=mean, na.rm=TRUE)

#Boxplot formula notation
boxplot(flipper_length_mm~species, data=penguins, FUN=mean, na.rm=TRUE)

#Formula notation species and sex
aggregate(flipper_length_mm~species+sex, data=penguins, FUN=mean, na.rm=TRUE)

boxplot(flipper_length_mm~species+sex, data=penguins, FUN=mean, na.rm=TRUE)




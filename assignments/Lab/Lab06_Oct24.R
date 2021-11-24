#Lab06: Introduction to Inference due Oct. 24

##Notes/ Pre-lab##
#The sample standard error of the mean is the sample standard deviation divided by the square root of the sample size
#SSEm=sx/√n


require(palmerpenguins)

#Building sse_mean function
sse_mean = function(x)
{
 sse=sd(x, na.rm = TRUE)/(sqrt(length(x[!is.na(x)])))
  return(sse) 
}
  
sse_mean(penguins$bill_depth_mm)

#Penguins and p values
boxplot(flipper_length_mm ~ species, data = penguins)
#rm species
dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

#Remove unused factor levels from data frame
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

#For resampling - breaks association
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

#T-test
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
#resampled again and tested
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#comparing means
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

#aggregate function
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

#Num of indivudals 
table(dat_pen$species)

#Resampling with replacement
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

#Making a function
two_group_resample = function(x, n_1, n_2) 
  {
  dat_1 =sample(x, n_1, replace = TRUE)
  
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means=
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(difference_in_means)
}

#Testing function
set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)  

#Resampling experiment
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
sum(abs(mean_differences) >= diff_observed)

#Examine t test
str(t_test)
t_test$estimate

#Lab Questions
#Q1. 
#rm(list = ls())
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#Q. 2-3 - two group function

#Q. 4
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences, xlab = "Mean Differences", main="Histogram of Mean Differences", col="gray")

#5.
sum(abs(mean_differences) >= diff_observed)

#Q. 7
#boxplot
boxplot(body_mass_g ~ species, data = dat_pen, ylab="Body Mass", xlab = "Species", main="Penguin Body Mass by Species")

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

#Q. 8
agg_means
diff_observed


diff_crit=diff(agg_means[, 2])

t.test(body_mass_g ~ species, data = dat_pen)




#Q.10-11
n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$body_mass_g, 68, 152)
  )
}
sum(abs(mean_differences) >= diff_crit)

hist(mean_differences, xlab = "Mean Differences", main="Histogram of Mean Differences\n Body Mass", col="gray")

t.test(mean_differences, alternative = "two.sided", greater=diff_crit)




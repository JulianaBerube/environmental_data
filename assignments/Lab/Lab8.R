require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
#Perform a t-test with the alternative hypothesis that Adelie penguins have shorter flippers than Chinstrap penguins
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

#install package
#install.packages("simpleboot")
require(simpleboot)

#subset out species
ad = droplevels(subset(penguin_dat, species == "Adelie"))
ch = droplevels(subset(penguin_dat, species == "Chinstrap"))

#Two boot function
twoboot=two.boot(ad$flipper_length_mm, ch$flipper_length_mm, mean, 1000)
hist(twoboot)

#get data
require(here)
veg = read.csv(here("data", "vegdata.csv"))

#boxplot
boxplot(pine ~ treatment, dat = veg)

#subset tree data
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
#boxplot
boxplot(pine ~ treatment, dat = dat_tree)

#Figure out # of observations for each treatment type
table(dat_tree$treatment)

aggregate(pine~treatment, data=dat_tree, FUN=mean, na.rm=TRUE)

#wilcox test..not sure
wilcox.test(pine ~ treatment, data = dat_tree)
#wilcox.test(pine~treatment, data=dat_tree, alternative='two.sided")
#wilcox.test(17.875, 1.875, alternative = "two.sided")
#wilcox.test(subset(dat_tree, treatment == "clipped")$pine,
   #         subset(dat_tree, treatment == "control")$pine, alternative = "two.sided")
require(boot)

#Bootstrap of tree data
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )
boot.ci(tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")
quantile(tree_boot$t, 0.025)

#Read in new data
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

#Merge datasets
dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))
#read structure
head(dat_all[, c("b.sidi", "s.sidi")])

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

#Now, examine the standardized data to see if our standardization worked:
mean(dat_all$b.sidi.standardized) 
sd(dat_all$b.sidi.standardized)

##Same process for other column##
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

#Now, examine the standardized data to see if our standardization worked:
mean(dat_all$s.sidi.standardized) 
sd(dat_all$s.sidi.standardized)

##We’ll only be using two of the many variables in the merged data:
  #Simpson’s diversity index for breeding birds: b.sidi
  #Simpson’s diversity index for vegetation cover types: s.sidi

#plot data
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#linear regressions
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#monte carlo method
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

#To create a resampled dataset, we can create two vectors of randomly generated row indices. 
#Then we can use these to create two new vectors of bird and vegetation diversity indices.
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]
print(slope_resampled_i)

#And we can re-create the scatterplot with regression line:
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#First we can pre-allocate a vector to hold the results using numeric(). Check out the help entry to find out more about numeric().
m = 10000 
result = numeric(m) 

#build new loop
m = 10000 
result = numeric(m) 
for(i in 1:m)
{
  m = 10000 
  result = numeric(m) 
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
   
  result[i] = coef(fit_resampled_i)[2]
} 
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
quantile(result, c(.05))

#lab Questions 
#1.
two.boot(ad$flipper_length_mm, ch$flipper_length_mm, mean, 10000)
#2.
two.boot(ad$flipper_length_mm, ch$flipper_length_mm, mean, 10000, na.rm=TRUE)
#3
pen_boot=two.boot(ad$flipper_length_mm, ch$flipper_length_mm, mean, 10000, na.rm=TRUE)
#4
hist(pen_boot)
#5
quantile(pen_boot$t, 0.025)
quantile(pen_boot$t,c(0.025,0.975))
#6
aggregate(flipper_length_mm~species, data=penguin_dat, FUN=mean, na.rm=TRUE)
aggregate(flipper_length_mm~species, data=penguin_dat, FUN=median, na.rm=TRUE)

#mean(pen_boot$data, na.rm=TRUE)
mean(pen_boot$t, na.rm=TRUE)
median(pen_boot$t, na.rm=TRUE)

str(pen_boot)

#Q1. 
sd(pen_boot$t, na.rm=TRUE)

#Q.2
hist(pen_boot$t, xlab="Difference in Means", main="Histogram of Boostrap\n Mean Flipper Lengths")

#Q.3
quantile(pen_boot$t,c(0.025,0.975))

#Q4.Graph

#Q.5
pen_ecdf=ecdf(pen_boot$t)

#Q.6
1-pen_ecdf(-4.5)

#Q.7
pen_ecdf(-8)

#Q.8
#hypotheses

#Q.9
head(veg)
dat_trees = droplevels(subset(veg, treatment %in% c("control", "clipped")))
wilcox.test(pine ~ treatment, data = dat_trees, exact=FALSE)

#Q.10
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

quantile(tree_boot$t, c(0.025,0.975))

#Q.11
mean(tree_boot$t)

#Q.12
dat_all$s.sidi.standardized
dat_all$b.sidi.standardized

#Loop 1 not standardized
for(i in 1:m)
{
  
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
}

#Loop 2 standardized
for(i in 1:m)
{
  
  index_1 = sample(nrow(dat_all), replace = TRUE)
  index_2 = sample(nrow(dat_all), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_all$b.sidi.standardized[index_1],
      s.sidi = dat_all$s.sidi.standardized[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
} 

#Loop 3...wrapped
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))
dat_all = merge(dat_bird, dat_habitat,by = c("basin", "sub"))
dat_1 = subset(dat_all,select = c(b.sidi, s.sidi))

m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
  coef(fit_1)
  slope_observed = coef(fit_1)[2]
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
}
#Quanitle
CV=quantile(result, c(.05))
CV
#Hist
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v=CV, lty=2, col="red", lwd=2)

slope_observed

#Using Models 1 Nov. 7

require(here)

#Questions & walkthrough

catrate = read.csv(here("data", "catrate.csv"))
head(catrate)
summary(catrate)

#Q1
hist(catrate$cat.rate, xlab="Catastrophic Rates", 
     main = "Histogram of Salamander Catastrophic Rates",
     col="brown3")

#Q2-4
shapiro.test(catrate$cat.rate)

#Q5-10
t.test(catrate$cat.rate, mu=0.28)

t.test(catrate$cat.rate, mu=0.28, alternative = "g")

mean(catrate$cat.rate)

#Q11-15
wilcox.test(catrate$cat.rate, mu = 2 / 7, exact=FALSE)
wilcox.test(catrate$cat.rate, mu = 0.28, exact=FALSE, alternative = "g")

#Q16
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")


dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
#dat_gentoo = subset(penguin_dat, species == "Gentoo")


shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)
#shapiro.test(flipper_length_mm~species, data=penguin_dat, FUN=mean, na.rm=TRUE)
aggregate(flipper_length_mm~species, data=penguin_dat, FUN=mean, na.rm=TRUE)

#Q.18

hist(flipper_length_mm ~ species, 
data = penguin_dat,
ylab = "Flipper Length (mm)")

hist(flipper_length_mm ~ species, 
     data = penguin_dat)

png(
  here("assignments", "Plots", "Models_1_hist.png"), 
  width = 1800, height = 1400, 
  res=180)

par(mfrow=c(1,2))

hist(dat_adelie$flipper_length_mm, xlab="Flipper Length", 
     main = "Adelie Flipper Length", 
     ylim=c(0,50),
     col="coral2")

hist(dat_chinstrap$flipper_length_mm,
     xlab="Flipper Length", 
     main = "Chinstrap Flipper Length",
     ylim=c(0,20),
     col="cornflowerblue")

dev.off()

#Q19
t.test(flipper_length_mm ~ species, 
       data = penguin_dat)
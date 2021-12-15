#Using Models 2: Due Nov. 28

#Prep

require(palmerpenguins)
#Try running a 1-sample t-test on the Gentoo penguin flipper lengths
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

#Instead of comparing Gentoo penguin flipper lengths to zero, let’s test whether they are equal to 218 mm:
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

#Let’s try a one-tailed alternative hypothesis: Gentoo penguin flippers are smaller than 218 mm:
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

#compare the flipper lengths of two species
t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

#Running an anova: graphical exploration
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

boxplot(body_mass_g ~ species, data = penguins)

#Numerical:test whether within-group data are normally-distributed.Extract the measurements for each species, Calculate the mean body mass for each species, Conduct Shapiro tests on each species’ body mass.

dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

shapiro.test(dat_chinstrap$body_mass_g)

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)

  #linear model
fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

  #ANOVA
anova(fit_species)

#2 way additive model
fit_additive = lm(body_mass_g ~ sex + species, data = penguins)
fit_interactive = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_interactive)
anova(fit_interactive)

lm(bill_length_mm ~ body_mass_g, data = penguins)

#Questions:
#Q1-3
boxplot(body_mass_g ~ sex + species, data = penguins,
        names = c("Adelie: F","Adelie: M","Chinstrap: F", "Chinstrap: M", "Gentoo: F", "Gentoo: M"),
        xlab="Species & Sex",
        ylab="Body Mass (g)",
        main= "Penguin Body Mass by Sex and Species",
        col=c("lightyellow1","lightyellow2","lightsteelblue1","lightsteelblue2","snow1", "snow2"))

#Q4-5
fit_both=lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)

#Q7
3368.84 +158.37

#Q8
aggregate(flipper_length_mm~species, data=penguin_dat, FUN=mean, na.rm=TRUE)
aggregate(body_mass_g ~ sex, subset(penguins, species == "Adelie"& sex == "female"), FUN= mean)


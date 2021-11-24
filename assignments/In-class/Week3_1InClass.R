#In Class Activity Sept. 16

#installing data - penguin package
#install.packages("palmerpenguins")
require(palmerpenguins)
require(here)

#Convert data to data frame
class(penguins)
penguins = data.frame(penguins)

#Mean bill length - returns NA. Add na.rm to remove outliers - returns 4201.754
mean(penguins$body_mass_g, na.rm = TRUE)

head(penguins)
summary(penguins)

#plots
#scatterplot
plot(penguins$species)
plot(penguins$body_mass_g, penguins$bill_length_mm)
plot(penguins)
pairs(penguins)

#boxplot
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

#coplots with plot save
png(filename = here("basic_coplot.png"), width = 800, height = 600)
coplot(body_mass_g ~ bill_depth_mm | year, data = penguins)
dev.off()

#Plots for questions
plot(penguins$body_mass_g, penguins$bill_length_mm)
plot(penguins$body_mass_g,penguins$bill_depth_mm)
plot(y=penguins$body_mass_g, x=penguins$bill_length_mm)

boxplot(bill_depth_mm ~ sex, data = penguins)
boxplot(bill_depth_mm ~ species, data = penguins)
boxplot(bill_depth_mm ~ island, data = penguins)
png(filename = here("Penguin_boxplot.png"), width = 800, height = 600)


boxplot(body_mass_g ~ island, data = penguins, xlab="Island", ylab = "Penguin Body Mass", main="Penguin Body Mass per Island")

dev.off()


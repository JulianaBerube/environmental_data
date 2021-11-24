#Lab09: Modeling Data 2

#Lab prep##

#Read in data
require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#How likely is a response of 33/61 if the reproductive success and failure are equally likely, i.e., Pr(success)=0.5?
#We can use a binomial test for this, specifying the number of successes (33) and the total sample size (61), as follows
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

#Let’s define variables to hold the late- and normal-filling rates:
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

#We can modify the code we used above to test the observed reproduction success rate against the normal-filling rate:
binom.test(
  n_success,
  n_years,
  p = normal_fill_rate) 

#We might instead prefer the one-sided alternative hypothesis that the observed success rate is less than the pond normal-filling rate
binom.test(
  n_success,
  n_years,
  p = normal_fill_rate,
  alternative ='less')

# To summarize, we can use classical two-sample tests to
  #compare two variances
  #compare two sample means or medians
  #check the correlation of two variables
  #compare two (or more) proportions
  #test for independence of two variables in a contingency table

#The F-statistic represents the ratio between two variances.
  #It is based on the idea that if the variances of the two samples are the same, then the ratio of the variances will be 1

#Read in new data
veg = read.csv(here("data", "vegdata.csv"))
head(veg)

#Boxplot
boxplot(pine ~ treatment, data = veg)

#Let’s test whether the variance in pine seedling count differs between two treatments
var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#Note that Fisher’s F test for unequal variances assumes that the data are normally distributed.
shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])

#If the results indicate that the data are non-normal, then we should use a non-parametric test of homogeneity of variances, such as the Fligner-Killeen test
fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#However, there are roughly equivalent tests for k-sample problems; i.e., when there are more than two groups.The ksample parametric test is called Bartlett’s test, which we can use to test for homogeneity of variances among all four treatment levels as follows:
bartlett.test(pine ~ treatment, data=veg)
fligner.test(pine ~ treatment, data = veg)

#The Student’s t test is appropriate when the samples are independent, the variances constant, and the errors normally distributed. 
t.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#Testing since some samples are non normal
wilcox.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#Subset since t-test doen't take formula notation
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired=TRUE)

wilcox.test(control, clipped, paired=TRUE)

#More data
disp = read.csv(here("data", "dispersal.csv"))
disp

plot(disp$disp.rate.ftb, disp$disp.rate.eb)

#test the significance of the correlation
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

#Spearman’s rank correlation
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#Kolmogorov-Smirnov test
  #Are two sample distributions the same, or are they significantly different from one another in one or more (unspecified) ways?
  #Does a particular sample distribution arise from a particular hypothesized theoretical distribution?

#plot 1
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)

#plot 2
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

#test...
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#This is a simple binomial proportions test, which we can easily do in R by specifying two vectors:
  #the number of mortalities for females and males c(4,16)
  #the total number of female and male candidates: c(40,250)

prop.test(c(4,16),c(40,250))

#Ex chi sq test
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

#fisher's exact test
fisher.test(owls)

#Birds and continegnecy tables 
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

#Lab Questions 
#Q1-2
chisq.test(br_creeper_table)

#Q3-5
require(palmerpenguins)
fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)
#Q3. Body Mass
fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)

#Q4. Body Mass/ Sex
fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)

#Q5. 
fit_both = 
  lm(
    formula = body_mass_g ~ sex*species,
    data = penguins)

#Q6. 
boxplot(formula = body_mass_g ~ species,
        data = penguins,
        ylab="Body Mass (g)",
        xlab="Species",
        main= "Penguin Body Mass by Species",
        col=c("azure", "cornsilk", "honeydew"))

#Q7.
boxplot(formula = body_mass_g ~ sex,
        data = penguins,
        ylab="Body Mass (g)",
        xlab="Sex",
        main= "Penguin Body Mass by Sex",
        col=c("lavender", "lightgoldenrodyellow"))

#Q8
boxplot(formula=body_mass_g ~ sex*species,
        data = penguins,
        ylab="Body Mass (g)",
        xlab="Sex + Species",
        main= "Penguin Body Mass by Species & Sex",
        names = c("Adelie (F)", "Adelie (M)", "Chinstrap (F)", "Chinstrap (M)", "Gentoo (F)", "Gentoo (M)"),
        col=c("azure", "azure", "cornsilk","cornsilk", "honeydew", "honeydew"))

#Q9 - Description

#Q10- null 

#Q11
bartlett.test(body_mass_g ~ species, penguins)

#Q12
bartlett.test(body_mass_g ~ sex, penguins)

#Q13
pen_all=aggregate(body_mass_g ~ sex*species, data=penguins, FUN=c)

bartlett.test(pen_all$body_mass_g)


#Week 10 Reading Q plot

require(here)
png(
  here("assignments", "Plots", "Week10ReadingQ.png"), 
  width = 2000, height = 1400, 
  res=180)

plot(body_mass_g ~ flipper_length_mm, dat_adelie,
     ylab="Body Mass (g) (Predictor Variable)",
     xlab= "Flipper Length (mm) (Response Variable)",
     main="Slope Parameter Example\n Adelie Penguin Body Mass and Flipper Length",
     col="aquamarine3"
)
abline(lm(body_mass_g ~ flipper_length_mm, dat_adelie))
   dev.off()  

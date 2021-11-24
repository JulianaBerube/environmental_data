#Week 10 In Class Model T-tests Nov. 2

require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

#removed NA
#dat_ade=dat_ade[complete.cases(dat_ade),]

#Didn't work. Why?
boxplot(dat_ade$sex, dat_ade$body_mass_g)

#Did work
boxplot(body_mass_g~sex, 
        data = dat_ade, 
        main="Body Mass of Adelie Penguins",  
        ylab="body mass (g)")

#Subset out female penguins
ade_fem = droplevels(subset(dat_ade, sex == "female"))

#t test on female penguins and body mass
t.test(ade_fem$body_mass_g, mu=0)

#Subset out males
ade_mal = droplevels(subset(dat_ade, sex == "male"))

#t-test for males
t.test(ade_mal$body_mass_g, alternative="greater", mu=4000)

#t-test for male and female penguins
t.test(ade_fem$body_mass_g, ade_mal$body_mass_g, alternative = "two.sided")
t.test(body_mass_g~sex, data=dat_ade)

#x y syntax (xy order matters. alternative is relative to x)
t.test(
  x=ade_fem$body_mass_g, 
  y=ade_mal$body_mass_g,
  alternative = "g"
)
#Cant reject null hypothesis above (females are not heavier)^

#Formula notation without subsetting (have to know factor levels(base case) for what alt is 
#relative to. R goes alphabetically but can also do level(dat_ade$sex) or by x in output of t-test)
t.test(body_mass_g~sex,
       data=dat_ade)

t.test(body_mass_g~sex,
       data=dat_ade,
       alternative="g")

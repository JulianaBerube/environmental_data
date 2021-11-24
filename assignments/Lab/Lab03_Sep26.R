#Lab 3: Data Exploration and Deterministic Functions Due Sun. Sept. 26
#note to seld: make r markdown script describing whats in each lab/ activity and with some code  - maybe mae master cheat sheet?



#installing package
#install.packages("psych")
require(psych)

#Pairs.panels function part of psych pacakge- to see pair plots
pairs.panels(iris)

#Loading datasets and naming
require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

dat_habitat<-read.csv(here("data", "hab.sta.csv"))

#Merging two datasets
dat_all=merge(dat_bird, dat_habitat)
#scatterplot
plot(ba.tot ~ elev, data = dat_all)

#Finding # Cedar Waxwings
sample(dat_all$CEWA, 100)
sum(dat_all$CEWA)

#Presence/Absence CEWA
my_vec = rep(1:3, 5)
my_vec == 3
my_vec > 1

#Maikng it boolean,test
P_A=CEWA>0
P_A
#FR now
cewa_present_absent=as.numeric(dat_all$CEWA > 0)
plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}


#Logistic Fit
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)


#Plot arguments
#cex: this controls the size of the plotting character. Default is 1.
#pch: this controls the shape of the plotting character. Default is 1. Code 16 plots a solid point.
#col: this controls the color of the plotting character. Default is 1, corresponding to black.
plot(x = dat_all$elev, y = cewa_present_absent, col=adjustcolor("orange", 0.1), cex=2)

#Lab Questions:
#Plots
png(
  filename=here("assignments", "Lab", "Plots", "Bird_Pair_Plots.png"),
  width=1600, height=1400)

pairs.panels(dat_habitat[c("elev", "slope", "aspect", "ba.tot")])

dev.off()

#Chestnut-bk Chickadee=CBCH
#Northern Flicker=NOFL

#Making birds p/a
cbch_present_absent=as.numeric(dat_all$CBCH > 0)
nofl_present_absent=as.numeric(dat_all$NOFL > 0)

#p/a plots
#plot 1
png(
  filename=here("assignments", "Lab", "Plots", "Chickadee_Plot.png"),
  width=1000, height=500)

plot(x = dat_all$ba.tot, y = cbch_present_absent, col=adjustcolor("darkorange3", 0.03), cex=2, pch=16, xlab="Total Basal Area", ylab = "Presence/Absence", main="Chestnut-bk Chichadee Presence/Absence")
curve(logistic_midpoint_slope(x, midpoint = 30, slope = 0.90), add = TRUE)

dev.off()

#plot 2
png(
  filename=here("assignments", "Lab", "Plots", "Flicker_Plot.png"),
  width=1000, height=500)
plot(x = dat_all$ba.tot, y = nofl_present_absent, col=adjustcolor("firebrick4", 0.04), cex=2, pch=16, xlab="Total Basal Area", ylab = "Presence/Absence", main="Northern Flicker Presence/Absence")
curve(logistic_midpoint_slope(x, midpoint = 15, slope = -0.99), add = TRUE)

dev.off()

#Q7-8
#Gray Jay=GRJA
sum(dat_all$GRJA)
#181

#Q.9
grja_present_absent=as.numeric(dat_all$GRJA > 0)
sum(grja_present_absent)






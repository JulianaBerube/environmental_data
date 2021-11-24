#Lab 5: Uncertainty, Samples, and Populations due Oct. 10

#Linear Function
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
#Ricker Function
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

#Plot
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

#Expoenential Function and plot
exp_fun = function(x, a, b)
{return(a*exp(-b*x))}

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

#idk something else
# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

#add error
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

#more error
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

y_observed_3=
  y_pred+
  rexp(n=n_pts, rate=1.2)
plot(x_sim, y_observed_3, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

#Histograms
par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")


#Lab Q's
#data
require(here)
dispersal=read.csv(here("data", "dispersal.csv"))

#Q2
#curve 1: a = 1.9, b = 0.1, line color = black, line texture = solid
#curve 2: a = 1.9, b = 0.3, line color = black, line texture = dotted
#curve 3: a = 1.2, b = 0.2, line color = red, line texture = solid
#curve 4: a = 1.2, b = 0.4, line color = red, line texture = dotted

curve(
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)")
curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50, lty=3)
curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50, col="red")
curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50, col="red", lty=3)

#Q3
#curve 1: a = 25, b = 0.1, line color = black, line texture = solid
#curve 2: a = 20, b = 0.2, line color = black, line texture = dotted
#curve 3: a = 10, b = 0.2, line color = black, line texture = dotted
#curve 4: a = 75, b = 0.3, line color = red, line texture = solid
#curve 5: a = 50, b = 0.3, line color = red, line texture = dotted
#curve 6: a = 40, b = 0.3, line color = red, line texture = dotted
curve(
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 50, add = FALSE, lty=1, ylab="",xlab="")
curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 50, add = TRUE, lty=3)
curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 50, add = TRUE, lty=3)
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 50, add = TRUE, lty=1,col="red")
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 50, add = TRUE, lty=3, col="red")
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 50, add = TRUE, lty=3,col="red")

#Q.8-13

#Linear
plot(dispersal$disp.rate.ftb~ dispersal$dist.class, main = "Q.9 Linear Model", xlab="Dispersal Class", ylab="Dispersal Rate",
     xlim=c(0, 1500),
     ylim=c(0, 1.2))
guess_x = 600
guess_y = 0.4
guess_slope = -.0004
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#Exponenetial
plot(dispersal$disp.rate.ftb~ dispersal$dist.class,
     main = "Q.11 Exponential Model", xlab="Dispersal Rate", ylab="Dispersal Class",
     xlim=c(0, 1500),
     ylim=c(0, 1.2))
curve(
  exp_fun(x, 1, .0035), add = TRUE, from = 0, to = 1500)
#a is where x and y intersect. B is decay factor 
#set y limits so that they include 0

#Ricker
plot(dispersal$disp.rate.ftb~ dispersal$dist.class,
     main = "Q.13 Ricker Model", xlab="Dispersal Rate", ylab="Dispersal Class",
     xlim=c(0, 1500),
     ylim=c(0, 1.2))
curve(
  ricker_fun(x, .008, 1/250), 
  from = 0, to = 1500 , add = TRUE)
#b control x maximum - so look at x axis/ where you want the peak
#a controls height of curve. Try extreme values and very small ones. Bc units are big on x axis
#a small slope will go up quickly on y axis (small units)

#Q14-15 Residuals 
#lin
y_predicted1<-line_point_slope(dispersal$dist.class, guess_x, guess_y, guess_slope)

resids_linear<-y_predicted1-dispersal$disp.rate.ftb
dispersal=cbind(dispersal, resids_linear)

#Exp
y_predicted2<-exp_fun(dispersal$dist.class, 1, .0035)
resids_exp=y_predicted2-dispersal$disp.rate.ftb
dispersal=cbind(dispersal, resids_exp)

#Ricker
y_predicted3=ricker_fun(dispersal$dist.class,.008, 1/250)
resids_ricker=y_predicted3-dispersal$disp.rate.ftb
dispersal=cbind(dispersal, resids_ricker)

#Resids functions ex. 
#line_point_slope(dat$x, guess_x, guess_y, guess_slope)

#Histogram
par(mfrow = c(1, 3))
hist(dispersal$resids_linear, col="royalblue1", xlab = "Dispersal", main="Linear Dispersal")
hist(dispersal$resids_exp, col="royalblue3", xlab = "Dispersal", main="Exponential Dispersal")
hist(dispersal$resids_ricker, col="royalblue4", xlab = "Dispersal", main="Ricker Dispersal")






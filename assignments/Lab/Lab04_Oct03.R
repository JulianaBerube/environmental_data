#Lab04: Uncertainty and Error due Oct. 3

#dnorm(): the probability density

#pnorm(): the cumulative probability density

#qnorm(): the quantile function

#rnorm(): function to generate random, normally-distributed numbers.

dnorm(-1, mean = 0, sd = 1, log = FALSE)
pnorm(-1.96, mean = 0, sd = 1, log = FALSE)

#Basic plot of normal curve
# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

#Penguin example: histogram
require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")
#Finding the mean, standard deviation, and # of observations
mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

#generate four sets of random penguin body masses using the quantities I calculated from the observed values
set.seed(1)
n_samples = 344
pop_sd = 802
pop_mean = 4202

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
#my test to avoid hard code - works
dat_5 = rnorm(n=nrow(penguins), mean=mean(penguins$body_mass_g, na.rm = TRUE), sd = sd(penguins$body_mass_g, na.rm = TRUE))

#Histograms
par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

set.seed(12)
dat_unif = runif(n = 270, min = 0, max = 4)
hist(dat_unif)

#another model?
set.seed(75)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
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
#Another model pt. 2
set.seed(75)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 5
guess_y = 0
guess_slope = 0.4

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#Predicted values
y_predicted<-line_point_slope(dat$x, guess_x, guess_y, guess_slope)

#Adding a column
dat <- cbind(dat, y_predicted)
resids<-dat$y_predicted-dat$y_observed
dat <- cbind(dat, resids)
sum(dat$resids)

###Functions mini lecture###

require(palmerpenguins)
require(here)


#make file name
image_file = "Ugly Histogram.png"

#Make function to save plot
save_png_1=function(image_file)
{
png(
  here("assignments", "Plots", image_file),
  width=1200, height= 1000
  )
}

#run function and make name
save_png_1("Ugly_hist_2.png")

#run plot
hist(penguins$body_mass_g)
#stop running the image
dev.off()



#define asethetic
dat_vec =penguins$body_mass_g
my_title="Juliana's Hist"
x_label="Data"
hist(dat_vec, col="coral", main=my_title, xlab=x_label)

coral_hist_fun = function(dat_vec, my_title, x_label)
{
  hist(
    dat_vec,
    col="coral",
    main = my_title,
    xlab = x_label
  )
}


coral_hist_fun(
dat_vec=sample(x= 1:100, size=1000, replace=TRUE), my_title = "Jul's Hist", x_label = "x-values")

##Done. Cont. Lab 4

#get absolute value
abs(dat$resids)

plot(dat$resids)
hist(dat$resids)

#Lab Questions 
#Q1
lab.mean<-10.4
lab.sd<-2.4
norm_17 = rnorm(n = 17, mean = lab.mean, sd = lab.sd)
norm_30 = rnorm(n = 30, mean = lab.mean, sd = lab.sd)
norm_300 = rnorm(n = 300, mean = lab.mean, sd = lab.sd)
norm_3000 = rnorm(n = 3000, mean = lab.mean, sd = lab.sd)

#Q2

png(
  here("assignments", "Plots", "lab_04_hist_01.png"),
  width=1500, height= 1600, res=180
)
par(mfrow = c(2, 2))
hist(norm_17, main="17 Data Points")
hist(norm_30, main= "30 Data Points")
hist(norm_300, main= "300 Data Points")
hist(norm_3000, main= "3000 Data Points")

dev.off()

mean(norm_3000)
sd(norm_3000)
#Q.7
# Generate a vector of x-values & plot
png(
  here("assignments", "Plots", "norm_1.png")
  
)
x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean=lab.mean, sd=lab.sd)

plot(x, y, main = "Mean =10.4, SD=2.4", type = "l", xlim = c(0,20 ))
abline(h = 0)

dev.off()

#testing area
pnorm(20, mean=10.4, sd=2.4)

#Q.9
par(mfrow = c(2, 2))
set.seed(97)

png(
  here("assignments", "Plots", "4Plots.png"))
  
n_pts = 250
x_min = 0.5
x_max = 5.0
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(dat, 
     col=("blueviolet"), 
     cex=1.0, pch=5, 
     xlab="X Values", 
     ylab = "Y Observed", 
     main="Plot 1")


set.seed(45)
n_pts = 50
x_min = 15
x_max = 50
x = runif(n = n_pts, min = x_min, max = x_max)
dat2 = data.frame(x = x, y_observed = rnorm(n_pts))

plot(dat2,
        col=adjustcolor("darkolivegreen", 0.5), 
        cex=1.5, pch=16, 
        xlab="X Values", 
        ylab = "Y Observed", 
        main="Plot 2")
#hist(dat)



n_pts = 88
x_min = 1
x_max = 90
x = runif(n = n_pts, min = x_min, max = x_max)
dat3 = data.frame(x = x, y_observed = rnorm(n_pts))
boxplot(dat,
        col=("darkslateblue"), 
        main="Plot 3")


n_pts = 50
x_min = 5
x_max = 100
x = runif(n = n_pts, min = x_min, max = x_max)
dat4 = data.frame(x = x, y_observed = rnorm(n_pts))

hist(dat4$x,col=("darkseagreen"), 
     main="Plot 4",
     xlab = "X")
     
png(
  here("assignments", "Plots", "4Plots.png"))
par(mfrow = c(2, 2))

plot(dat, 
     col=("blueviolet"), 
     cex=1.0, pch=5, 
     xlab="X Values", 
     ylab = "Y Observed", 
     main="Plot 1")
plot(dat2,
     col=adjustcolor("darkolivegreen", 0.5), 
     cex=1.5, pch=16, 
     xlab="X Values", 
     ylab = "Y Observed", 
     main="Plot 2")
boxplot(dat,
        col=("darkslateblue"), 
        main="Plot 3")
hist(dat4$x,col=("darkseagreen"), 
     main="Plot 4",
     xlab = "X")
dev.off()

#Q.11-12
png(
  here("assignments", "Plots", "ModelFit.png"))
plot(dat4,main="Model Fit", ylab="Y")
guess_x = 50
guess_y = 0
guess_slope = -0.01
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

dev.off()

#Q.13-14
n_pts = 50
x_min = 5
x_max = 100
x = runif(n = n_pts, min = x_min, max = x_max)
dat4 = data.frame(x = x, y_observed = rnorm(n_pts))

y_predicted<-line_point_slope(dat4$x, guess_x, guess_y, guess_slope)
resids<-dat4$y_predicted-dat4$y_observed
dat4 <- cbind(dat4, y_predicted)
dat4 <- cbind(dat4, resids)
par(mfrow = c(1, 2))
hist(dat4$resids,main = "Histogram of Residuals", xlab="Residuals", col="royalblue4")
plot(x=dat4$y_predicted, y=dat4$resids,
     main="Plot of Residuals and Y Predicted Values",
     xlab="Y Predicted", ylab="Residuals",
     col="royalblue4")





#Data exploration and deterministic functions: Individual assignment due Sept. 19

#intall package 
install.packages("here")
library("here")
file.exists(here("data", "hab.sta.csv"))

#Laod data
dat_habitat<-read.csv(here("data", "hab.sta.csv"))

##3 Historgrams##
#Order following plots in a row
par(mfrow = c(2, 3))
#Elevation Hist
hist(dat_habitat$elev, xlab="Elevation",main = "Habitat Elevation")
summary(dat_habitat$elev)
#Slope Hist
hist(dat_habitat$slope, xlab="Slope", main="Habitat Slope")
summary(dat_habitat$slope)
#Aspect Hist
summary(dat_habitat$aspect)
#hist(dat_habitat$aspect, xlab="Aspect", main="Habitat Aspect", breaks=seq(0,400,100))
hist(dat_habitat$aspect, xlab="Aspect", main="Habitat Aspect", breaks=c(0,90,180, 270, 360))

##Function for slope line
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


##3 Scatterplots##
#Elevation
plot(x = dat_habitat$elev, y = dat_habitat$ba.tot, col="pink", xlab="Elevation", ylab="Total Basal Area",
     title(main = "Basal Area and Elevation"), cex=0.5)
abline(lm(dat_habitat$ba.tot ~ dat_habitat$elev, data=dat_habitat), col="blue")
#curve(line_point_slope(x, x1 = 3, y1 = 24, slope = 0.01), add = TRUE, col="blue")
#Slope
plot(x = dat_habitat$slope, y = dat_habitat$ba.tot, cex=0.5, col="orange", xlab="Slope", ylab="Total Basal Area",
     title(main = "Basal Area and Slope"))
abline(lm(dat_habitat$ba.tot ~ dat_habitat$slope, data=dat_habitat), col="blue")
#curve(line_point_slope(x, x1 = 3, y1 = 24, slope = 0.05), add = TRUE, col="blue")

#Aspect
plot(x = dat_habitat$aspect, y = dat_habitat$ba.tot, cex=0.5, col="goldenrod", xlab="Aspect", ylab="Total Basal Area",
     title(main = "Basal Area and Aspect"))
abline(lm(dat_habitat$ba.tot ~ dat_habitat$aspect, data=dat_habitat), col="blue")
#curve(line_point_slope(x, x1 = 3, y1 = 24, slope = 0), add = TRUE, col="blue")

##Linear regression model: Aspect
aspect.lm <- lm(aspect ~ ba.tot, data = dat_habitat)
summary(aspect.lm)
#gpglot LR for slope 
library(ggplot2)
Aspect<-ggplot(dat_habitat, aes(x=aspect, y=ba.tot))+
  geom_point()+
  geom_smooth(method="lm", col="black")
Aspect
#Slope
ggplot(dat_habitat, aes(x=slope, y=ba.tot))+
  geom_point()+
  geom_smooth(method="lm", col="black")
#elevation
ggplot(dat_habitat, aes(x=elev, y=ba.tot))+
  geom_point()+
  geom_smooth(method="lm", col="black")




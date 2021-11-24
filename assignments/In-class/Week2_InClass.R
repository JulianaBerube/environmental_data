#Load data
data(iris)

#Look at data
head(iris)

#Basic scatter plot
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)

data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)

plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")


#function
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

line_point_slope(2, 4, 4, -2)

#plot w/ line & point
plot(x = iris$Sepal.Width, y = iris$Sepal.Length, xlab="What")
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.1), 
  add = TRUE)

#Activity
library(MASS)
data(Animals)
head(Animals)


plot(x = Animals$body, y = Animals$brain)
points(x = data_center_x.1, y = data_center_y.2, col = "red")


data_center_x.1 = mean(Animals$body)
data_center_y.2 = mean(Animals$brain)
c(data_center_x.1, data_center_y.2)


plot(x = Animals$body, y = Animals$brain, xlab="Body", ylab="Brain",
     title(main = "Animal body to brain size: Juliana Berube"), 
     xlim=c(0,15000), ylim=c(0,1500))

points(x = data_center_x.1, y = data_center_y.2, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x.1, 
    data_center_y.2,
    -0.1), 
  add = TRUE)



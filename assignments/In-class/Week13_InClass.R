#In-Class Ginkgo Data Exploration: Nov 23
#Boxplot
require(here)
ginkgo = read.csv(here("data", "ginkgo_data_2021.csv"))
boxplot(notch_depth~seeds_present, data=ginkgo,
        xlab="Seeds Present",
        ylab="Notch Depth",
        main="Boxplot of notch depth by seeds present")

#Scatterplot
plot(max_width~max_depth, data=ginkgo, group=seeds_present,col=c("blue", "green"))

#Week 8 In_class Likliehoods Oct 21-
require(here)
bird= read.csv(here("data", "bird.sta.csv"))
habitat= read.csv(here("data", "hab.sta.csv"))

#prep
x_observed = c(2, 6)
print(x_observed)
dpois(x = 2, lambda = 4.5)
#I know the likelihood of observing those particular counts together is the product of the individual likelihoods:
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
prod(dpois(x = wiwa_counts, lambda = 4.5))
#sum of log likelihoods
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

#Questions
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
log(dpois(2, lambda=4.0))

#WIWR
hist(dat_all$WIWR, breaks=6, xlab="Winter Wren Count", main="Histogram of Winter Wren Counts")


mean(dat_all$WIWR)

1.5/6





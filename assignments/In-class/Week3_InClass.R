dat_birds = read.csv("https://michaelfrancenelson.github.io/environmental_data/data/bird.sta.csv")
dat_habitat = read.csv("https://michaelfrancenelson.github.io/environmental_data/data/hab.sta.csv")

hist(dat_habitat$ba.con)
hist(dat_habitat$ba.hard)

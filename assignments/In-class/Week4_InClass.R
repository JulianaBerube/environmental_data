##Tue Sept. 21##
dat_catrate<-read.csv(here("data", "catrate.csv"))
dat_delomys<-read.csv(here("data","delomys.csv"))
dat_rope<-read.csv(here("data","rope.csv"))

head(dat_catrate)
head(dat_delomys)
head(dat_rope)

hist(dat_catrate$cat.rate)

hist(dat_delomys$body_mass)
hist(dat_rope$p.strength)

plot(x=dat_catrate$cat.rate, y=dat_catrate$pond)
plot(x=dat_delomys$body_mass, y=dat_delomys$body_length, title(main = "J.Berube\n Dat_delomys dataset"),xlab="Body Mass", ylab="Body Length", )

##Thur Sept. 23##
#Reading in data files
dat_habitat<-read.csv(here("data", "hab.sta.csv"))
dat_birds<-read.csv(here("data", "bird.sta.csv"))

#Pair plots 
pairs(dat_habitat[, c("ba.con", "ba.hard", "ba.snag", "ba.tot", "ba.ratio")])

#Histogram with 4 breaks, axis not centered
hist(dat_birds$BHGR, breaks=c(0, 1, 2, 3),
     main = "Histogram of Black-headed Grosbeak",xlab="Black-headed Grosbeak")

#Histogram for BHGB in 4 breaks, axis centered 6
hist(dat_birds$BHGR, breaks=(0:4-0.5),  main = "Histogram of Black-headed Grosbeak",xlab="Black-headed Grosbeak")


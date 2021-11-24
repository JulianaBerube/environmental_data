#Section 1
a1<-"JB"
b1<-45.6
b2<-"45.6"
c1<-0:3
b1+c1

#Section 2
v1<- -2:2
v2<- v1*3
sum(v2)

#Section 3
vec_4<-1:12
mat_1<-matrix(vec_4, nrow=3, ncol=4, byrow = TRUE)
mat_2<-matrix(vec_4, nrow=3, ncol=4)
my_list_1<-list(5.2, "five point two", 0:5)
names(my_list_1)<-c("two", "one", "three")
my_list_1$"three"
my_list_1$"one"
my_list_1[3]


my_vec = rep(1:3, 5)
my_vec


my_bool_vec<-my_vec==3

#test
data.frame(my_vec, my_bool_vec)

my_vec[my_bool_vec]


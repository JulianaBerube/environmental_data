getwd()
c_1 = c(1, 2, 3)
c_2= "c(1, 2, 3)"

c_2

my_vec<-c(1,2,3)
mat_1 = matrix(my_vec)
mat_1[3,1]

vec_2<-c("one", "two", "three")
vec_3<-c(1.0, 2.0, 3.0)

mat_2=matrix(my_vec, nrow=2, ncol = 3)
mat_3=matrix(my_vec, nrow=3, ncol = 2)
mat_4=matrix(my_vec, nrow=5, ncol=5)

list_vector<-1:5

my_list_1<-list(5.2, "five point two", list_vector)
my_list_2<-list(5.2, "five point two", 1:5)
names(my_list_1) <- c("two", "one", "three")

my_list_1[[1]] 
my_list_1[[as.numeric("1")]] 
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one" #(only works with names)#
my_list_1$1

my_list_1$"1"

#newline




---
  author: "Juliana Berube"
output: html_document:
  toc: yes
toc_float: yes
---
  remove.packages("yaml")
getwd()




# Statistical learning methods
# Lab 1 - Introduction to R, Logistic regression, shiny, plotly
# materials: https://github.com/nosarzewski/SGH_SDM_18
# mail: nosarzewski.aleks@gmail.com, an56786@sgh.waw.pl

# Plan
#   I. About R
#   II. Working R
#   III. Objects in R
#   IV. Data Export/Import 
#   V. Statistics in R
#   VI. Conditional instructions
#   VII. Loops
#   VIII. Functions

# I. About R -------------------------------------------------------------

# Where to find information?
# http://www.r-project.org/doc/bib/R-books.html
## manuals: 
# http://cran.r-project.org/manuals.html
## R for data analysis
##  Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani
## An Introduction to Statistical Learning with Applications in R
#http://www-bcf.usc.edu/~gareth/ISL/
## Andy Field, Discovering Statistics Using R - very straightforward and easy to understand

#Polish books, websites:
## Bogumił Kamiński, Mateusz Zawisza "Receptury w R - Podręcznik dla ekonomistów"
##K. Kopczewska, T. Kopczewski, P. Wójcik, Metody ilościowe w R. Aplikacje ekonomiczne i finansowe, 2009, CeDeWu
## Eugeniusz Gatnar, Marek Walesiak "Analiza danych jakościowych i symbolicznych z wykorzystaniem programu R"
## Eugeniusz Gatnar, Marek Walesiak "Statystyczna analiza danych z wykorzystaniem programu R"
## Przemysław Biecek 'Przewodnik po pakiecie R'
# http://www.biecek.pl/R/R.pdf
## Przemysław Biecek 'Na przelaj przez Data Mining z pakietem R'
# http://www.biecek.pl/NaPrzelajPrzezDataMining/NaPrzelajPrzezDataMining.pdf

## Project website:
# http://www.r-project.org/

##Some MOOCs:
# https://www.datacamp.com/courses/free-introduction-to-r
# http://tryr.codeschool.com/
# http://swirlstats.com/ (learning R in R!)
# https://www.edx.org/course/introduction-r-data-science-microsoft-dat204x-7

## I strongly recommend attending prof. Ramsza's classes (both in Polish and English) - a bit more demanding but very valueable:
# Basic R programming / Podstawy programowania w R
# http://michal.ramsza.org/

# What if I did't find there help?
# http://r.789695.n4.nabble.com/
# http://stackoverflow.com/

## Selected keyboard shortcuts in RSTUDIO
# CTRL+ENTER: running a instruction (R GUI - F5): active line or 
# TAB: hints for functions/objects
# TAB after opening bracket: function arguments
# F1 on function: help
# CTRL+1: switching to editor
# CTRL+2: switching to console


# I. Working with R -----------------------------------------------------------


# How R works:
# - console
# - scripts


# If we need documantation of function (or any other thing)
# help(<function_name>)
help()
help(plot)

# Similar to:
# ?function_name - looks for functions with identical name as given string
# ??"function_name" - looks for functions that have in name or in description given string
?plot
??"plot"

# Setting and changing working directory
getwd() 
setwd("D:/") #R uses Unix-type (macOS or Linux) locations, on Windows replace '\' with '/'
dir() #content of working directory

# We can also change it by menu and "session" tab


# Environment
# keeps declared variables/functions/data
x <- 2
ls()            # check what we have in workspace
rm("x")         # deleting element from workspace
rm(list=ls())   # deleting ALL elements from workspace


## Additional packages

# Installing
# install.packages("package_name")
install.packages("randomForest")

# First you have to load package, before you use it
# library(<package_name>)
library(randomForest)

require(randomForest)

## Types of variables 
# There are no seperate types for letters and words
typeof()

#logical values
T == TRUE   
F == FALSE   

# Class is different from type. Type defines the way that object is kept
# Class is an attribute of the object (in the terms of object programming)
# Klasa to co innego ni? typ. Typ kt?ry okre?la wewn?trzny spos?b 
typeof(1)
class(1)

# Family of function checking and converting types 
is.integer(1.5)
as.character(3)

# If we do not have data
NA           # Not Available
is.na(NA)

NaN     # Not a Number
is.na(NaN)

#check vetor with NA and "NA"

# what to do with na? na.rm
abc <- c(1,2,3,NA)
mean(abc, na.rm = TRUE)

# arrows can be used in both ways
10 -> a
a

# multiassignement
a <- b <- c <- 20
a
b
c

# III. Objects in R --------------------------------------------------------

# !!!  In R evertying is an object


### scalars

b <- 5
b

# Maths


# Mathematical functions
# square root
# absolute value
# logarithm

2^3         # power
sqrt(5)     # square root of 5
5^0.5
5^(-1/3)
abs(-99)    # absolute value of -99

log(56)     # natural logarithm
log2(64)    # binary logarithm
logb(56, base = 5.6) # base of choice

exp(2)      # Euler number to the power of 2 (e^2)

factorial(5)   # 5!

sin(0)
?Trig

#How do we round numbers?
ceiling(3.5)
floor(3.5)
trunc(5.99)
# difference between 'trunc' and 'floor'
x <- c(-3.2, -1.8, 2.3, 2.9)
floor(x)
trunc(x)

round(6.592, digits = 2)

### Vectors

# !!! Vector keeps elements of the same type 

# Vectors begin with 'c'
x <- c(1, 6, 9) 
x     
y <- c("a", "b", "c")
y
z <- c(TRUE, FALSE, TRUE)

# !!! Vector keeps elements of the same type 
f <- c("a", 1, TRUE)
f   # values automatically converted to strings

#Sequences
1:10
x <- seq(1, 8, by = 2)     
x
x <- seq(1, 8, length = 5)
x

#Vectors with replicated values
rep(c(1, 3, 5), times = 3)
rep(c(1, 3, 5), each = 3)
rep(c(1, 3, 5), times = 3, each = 3)

# Different functions useful for vectors
#sample
#length
#rev
#unique
#sort
#order
#sum, cummulative sum
#product, cumulative prod
#difference

x <- sample(1:10, 10) # random vector
length(x)      # haw many elements?
rev(x)         # reversing backwards
unique(x)      # unique values
sort(x)        # sorting (by default ascending)
sort(x, decreasing = TRUE)
order(x)       # order in sorted vector
x[order(x)]    # we get sorted vector

sum(x)         # sum of vector elements
prod(x)        # product of vectors elements

cumsum(x)      # cumulative sum
cumprod(x)     # cumulative product
diff(x)        # differences between following elements

#Generating random numbers
set.seed(1) #setting seed of generator
los <- rnorm(1000) # generates 1000 pseudo random numbers from N(0,1)
hist(los,freq=0) # draws histogram
lines(density(los),col="blue") # adds density function

x <- seq(from=min(los),to=max(los),by=0.1)
mean(los) #srednia
sd(los) #odchylenie standardowe
wart.f.gestosci <- dnorm(x,mean=mean(los),sd=sd(los)) # theoretical density
lines(x,wart.f.gestosci,col="red")

hist(x<-rnorm(100,mean=10,sd=3),freq=0)
lines(density(x))

los<-runif(10^6)
hist(los,xlim=c(-1,2))

# Indexing, can be done be reffering the position
y <- c(3, 5, 1, -0.9, 44, 17, 9)
y[5]      # 5th element
y[3:5]    # elements from 3 to 5
y[4] <- 1000
y
y[-4]     # except 4th element
# we can also use vectors for indexing
y[c(1, 5, 6)]
y[c(-2, -3, -7)]
# it is not possible to mix positive and negative values
y[c(-3, 1)]


# or by logical values
# & - and
# | - or
y <- rnorm(100)
y > 0 

y[y > 2]           # elements bigger than 5
y[y < -2 | y > 2]  # elements smaller than -2 OR greater than 2
y[y > 1 & y < 2]   # elements greater than 1 AND lower than 2
y[y > mean(y)]

x <- y            # x becomes y
x == y            # comparing values
all(x == y)       # comparing whole vectors
(x[6] <- 0)
!(x == y)         # negation
all(x == y)
any(x == y)

## Working on vectors
# working sperately on each element
# or we can use functions

# mathematical operations performed on each element seperately
(x <- 1:12)
x + 2
x * 5
x ^ 2

# working with two vectors
x <- 1:5
y <- 6:10
x + y
x^y

# Vector functions
# many functions take numbers, as well as vectors as arguments
# function os performed for each element seperately
sin(x)
exp(x)


## Excercise
# Create vector named id that consints of numbers from your student number
# Check if its type is numeric or character
# Replace 3th element with 5
# Take 1st and 2nd element to the power of two
# Check which elements are greater than 8 and smaller or equal to 35
# Sort decreasingly
# Sum all elements, devide by 3 and round to integer value

## Matrix
# Matrices keep elements of the same type
x <- seq(1, 99, length = 8)
A <- matrix(x, ncol = 4, nrow = 2, byrow = FALSE)
A
A <- matrix(x, ncol = 4, nrow = 2, byrow = TRUE)  
A

# Indexing
A[3:4] # returns elements counting form up to down and then from left to right
A[2, ] # whole rows
A[, 4] # whole columns
A

# Multiplying matrixes
B <- matrix(1:16, nrow = 4)
A %*% B

# dimmensins have to be correct
B %*% A

dim(A) # output: x,y, where: x - rows, y - columns
nrow(A)
ncol(B)

# Multiplying elements by each other
A * A

# Important
# For matrixes and vectors we can use most mathematical functions
log(B)
sin(A)
A * 5
A + 10

# Binding matrixes
cbind(B, t(B))
rbind(A, 2 * A)
# dimensions must be the same!
cbind(A, B)
rbind(A, B)


### Lists
# Lists are a collection of vectors that can keep elements of different types (or other lists as well)

myList <- list(a = 1, b = "a", c = 1:4, d = list(), 6)
myList

#Indexing

# ATTENTION list indexing is similar to vectors but there are some substantioal differences
myList[3]           # returns a list of one element
class(myList[3])    # just to make sure that we get a list instead of vector
myList[[3]]         # returns an element of list 
myList[[3]][1]      # returns first element of third element from list

#or we can reffer to name
lista[["c"]]
# or use '$' operator
lista$c

##### DATA FRAME
Dane <- data.frame(aa = B[,1], bb = B[,2], B[,3])
# Vectors have to have the same length
Dane
Dane$aa

#
attributes(Dane) 
head(Dane, 1)    
tail(Dane)
str(Dane)

# Most functions can be applied to data.frame
sin(Dane)
t(Dane)
rbind(Dane, Dane)

# EXCERCISE
# 
# Create data frame "myData" with following records:
# ? name
# ? second_name (NA if he/she does not have)
# ? age
# ? sex (TRUE if woman, FALSE if man)
# containg data for 3 people
# order according to age
# choose 1 and 3 record
# add new column named occup that has value 'student' for all cases

#### Factors

eyeColor <- c('n', 'n', 'z', 'b', 'b', 'b', 'n', 'z', 'b', 'z')
eyeColor
eyeColor <- factor(eyeColor)
eyeColor
levels(eyeColor)
levels(eyeColor) <- c("brown", "blue", "green")

####  Working with text value ####

# 'paste' -
tekst <- c("X","Y","Z")
liczba <- c(1:12)
paste(tekst, liczba, sep="")
paste(tekst, liczba, collapse = "")
paste0(tekst, liczba)


zdanie <- c("Ala", "ma", "kota")	
grep("Ala", zdanie)     # returns indices of elements that match given pattern
grepl("ma", zdanie)     # as above but returns logical vector
gsub("ma", "nie ma", zdanie)	# replacing given string with another

	

# IV. Data export/import ---------------------------------------------------

?read.table
?read.csv
?read.csv2

getwd()
setwd("/Desktop/materiały") # in labs it is advised to use desktop as location (due to access issues)

ex_data<-read.csv("quantile_health.csv",sep=",",dec=".")
read.csv()
read.csv2()

# Useful libraries: 'foreign' (SPSS, SAS formats), XLConnect (xls, xlsx)

# V. Statistics in R -------------------------------------------------

require(moments)
require(MASS)

#1. Basic statistics

# Lets use previous data

head(ex_data) # first observations and column names
tail(ex_data) # last observations and column names
colnames(ex_data) # column names
ex_data$age # particular column/variable
ex_data[,6] # same as '$'

ex_data <- rbind(ex_data, rep(NA, length(colnames(ex_data))))
sum(is.na(ex_data)) # counts NAs (lacking observations)

summary(ex_data) # basic statistics for each column/variable 

# A) Mean, median, quantiles

mean(ex_data$age)
mean(ex_data$age[!is.na(ex_data$age)])
mean(ex_data$age, na.rm=TRUE)
ex_data <- ex_data[-nrow(ex_data),]
mean(ex_data$age)
mean(ex_data$age, trim=0.10) # skips 10% of highest and lowest values
quantile(ex_data$age, 0.1) #10th percentile
median(ex_data$age) # same as second quartile

plot(ex_data$age,pch=20,type="p", ylab = "Age", xlab = "Number of observation", ylim = c(50,100))
# pch - typ punktu - 20 to wypelniona kropka xlab - opis osi x, ylab opis osi y, typ wykresu (tu: p-points)
abline(h=mean(ex_data$age),col="red",lwd=2) 
# linia oznaczajaca wartosc srednia , abline - kreski pozioma (horizontal h) i pionowa (vertrical v)
abline(h=quantile(ex_data$age,0.1),col="red",lwd=2,lty=2)
# linia oznaczajaca wartosc srednia , abline - kreski pozioma (horizontal h) i pionowa (vertrical v),lty - typ lini, lwd - grubo?? lini
abline(h=quantile(ex_data$age,0.9),col="red",lwd=2,lty=2) 


# B) Variance and standard deviation

var(ex_data$age)
sd(ex_data$age) # standard deviation
abline(h=c(mean(ex_data$age)+3*sd(ex_data$age),mean(ex_data$age)-3*sd(ex_data$age)),lwd=2,lty="dashed",col="blue")

# C) Maximum and minimum
max(ex_data$age)
min(ex_data$age)


# D) Quantiles
quantile(ex_data$age)

# E) Range (rozstęp)
range(ex_data$age)

# F) Quantile range
IQR(ex_data$age)

# G) Number of elements
length(ex_data$age)

# H) Skewness
skewness(ex_data$age)

# I) Kurtosis
kurtosis(ex_data$age)-3 # this function does not subtract 3 from kurtosis so we have to do this manually

# K) Correlation
cor(ex_data$age,ex_data$totexp)
cor(ex_data$totexp,ex_data$totchr)

# L) Standarization
stand<-scale(ex_data$totexp) # scales to normal distribution N(0,1)
sigma<-stand[stand<3 & stand>-3] # we get rid of extreme observations (differing from bean more than 3 st. deviations)
length(sigma)/length(stand)

# N) All-in-One
summary(ex_data$age)

##Frequencies
tablica <- table(ex_data$white,ex_data$female)

prop.table(tablica,1)
prop.table(tablica,2)


margin.table(tablica,1)
margin.table(tablica,2)

#Excercise
# Load dataset 'cars' 
# Check number of observations
# Check tha values of variables in the last line
# Calculate mean, median, standard deviation for variable 'disp'. Chcek the skewness
# Draw a scatterplot for variable 'Hight". With red line indicate the mean value, change axis nammes


# Conditional instructions -------------------------------------------------


(variable <- rnorm(1))

#IF statement

# IF condition met THEN do something
if (variable < 0){
  cat("lower \n")
}

# IF condition met THEN do something ELSE do something else
if (variable < 0){
  cat("lower \n")
}else{
  cat("higher")
}


# condition must return only one value
if (c(-1,0,1) > 0){
  cat("higher\n")
}


# advanced conditions
variable <- 1.2
if (variable < 0 && variable^2 > 0.5) {
    cat("OK\n")
  } else {
    cat("Not OK\n")
  }

if (variable < 0 || variable^2 > 0.5){
  cat("OK\n") 
}else{
  cat("Not OK\n")
}

### IFELSE - vector version of IF statement 

(variable <- rnorm(5))

ifelse(variable < 0, "lower", "higher")
d <- ifelse(variable< 0, "lower", "higher")
# it is done for each element
d

# values returned can also be vectors
x <- 1:5
y <- -(1:5)

ifelse(variable < 0, x, y)


# VII. Loops ---------------------------------------------------------------


### FOR

for(i in 1:5) {
  cat("Current value of i is:", i, "\n")
}


(macierz <- matrix(1:20, 5, 4))
for(i in 1:nrow(macierz)) {
  print(mean(macierz[i,]))
}

# Loops in R are SLOW, it is better to use vector functions instead when possible

rowMeans(macierz)


# next skips to next element immediately (omits code after itself)
## 'next' i 'break'
for(i in 1:5){
  if (i == 3) next
  cat(paste("Current value of i is:", i, "\n"))  
}

# break ends loop
for(i in 1:5){
  cat(paste("Current value of i is:", i, "\n"))  
  if (i == 3) break
}

# Nested loops 
for(i in 1:4){
  for (j in 1:4){
    if (j == i) break
    cat(paste("Current value of i is:", i, "and for j:", j, "\n"))
  }
}

#VIII. Functions


myFunction <- function(argumentsNames){
  instructions
}


#

sayHello<-function(i){
  cat(rep("Hello world!\n",i))
}
sayHello(3)

jedynka <- function(x, y){
  z1 <- sin(x)
  z2 <- cos(y)
  z1^2 + z2^2
}
jedynka(2,1)
# by default only last value calculated is returned

# we can declare what should be returned
jedynka <- function(x, y){
  z1 <- sin(x)
  z2 <- cos(y)
  wynik <- z1^2+z2^2
  return(wynik)
  # return ends function so 'blablabla' won't be displayed
  cat("blablabla")
}
jedynka(2,1)

#-------------------LOGISTIC REGRESSION-----------------------------

rm(list=ls())
dane <- read.csv("adult.csv")
head(dane)
attach(dane)

# number of occurances
table(ABOVE50K)
cut_off <- sum(ABOVE50K)/length(ABOVE50K)

# create Training set
input_ones <- dane[which(ABOVE50K == 1), ]  # all 1's
input_zeros <- dane[which(ABOVE50K == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# create Test set
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

# build model
logitMod <- glm(ABOVE50K ~ ., data=trainingData, family=binomial(link="logit"))
summary(logitMod)

logitMod_short <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, 
                data=trainingData, family=binomial(link="logit"))
summary(logitMod_short)

# predict for test set
predicted_odds <- predict(logitMod_short, testData)  # predicted odds (not from [0,1])
predicted_prob <- plogis(predict(logitMod_short, testData))  # predicted probabilities (from [0,1])
predicted_bin <- ifelse(predicted_prob<cut_off,0,1) # predicted classification

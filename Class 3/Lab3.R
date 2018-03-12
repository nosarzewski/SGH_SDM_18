# Statistical Learning Methods
# Lab 3 - Non-parametric regression models: LOESS (Local Regression) & GAMs (Generalized Additive Models)
# materials: https://github.com/nosarzewski/SGH_SDM_18
# mail: nosarzewski.aleks@gmail.com, an56786@sgh.waw.pl

library(ISLR)
attach(Wage)

# LOESS (Local Regression)

# let's prepare some parameters for plotting
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])

# fitting LOESS
# s parameter (span) controls of how much observations each neighbourhood consists
# s = 0.3 means each neighbourhood consists of 30% of total observations
fit <- loess(wage~age, span=.2, data=Wage) # for s=0.2 (better local fit, line more wiggly)
fit2 <- loess(wage~age, span=.5, data=Wage) # for s=0.5 (more global fit, smoother line)

# plotting results
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Local Regression")
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2) # s=0.2
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2) # s=0.5
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# for curious ones: you can also use 'locfit' library to fit LOESS
if (!require(locfit)){
  install.packages('locfit')
  library(locfit)
}
# instead of span we specify alpha (doeas the same thing)
alt_fit <- locfit(wage~age, data = Wage, alpha = 0.5)

# Choosing span based on Cross Validation
set.seed(1)

# Loading data
RAW_DATA_SET <- readLines("https://raw.githubusercontent.com/nosarzewski/SGH_SDM_18/master/Class%203/housing.data.txt")
LSTAT <- as.numeric(substr(RAW_DATA_SET, 85, 90))
MEDV <- as.numeric(substr(RAW_DATA_SET, 92, 97))

# Function for splitting dataset in folds
GetSplitLabels <- function(data.length, proportions) {
  proportioned.labels <- rep(1:length(proportions), proportions)
  labels <- rep(proportioned.labels,len = data.length)
  return(sample(labels, data.length))
}

CV_FOLDS <- 10 # number of subsamples (folds)
SPAN_LEVELS <- seq(from = 0.1, to = 1, length.out = 50) # span values to test
split.labels <- GetSplitLabels(length(LSTAT), rep(1, CV_FOLDS)) # split into folds

sse <- numeric(length(SPAN_LEVELS)) # empty vector for storing SSE for spans
for (fold in 1:CV_FOLDS) {
  training.x <- LSTAT[split.labels != fold]
  training.y <- MEDV[split.labels != fold]
  testing.x <- LSTAT[split.labels == fold]
  testing.y <- MEDV[split.labels == fold]
  for (i in 1:length(SPAN_LEVELS)) {
    model <- loess(training.y ~ training.x, span = SPAN_LEVELS[i],
                   control = loess.control(surface = "direct"))
    prediction <- predict(model, data.frame(training.x = testing.x))
    sse[i] <- sse[i] + sum((prediction - testing.y) ^ 2)
  }
}
optimal.span <- SPAN_LEVELS[which.min(sse)]
model <- loess(MEDV ~ LSTAT, span = optimal.span)

par(mfrow = c(1, 2))
plot(SPAN_LEVELS, sse, pch = 19)
points(optimal.span, sse[which.min(sse)], col = "red", pch = 19)
plot(LSTAT, MEDV, pch = 20)
lines(sort(LSTAT), predict(model)[order(LSTAT)], col = "red", lwd = 3)

# GAMs (Generalized Additive Models)

if (!require(splines)){
  install.packages('splines')
  library(splines)
}

# we can perform one type of GAM using functions we already know
# let us transform our variables using natural splines (ns() function)
# natural spline is spline with additional constraint: function has to be linear near knots
# in other words: it has to be continuous and near knots smooth 
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

# We can also use smoothing splines, but then we need to use gam() to fit model
if (!require(gam)){
  install.packages('gam')
  library(gam)
}
# s(variable, df) indicates we are using smoothing splines
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

# Let's plot these two models
par(mfrow=c(2,3))
plot(gam.m3, se=TRUE, col="blue") # for gam() objects we can use plot()
plot.Gam(gam1, se=TRUE, col="red") # for lm() objects we need to use plot.Gam()
# we see that results are similar, altough not identical
# in most cases in GAMs difference between natural splines vs smoothing splines are small

# Let's take a look at function of year, which looks rather linear
# we already have a model with spline of year: gam.m3
# let's build others:
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage) # with no year whatsoever
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage) # with linear effect of year

# Now we perform ANOVA to see which model is best
anova(gam.m1, gam.m2, gam.m3, test="F")
# we treat gam.m1 as base model, since it is the simplest
# model with linear effect of year is better than base model
# however there is no support for including splines for year

# We can also look into model with splines directly:
summary(gam.m3) # effect of year is not significant so our above counclusion is also supported

# As always we can make predictions:
preds <- predict(gam.m2, newdata=Wage)

# Apart from splines we can use LOESS as well inside GAM (spline for year and LOESS for age):
gam.lo <- gam(wage~s(year, df=4) + lo(age, span=0.7) + education, data=Wage)
par(mfrow=c(1,3))
plot.Gam(gam.lo, se=TRUE, col="green")

# Logistic regression using GAM
# by using operator I() we create binary dependant variable
gam.lr <- gam(I(wage>250)~year + s(age, df=5) + education, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")
# Let's change scale of y for first plot for better visibility
plot(gam.lr, se=T, col="green", ylim = c(-4,4))
# First category of education seems to have very wide standard errors, let's investigate: 
table(education, I(wage>250)) 
# no high earners in this category, we will skip it:
gam.lr.s <- gam(I(wage>250)~year+ s(age, df=5) + education, family=binomial, data=Wage, subset=(education!="1. < HS Grad"))
plot(gam.lr.s, se=T, col="green", ylim = c(-4,4))

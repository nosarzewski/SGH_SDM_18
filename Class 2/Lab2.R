# Statistical Learning Methods
# Lab 2 - Methods of evaluation of classifiers, Non-parametric regression models: splines
# materials: https://github.com/nosarzewski/SGH_SDM_18
# mail: nosarzewski.aleks@gmail.com, an56786@sgh.waw.pl


### Assessing classifiers
#Ex 1
rm(list=ls())
library(ROCR)

#reading data
LINES <- readLines("https://raw.githubusercontent.com/nosarzewski/SGH_SDM_18/master/Class%202/dictionary.txt")
SELECTED_LINES <- LINES[4:89]
LIST_OF_STRING_VECTORS <- strsplit(SELECTED_LINES, split = " ")
# if you get an error (ex. if you are a Mac user) please run instead:
# LIST_OF_STRING_VECTORS <- strsplit(SELECTED_LINES, split = " ", perl = TRUE)
GetSecondElement <- function(vector) {
  return(vector[2])
}
VARIABLE_NAMES <- sapply(LIST_OF_STRING_VECTORS, GetSecondElement)

RAW_DATA_SET <- read.delim("https://raw.githubusercontent.com/nosarzewski/SGH_SDM_18/master/Class%202/ticdata2000.txt", header = F)
names(RAW_DATA_SET) <- VARIABLE_NAMES
DATA_SET <- RAW_DATA_SET[, c(-1, -5)]

#To evaluate my classifier we need to have validation and training set 
#Training for model to learn the data
#Validation - observations unused, unnkown to the model

#Let's set seed for reproducibilty of outcomes
set.seed(1)

# 1st way - simple, straightforward
rand<-sample(1:nrow(DATA_SET),0.8*nrow(DATA_SET))
train<-DATA_SET[rand,]
val<-DATA_SET[-rand,]

# 2nd way as a function with fraction parameter
SplitDataSet <- function(data.set, training.fraction){
  random.numbers <- sample.int(nrow(data.set))
  quantiles <- quantile(random.numbers, probs = c(0, training.fraction, 1))
  split.labels <- cut(random.numbers, quantiles, include.lowest = T,
                      labels = c("training", "validation"))
  return(split(data.set, split.labels))
}

splitted.data.set <- SplitDataSet(DATA_SET, 0.7)
training.set<-splitted.data.set$training
validation.set<-splitted.data.set$validation

# To consider: 
# Is it a good approach? If we have a large dataset % of occurances in training and validation sets should be close to whole data set. 
# But maybe it is better to sample in such a way that proportion is exactly the same?
# Or quite the contrary - maybe we should resample our training set so that it has balanced number of 1's and 0's?

model1<-glm(CARAVAN~.,family="binomial",data=training.set)
predModel1<-predict(model1,newdata=validation.set,type="response")

# Confusion matrix
pred<-ifelse(predModel1>0.2,1,0)
conf_matrix<-table(validation.set$CARAVAN,pred)

# Accuracy - percentage of correct predictions 
# ACC = (TP + TN)/(TP + FP + TN + FN)
acc <- (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)

# Precision - percentage of positive predictions which were actually correct
# PREC = TP / (TP + FP)
prec <- conf_matrix[2,2]/sum(conf_matrix[,2])

# Recall - what percentage of actual positives were predicted correctly
# Recall / Sensitivity / Hit rate / True Positive Rate (TPR)
# REC = TP / (TP + FN)
rec <- conf_matrix[2,2]/sum(conf_matrix[2,])

# Specificity - what percentage of actual negatives were predicted correctly
# Specificity / True Negative Rate
# TNR = TN / (TN + FP)
tnr <- conf_matrix[1,1]/sum(conf_matrix[1,])

# caret package for automated calculations - functions: cunfusionMatrix (all-in-one), sensitivity, specificity, etc.

# There is no universal way of looking at these measures - always set your KPIs for a given (business) problem

# All above measures are case-specific - they measure performance of a model for particular threshold of probability
# Let's try measuring every variant of a given model - for every treshold from range (0,1)

# ROC curve & AUC measure
# Receiver Operating Characterictics shows TPR (Recall) and FPR for all thresholds
# AUC - Area Under the ROC Curve - the highr the better
# AUC interpretation:
# probability of our model ranking a random positive observation with higher probability than random negative observation
p<-prediction(predModel1,validation.set$CARAVAN) # calculates all basic measures for thresholds
str(p)

plot(performance(p,"tpr","fpr"),colorize=T) # performance calculates more complex measures based on basic ones
attr(performance(p, "auc"), "y.values")

# Gain curve 
# how much of people interested in buying caravan insurance will we cover if we contact x% of our client database
# in this case
plot(performance(p,"tpr","rpp"),lwd=2, colorize=T)

# Lift curve - how many times Gain of our model is higher than for a random model?
# It shows a profit form using a model vs not using a model and randomly guessing.
plot(performance(p,"lift","rpp"),lwd=2, colorize=T)

# Selecting variables
# 1 - based on correlation significance
MINIMUM_LEVEL <- 0.000001
selected.variables <- ncol(DATA_SET)

cor.test(training.set$MAANTHUI,training.set$CARAVAN)

for (i in 1:(ncol(DATA_SET) - 1)) {
  if (cor.test(training.set$CARAVAN,
               training.set[, i])$p.value < MINIMUM_LEVEL)
    selected.variables <- c(selected.variables, i)
}

predictions <- prediction.object <- roc  <- model <- list()
legend.label <- auc <- NULL
NAMES <- c("full", "selection")
model[[1]] <- glm(CARAVAN ~ ., family = "binomial",
                  data = training.set)
model[[2]] <- glm(CARAVAN ~ ., family = "binomial",
                  data = training.set[, selected.variables])

pdf("corVarSelect.pdf", encoding="CP1250.enc")
for (i in 1:length(model)) {
  predictions[[i]] <- predict(model[[i]], new = validation.set)
  prediction.object[[i]] <- prediction(predictions[[i]],
                                       validation.set$CARAVAN)
  roc[[i]] <- performance(prediction.object[[i]], "tpr", "fpr")
  auc[i] <- attr(performance(prediction.object[[i]], "auc"), "y.values")
  legend.label[i] <- paste(NAMES[i], "(AUC=", format(auc[i], digits = 4), ")",
                           sep = "")
  plot(roc[[i]], add = (i != 1), col = i + 1)
}
legend("bottomright", legend.label, col = 1 + (1:length(model)),
       title = "Models", lty = 1)
dev.off()

# 2 - based on information criterions
if (!require(Rcmdr)) {
  install.packages('Rcmdr')
  library(Rcmdr)
}
selection <- stepwise(model1, criterion = "BIC", direction = "forward/backward", trace = FALSE)
final_model <- eval(selection$call)


#Ex 2
# Adding cost matrix for selecting a treshold
rm(list=ls())
set.seed(1)
DATA_SET <- read.fwf("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric",widths = rep(4, 25), header = FALSE)
names(DATA_SET)[25] <- "target"
DATA_SET$target <- 2 - DATA_SET$target

SplitDataSet <- function(data.set, training.fraction){
  random.numbers <- sample.int(nrow(data.set))
  quantiles <- quantile(random.numbers, probs = c(0, training.fraction, 1))
  split.labels <- cut(random.numbers, quantiles, include.lowest = T,
                      labels = c("training", "validation"))
  return(split(data.set, split.labels))
}

CalculateCost <-function(cut.off, cost.matrix, score, true.y){
  prediction <- ifelse(score > cut.off, 1, 0)
  confusion.matrix <- prop.table(table(factor(prediction, levels = c(0, 1)),
                                       true.y))
  return(sum(cost.matrix * confusion.matrix))
}

set <- SplitDataSet(DATA_SET, 0.5)

score <- costs <- list()
model <- glm(target ~ ., data = set$training, family = "binomial")

CUT_OFFS <- seq(0.5, 1, by = 0.01) 
BAD_CREDIT_COST <- 5
LOST_CLIENT_COST <- 1
COST_MATRIX <- matrix(c(0, BAD_CREDIT_COST, LOST_CLIENT_COST, 0), 2)
score[[1]] <- predict(model, newdata = set$validation, type = "response")
costs[[1]] <- sapply(CUT_OFFS, CalculateCost, cost.matrix = COST_MATRIX,
                     score = score[[1]], true.y = set$validation$target)
score[[2]] <- predict(model, type = "response")
costs[[2]] <- sapply(CUT_OFFS, CalculateCost, cost.matrix = COST_MATRIX,
                     score = score[[2]], true.y = set$training$target)

plot(data.frame(CUT_OFFS, 0.7), type = "l", lty = 3, log = "y",
     ylim = range(c(0.7, unlist(costs))),
     ylab = "Cost per Client", xlab = "Cut-off")
for (i in 1:2) {
  lines(CUT_OFFS, costs[[i]], lty = i, lwd = 2)
  points(CUT_OFFS[which.min(costs[[i]])], min(costs[[i]]),
         pch = 19, cex = 1.3)
}

legend("topright", c("Validation", "Training", "NoModel"),
       lty = c(1, 2, 3), cex = .7, ncol = 3,
       lwd = c(2, 2, 1))

### Polynomial regression
library(ISLR)
data("Wage")

#orthogonal polynomials - each column is a linear orthogonal combination of the variables age, age^2, age^3 and age^4.
fit<-lm(wage~poly(age,4),data=Wage)
summary(fit)

#only exponentials
fit2<-lm(wage~poly(age,4,raw=T),data=Wage)
summary(fit2)

#the same as with raw=T
fit22<-lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(fit22)

#the same, only squares
fit3<-lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
summary(fit3)

#lets plot that
age.grid=seq(from=min(Wage$age), to=max(Wage$age))
preds=predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands=cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

par(mfrow=c(1,2), mar=c(4.5 ,4.5 ,1 ,1), oma=c(0,0,4,0))
plot(Wage$age, Wage$wage, xlim=c(min(Wage$age), max(Wage$age)), cex =.5, col =" darkgrey ")
title ("Degree -4 Polynomial", outer =T)
lines(age.grid, preds$fit, lwd =2, col =" blue")
matlines(age.grid, se.bands, lwd =1, col =" blue", lty =3)

#fit model only with squares
preds2 =predict(fit2, newdata =list(age=age.grid), se=TRUE)
se.bands2=cbind(preds2$fit + 2*preds2$se.fit, preds2$fit - 2*preds2$se.fit)

plot(Wage$age, Wage$wage, xlim=c(min(Wage$age), max(Wage$age)), cex =.5, col =" darkgrey ")
lines(age.grid,preds2$fit, lwd =2, col =" blue")
matlines(age.grid, se.bands2, lwd =1, col =" blue", lty =3)

#there is no difference in having only squares and all combination
round(max(abs(preds$fit - preds2$fit )),2)

#How many polynomials should I use?
fit.1= lm(wage~age ,data=Wage)
fit.2= lm(wage~poly(age,2) ,data=Wage)
fit.3= lm(wage~poly(age,3) ,data=Wage)
fit.4= lm(wage~poly(age,4) ,data=Wage)
fit.5= lm(wage~poly(age,5) ,data=Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary (fit.5))

### Splines

rm(list=ls())
library(splines)
data("Wage")
attach(Wage)

# Splines with knots specified explicite
fit <- lm(wage~bs(age, knots = c(25, 40, 60)), data=Wage)
age.grid=seq(from=min(Wage$age), to=max(Wage$age))
pred <- predict(fit, newdata = list(age=age.grid), se = TRUE)

# Let's plot our spline
par(mfrow=c(1,1))
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2, col = "blue")
lines(age.grid, pred$fit + 2*pred$se, lty="dashed", col = "dodgerblue3")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed", col = "dodgerblue3")
lines(rep(25, length(wage)), wage)
lines(rep(40, length(wage)), wage)
lines(rep(60, length(wage)), wage)

# Smoothing splines

# with tuning parameter set explicite through degrees of freedom
fit <- smooth.spline(age, wage, df=16)

# finding optimal tuning parameter through cross validation
fit2 <- smooth.spline(age, wage, cv=TRUE)

# check number of degrees of freedom for optimal tuning parameter
fit2$df

title("Different Splines")
lines(fit, col="red", lwd=2)
lines(fit2, col="green", lwd=2, lty=2)
legend("topright", legend=c("spline with knots","smooth spline df", "smooth spline CV"), 
       col=c("blue", "red", "green"), lty=c(1,1,2), lwd=2, cex=.8)


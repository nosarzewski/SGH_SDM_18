# Statistical Learning Methods
# Lab 4 - Classical machine learning modles: CART, ctree, random forest; 
#  extensions: extraTrees and XGBoost; Neural networks: nnet
# materials: https://github.com/nosarzewski/SGH_SDM_18
# mail: nosarzewski.aleks@gmail.com, an56786@sgh.waw.pl

# Loading data
set.seed(1)
DATA_SET <- read.csv("https://raw.githubusercontent.com/nosarzewski/SGH_SDM_18/master/Class%204/car.data.txt", 
                     header = FALSE)
names(DATA_SET) <- c("buying", "maint", "doors", "persons",
                     "lug_boot", "safety", "class")
DATA_SET$class <- factor(ifelse(DATA_SET$class == "unacc", 0, 1))

# Splitting data into training and validation set
TRAINING_SET_FRACTION <- 0.2
training.set.index <- (runif(nrow(DATA_SET)) < TRAINING_SET_FRACTION)
train.set <- DATA_SET[training.set.index, ]
test.set <- DATA_SET[!training.set.index, ]

# Decision trees

# Loading packages
if (!require(RColorBrewer)){
  install.packages('RColorBrewer')
  library(RColorBrewer)
}
# CART (Classification and Regression Trees)
if (!require(rpart)){
  install.packages('rpart')
  library(rpart)
}
# CART choses variables and split points by maximal change in selected measure (Gini index, entropy)
# fitting model
rpart.model <- rpart(class ~ ., train.set, cp = 0.00001, minsplit = 2)
plot(rpart.model, compress = T, uniform = T, margin = 0.1,
     branch = 0.3, nspace = 2)
text(rpart.model, use.n = TRUE, pretty = 0)
# left branch represents part of data set for whick criterion is met

# chosing optimal tree size for a tree with cross validation
plotcp(rpart.model)
minimum.error <- which.min(rpart.model$cptable[, "xerror"])
optimal.complexity <- rpart.model$cptable[minimum.error, "CP"]
points(minimum.error, rpart.model$cptable[minimum.error, "xerror"],
       col = "red", pch = 19)
# building pruned tree
pruned.tree <- prune(rpart.model, cp = optimal.complexity)
plot(pruned.tree, compress = T, uniform = T, margin = 0.1,
     branch = 0.3, nspace = 2)
text(pruned.tree, use.n = TRUE, pretty = 0)

# ctree
if (!require(party)){
  install.packages('party')
  library(party)
}
# ctree algorithm choses variables based on significance tests
# potential split points are randomly generated and optimal one is selected only from them
ctree.model <- ctree(factor(class) ~ ., data = train.set,
                     controls = ctree_control(mincriterion = 0.99,
                                              minsplit = 20))
plot(ctree.model, tnex = 2, type = "extended")
devAskNewPage(ask = TRUE)

# Random forests
if (!require(randomForest)){
  install.packages('randomForest')
  library(randomForest)
}

forest <- randomForest(class ~ ., data = train.set, ntree = 300)
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
plot(forest)
varImpPlot(forest, bg = 11)
plot(margin(forest, sort = TRUE), ylim = c(-1, 1), ylab = "margin")
abline(h = 0, lty = 2)

confusion.matrix <- list()
cat("Confusion matrix ctree")
print(confusion.matrix[[1]] <- table(predict(ctree.model, new = test.set),
                                     test.set$class))
cat("\nConfusion matrix rpart pruned\n")
print(confusion.matrix[[2]] <- table(predict(pruned.tree, type = "class",
                                             newdata = test.set),
                                     test.set$class))
cat("\nConfusion matrix random forest")
print(confusion.matrix[[3]] <- table(predict(forest, newdata = test.set),
                                     test.set$class))

cat("\nPorównanie dok?adno?ci modeli\n")
CalculateAccuracy <- function(confusion.matrix) {
  return(sum(diag(confusion.matrix)) / sum(confusion.matrix))
}
print(data.frame(model = c("ctree", "rpart pruned", "random forest"),
                 accuracy = sapply(confusion.matrix, CalculateAccuracy)), row.names = FALSE)

# extraTrees (Extremely Randomized Trees)
# this package uses Java - if you get an error you need to install Java manually
if (!require(extraTrees)){
  install.packages('extraTrees')
  library(extraTrees)
}
if (!require(caret)){
  install.packages('caret')
  library(caret)
}
# We need to tune our model parameters
cv_5 <- trainControl(method = "cv", number = 5) # CV options
et_grid <-  expand.grid(mtry = 4:7, numRandomCuts = 1:10) # number of variables and random cuts
et_fit <- train(class ~ ., data = train.set,
               method = "extraTrees",
               trControl = cv_5,
               tuneGrid = et_grid,
               numThreads = 4)
# our best tune
et_fit$bestTune
plot(et_fit)

# model accuracy
confusion.matrix[[4]] <- table(predict(et_fit, newdata = test.set), test.set$class)
print(data.frame(model = c("ctree", "rpart pruned", "random forest", "extraTree"),
                 accuracy = sapply(confusion.matrix, CalculateAccuracy)), row.names = FALSE)

# Gradient Boosting (XGBoost)
if (!require(xgboost)){
  install.packages('xgboost')
  library(xgboost)
}
# fitting model
xgb_fit <- train(class ~ ., data = train.set,
                method = "xgbTree",
                trControl = cv_5,
                verbose = FALSE,
                tuneLength = 10)

# Finding best tuning
xgb_fit$bestTune

# Comparison to other methods (accuracy)
confusion.matrix[[5]] <- table(predict(xgb_fit, newdata = test.set), test.set$class)
print(data.frame(model = c("ctree", "rpart pruned", "random forest", "extraTree", "XGBoost"),
                 accuracy = sapply(confusion.matrix, CalculateAccuracy)), row.names = FALSE)

# Neural networks (nnet)
if (!require(nnet)){
  install.packages('nnet')
  library(nnet)
}

# Load data
rm(list=ls())
DATA_SET <- read.table("https://raw.githubusercontent.com/nosarzewski/SGH_SDM_18/master/Class%204/housing.data")
names(DATA_SET) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE",
                     "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")
DATA_SET <- DATA_SET[-2]

# Prepare data (scaling and splitting)
SCALED_DATA <- scale(DATA_SET[-ncol(DATA_SET)])
FINAL_DATA <- cbind(SCALED_DATA, DATA_SET[ncol(DATA_SET)])
TRAIN_PART <- VALID_PART <- 0.4
random.numbers <- sample.int(nrow(FINAL_DATA))
quantiles <- quantile(random.numbers,
                      prob = c(0, TRAIN_PART, TRAIN_PART + VALID_PART, 1))
split.labels <- cut(random.numbers, quantiles, include.lowest = T,
                    labels=c("train", "valid", "test"))
data <- split(FINAL_DATA, split.labels)

# Prepare parameters for neural network
NEURONS <- 5 #number of neurons in hidden layer
DECAYS <- seq(0, 40, length.out = 100) # values of decay to test
wts.parameter <-  2 * runif(NEURONS * ncol(FINAL_DATA) + NEURONS + 1) - 1 # weights (not necessary, only for comparability)
train.error <- valid.error <- numeric(length(DECAYS)) # variables to store errors
neural.nets <- list() # list for storing nnets 

# Tuning nnet
for (d in 1:length(DECAYS)){
  neural.nets[[d]] <- nnet(MEDV ~ ., data = data$train, size = NEURONS,
                           decay = DECAYS[d], linout = T, maxit = 10000,
                           trace = FALSE, Wts = wts.parameter)
  train.error[d] <- mean(neural.nets[[d]]$residuals ^ 2)
  prediction <- predict(neural.nets[[d]], newdata = data$valid)
  valid.error[d] <- mean((prediction - data$valid$MEDV) ^ 2)
}

best.neural.net <- neural.nets[[which.min(valid.error)]]
test.prediction <- predict(best.neural.net, newdata = data$test)
best.net.test.error <- mean((test.prediction - data$test$MEDV) ^ 2)

# Ordinary Least Squares (OLS) for comparison
ols <- lm(MEDV ~ ., data = data$train)
ols.train.error <- mean(ols$residuals ^ 2)
prediction <- predict(ols, newdata = data$valid)
ols.valid.error <- mean((prediction - data$valid$MEDV) ^ 2)
prediction <- predict(ols, newdata = data$test)
ols.test.error <- mean((prediction - data$test$MEDV) ^ 2)

par(mfrow = c(1,1))
devAskNewPage(ask = FALSE)
plot(DECAYS, train.error, "l", ylim = range(c(train.error, valid.error)),
     lwd = 2, col = "red", xlab = "Parametr decay", ylab = "MSE")
lines(DECAYS, valid.error, "l", col = "blue", lwd = 2)
points(DECAYS[which.min(valid.error)], min(valid.error),
       pch = 19, col = "blue", cex = 1.5)
points(DECAYS[which.min(valid.error)], best.net.test.error,
       pch = 19, col = "green", cex = 1.5)
abline(h = ols.train.error, col = "red", lty = 2)
abline(h = ols.valid.error, col = "blue", lty = 2)
abline(h = ols.test.error, col = "green", lty = 2)
legend("bottomright", lty = c(1, 1, NA, 2, 2, 2), lwd = c(2, 2, NA, 1, 1, 1),
       col = c("red", "blue", "green"), pch = c(NA, NA, 19, NA, NA, NA),
       y.intersp = 0.7, ncol = 2,
       legend = c("Net train", "Net valid", "Net test",
                  "OLS train", "OLS valid", "OLS test"))                                                    
devAskNewPage(ask = TRUE)

# Marginal effects for variables
# blue - nnet
# red - OLS
par(mfrow = c(4, 3), mar = c(2.5, 2.5, 2, 1))
zeros <- data.frame(matrix(0, ncol = ncol(DATA_SET) - 1, nrow = 100))
names(zeros) <- names(DATA_SET[-ncol(DATA_SET)])  
for(j in 1:ncol(zeros)){
  x.change <- zeros
  x.change[, j] <- seq(-3, 3, length.out = 100)
  prediction <- predict(best.neural.net, newdata = x.change)
  plot(x.change[, j][-100], diff(prediction) / (6 / 99), ylim = c(-10, 10),
       "l", lwd = 2, main = paste(names(DATA_SET)[j]), col = "blue")
  abline(h = 0, lty = 2)
  abline(h = ols$coefficients[j + 1], lty = 2, col = "red", lwd = 2)
}


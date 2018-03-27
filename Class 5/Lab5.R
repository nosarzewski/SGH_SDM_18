# Statistical Learning Methods
# Lab  -  Neural networks continued: MXNet
# materials: https://github.com/nosarzewski/SGH_SDM_18
# mail: nosarzewski.aleks@gmail.com, an56786@sgh.waw.pl

# Classification with MXNet

# Library with dataset
if (!require(mlbench)){
  install.packages('mlbench')
  library(mlbench)
}

data(Sonar, package = "mlbench")

# Library MXNet
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
library(mxnet)

# if you get following error:
# object ‘set_global_graph_attrs’ is not exported from 'namespace:DiagrammeR'
require(devtools)
install_version("DiagrammeR", version = "0.9.0", repos = "http://cran.us.r-project.org")
require(DiagrammeR)
install.packages("mxnet")
library(mxnet)

# Let's prepare datasets
Sonar[,61] <- as.numeric(Sonar[,61]) - 1 # dependent variable
# Training and validation sets:
train.ind <- c(1:50, 100:150)
train.x <- data.matrix(Sonar[train.ind, 1:60])
train.y <- Sonar[train.ind, 61]
test.x <- data.matrix(Sonar[-train.ind, 1:60])
test.y <- Sonar[-train.ind, 61]

mx.set.seed(0)
model <- mx.mlp(train.x, train.y, hidden_node=10, out_node=2, out_activation="softmax",
                num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9,
                eval.metric=mx.metric.accuracy)
# hidden_node - number ofnodes for each hidden layer
# out_node - number of output nodes
# out_activation - activation function
# num.round - number of iterations
# array.batch.size - batch size (number of observations from training set) for each iteration
# learning.rate - parameter for speed of learning
# momentum - for optoimization purposes - the highest value the less random search
# eval.metric - metric used for evaluation

preds <- predict(model, test.x)
pred.label <- max.col(t(preds))-1
table(pred.label, test.y)

# We can define neural network manually
data <- mx.symbol.Variable("data") #input data
fc1 <- mx.symbol.FullyConnected(data, name = "fc1", num_hidden=10) # define 1st hidden layer
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu") # define activation
fc2 <- mx.symbol.FullyConnected(act1, name = "fc2", num_hidden=5) # define 2nd hidden layer
softmax <- mx.symbol.SoftmaxOutput(fc2, name="sm") #define output layer

model2 <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y,
                                     ctx=mx.cpu(), num.round=10, array.batch.size=100,
                                     learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                     initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100))

# Regression with MXNet
rm(list=ls())
data(BostonHousing, package="mlbench")

train.ind <- seq(1, 506, 3)
train.x <- data.matrix(BostonHousing[train.ind, -14])
train.y <- BostonHousing[train.ind, 14]
test.x <- data.matrix(BostonHousing[-train.ind, -14])
test.y <- BostonHousing[-train.ind, 14]

# We will create net manually
data <- mx.symbol.Variable("data") #input data
fc1 <- mx.symbol.FullyConnected(data, num_hidden=1) # define hidden layer
lro <- mx.symbol.LinearRegressionOutput(fc1) #define output layer

mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, X=train.x, y=train.y,
                                     ctx=mx.cpu(),     num.round=50, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9,  eval.metric=mx.metric.rmse)
# Forecasting for validation
preds <- predict(model, test.x)
sqrt(mean((preds-test.y)^2))


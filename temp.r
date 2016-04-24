# intializing environment -------------------------------------------------

# http://datascienceplus.com/perform-logistic-regression-in-r/
library(pscl) # install.packages("pscl")
library(caret) # install.packages("caret")
library(ROCR) # install.packages("ROCR")
library(dplyr) # install.packages("dplyr")
library(pROC) # install.packages("pROC")

setwd("D:\\Reps\\gopath\\src\\github.com\\WiseBird\\kaggle_titanic")
#setwd("C:\\Users\\sergey.sokolov\\Documents\\projects_\\kaggle_titanic")
rm(list = ls())
cat("\014") 

source("helpers.R")
source("processing.R")
source("approachs.base.R")
source("approachs.regression.R")
source("approachs.manual.R")

calcAuc <- function(training, testing = training) {
  fit <- train(Survived ~ ., data = training, method = "glm")
  print(getTrainPerf(fit))
  
  predProb <- caret::predict.train(fit, newdata = testing, type = "prob")
  predRaw <- caret::predict.train(fit, newdata = testing, type = "raw")
  
  print(confusionMatrix(predRaw, testing$Survived))
  
  roc <- roc(testing$Survived,
             predProb[,1],
             levels = rev(testing$Survived))
  print(roc)
  
  plot(roc, print.thres = c(.5), type = "S",
       print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
       print.thres.cex = .8,
       legacy.axes = TRUE)
}




# Loading data ------------------------------------------------------------

titanic <- read.titanic()
titanic <- regression.simpliest$transform(titanic)
split.res <- split.test.train(titanic)

calcAuc(titanic)
calcAuc(split.res$training, split.res$testing)












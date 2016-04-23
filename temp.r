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


# Loading data ------------------------------------------------------------

titanic <- read.titanic()
titanic <- regression.simpliest$transform(titanic)
split.res <- split.test.train(titanic)

glmTitanic <- train(Survived ~ ., data = titanic, method = "glm")
getTrainPerf(glmTitanic)

glmTraining <- train(Survived ~ ., data = split.res$training, method = "glm")
getTrainPerf(glmTraining)

predProbTitanic <- caret::predict.train(glmTitanic, newdata = titanic, type = "prob")
predRawTitanic <- caret::predict.train(glmTitanic, newdata = titanic, type = "raw")

confusionMatrix(predRawTitanic, titanic$Survived)




predictions <- regression.simpliest$predict(split.res$training, split.res$testing)
predictions

roc0 <- roc(split.res$testing$Survived,
            predictions,
            levels = rev(levels(split.res$testing$Survived)))
roc0

plot(roc0, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)

#confusionMatrix(predictions, split.res$testing$Survived)
confusionMatrix(ifelse(predictions < 0.5, "1", "0"), split.res$testing$Survived)

pred <- prediction(as.numeric(predictions), split.res$testing$Survived)
auc <- performance(pred,"auc")
auc
score.classifier(split.res, regression.simpliest)
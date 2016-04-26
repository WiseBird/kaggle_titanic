# intializing environment -------------------------------------------------

# http://datascienceplus.com/perform-logistic-regression-in-r/
library(pscl) # install.packages("pscl")
library(caret) # install.packages("caret")
library(ROCR) # install.packages("ROCR")
library(dplyr) # install.packages("dplyr")
library(pROC) # install.packages("pROC")
library(e1071) # install.packages("e1071")

#setwd("D:\\Reps\\gopath\\src\\github.com\\WiseBird\\kaggle_titanic")
setwd("C:\\Users\\sergey.sokolov\\Documents\\projects_\\kaggle_titanic")
rm(list = ls())
cat("\014") 

source("helpers.R")
source("processing.R")
source("approachs.base.R")
source("approachs.regression.R")
source("approachs.manual.R")


# Loading data ------------------------------------------------------------

titanic <- read.titanic()
split.res <- split.test.train(titanic)

# Analysing/testing -------------------------------------------------------

calc.auc(list(training = titanic, testing = titanic), regression.simpliest)
calc.auc(split.res, regression.simpliest, T)

scores <- cross.validate.k(titanic, 
                              regression.simpliest,
                              regression.by.sex)
compare.approaches(scores)


scores <- cross.validate.k(titanic, stat=calc.log.regr.cost,
                           regression.simpliest,
                           regression.by.sex)
compare.approaches(scores)



regression.simpliest$details(split.res$training, split.res$testing)
regression.by.sex$details(split.res$training, split.res$testing)



# dummy


# Completed approaches ----------------------------------------------------

score.approach <- calc.log.regr.cost

score.approach(split.res, regression.simpliest)
score.approach(split.res, regression.age.sex)
score.approach(split.res, regression.age.sex.and.pclass)
score.approach(split.res, regression.flare.cut.auto)
score.approach(split.res, regression.by.sex)
score.approach(split.res, regression.by.sex.and.child)
score.approach(split.res, regression.by.sex.and.pclass)
score.approach(split.res, regression.by.sex.and.fare)
score.approach(split.res, regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child)
#calc.auc(split.res, manual.by.sex.and.fare.and.pclass.and.child)
#calc.auc(split.res, manual.age.cut.manual.by.sex.fare.pclass.age)




create.submit(regression.simpliest, titanic, "regression.simpliest") # 0.76077
create.submit(regression.age.sex, titanic, "regression.age.sex")
create.submit(regression.age.sex.and.pclass, titanic, "regression.age.sex.and.pclass")
create.submit(regression.flare.cut.auto, titanic, "regression.flare.cut.auto")
create.submit(regression.by.sex, titanic, "regression.by.sex") # 0.76555
create.submit(regression.by.sex.and.child, titanic, "regression.by.sex.and.child") # 0.76555
create.submit(regression.by.sex.and.pclass, titanic, "regression.by.sex.and.pclass") # 0.76555
create.submit(regression.by.sex.and.fare, titanic, "regression.by.sex.and.fare") # 0.76077
create.submit(regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child, titanic, "regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child") # 0.76555
#create.submit(manual.by.sex.and.fare.and.pclass.and.child, titanic, "manual.by.sex.and.fare.and.pclass.and.child") # 0.78469
#create.submit(manual.age.cut.manual.by.sex.fare.pclass.age, titanic, "manual.age.cut.manual.by.sex.fare.pclass.age") # 0.77033


# Trash -------------------------------------------------------------------

titanic$Title <- sapply(titanic$Name, FUN=function(x) {strsplit(x, split = "[,.]")[[1]][2]})
titanic$Title <- sub(' ', '', titanic$Title)
titanic$Title[titanic$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
titanic$Title[titanic$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
titanic$Title[titanic$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
titanic$Title[titanic$Title %in% c('Col', 'Lady', 'Dr', 'Jonkheer', 'Master', 'Mlle', 'Ms', 'Rev', "Sir")] <- 'High'
titanic$Title <- factor(titanic$Title)
table(titanic$Title)

titanic$Name <- NULL


#mean(unlist(replicate(300, calculate_auc())))


# analysis

aggregate(Survived ~ Sex, data = titanic, FUN = function(x) { sum(x == 1)/length(x) })

titanic$Child <- factor("Adult", levels = c("Child", "Adult"))
titanic$Child[titanic$Age < 18] <- "Child"
aggregate(Survived ~ Child + Sex, data = titanic, FUN = function(x) { sum(x == 1)/length(x) })
# nothing interesting

mean(unlist(replicate(300, calculate_auc())))

titanic$FamilySize <- titanic$SibSp + titanic$Parch + 1
titanic$SibSp <- NULL
titanic$Parch <- NULL

mean(unlist(replicate(300, calculate_auc())))

aggregate(Survived ~ Fare + Pclass  + Sex, data = titanic, FUN = function(x) { sum(x == 1)/length(x) })
# Fare - 30+, Pclass - 3. Very low rate.



#anova(model, model2, test="Chisq")
#titanic$CabinLevel <- factor(substring(titanic$Cabin, 1, 1))

# intializing environment -------------------------------------------------

# http://datascienceplus.com/perform-logistic-regression-in-r/
library(pscl) # install.packages("pscl")
library(caret) # install.packages("caret")
library(ROCR) # install.packages("ROCR")
library(dplyr) # install.packages("dplyr")
library(pROC) # install.packages("pROC")
library(e1071) # install.packages("e1071")
library(ggplot2) # install.packages("ggplot2")
library(rpart) # install.packages('rpart')
library(rattle) # install.packages('rattle')
library(rpart.plot) # install.packages('rpart.plot')
library(RColorBrewer) # install.packages('RColorBrewer')

setwd("D:\\Reps\\gopath\\src\\github.com\\WiseBird\\kaggle_titanic")
#setwd("C:\\Users\\sergey.sokolov\\Documents\\projects_\\kaggle_titanic")
rm(list = ls())
cat("\014") 

source("helpers.R")
source("processing.R")
source("approachs.base.R")
source("approachs.regression.R")
source("approachs.manual.R")
source("approachs.rpart.R")


# Loading data ------------------------------------------------------------

titanic <- read.titanic()
split.res <- split.test.train(titanic)

# Analysing/testing -------------------------------------------------------

regression.simpliest$details(split.res$training, split.res$testing)
regression.by.sex$details(split.res$training, split.res$testing)
rpart.simpliest$details(split.res$training, split.res$testing)
rpart.by.sex$details(split.res$training, split.res$testing)
rpart.age.cut$details(split.res$training, split.res$testing)

scores <- cross.validate.k(titanic, 
                           regression.simpliest,
                           regression.by.sex,
                           rpart.simpliest,
                           rpart.by.sex)
compare.approaches(scores)

scores <- cross.validate.k(titanic, stat=calc.kappa,
                           regression.simpliest,
                           regression.by.sex,
                           rpart.simpliest,
                           rpart.by.sex)
compare.approaches(scores)

scores <- cross.validate.k(titanic, stat=calc.accuracy,
                           regression.simpliest,
                           regression.by.sex,
                           rpart.simpliest,
                           rpart.by.sex)
compare.approaches(scores)



regression.simpliest$details(split.res$training, split.res$testing)
regression.by.sex$details(split.res$training, split.res$testing)



# Completed approaches ----------------------------------------------------

# small helper to simplify scoring
score.approach <- function(approach) {
  score <- cross.validate.k(titanic, 
                            k = 3,
                            #stat=calc.log.regr.cost,
                            approach);
  mean(score[, 1])
}

score.approach(regression.simpliest)
score.approach(regression.age.sex)
score.approach(regression.age.sex.and.pclass)
#score.approach(regression.flare.cut.auto)
score.approach(regression.by.sex)
score.approach(regression.by.sex.and.child)
score.approach(regression.by.sex.and.pclass)
score.approach(regression.by.sex.and.fare)
score.approach(regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child)

score.approach(manual.by.sex.and.fare.and.pclass.and.child)
score.approach(manual.age.cut.manual.by.sex.fare.pclass.age)

score.approach(rpart.simpliest)
score.approach(rpart.by.sex)
score.approach(rpart.by.sex.and.child)
score.approach(rpart.by.sex.and.pclass)
score.approach(rpart.by.sex.and.fare)
score.approach(rpart.age.na.sex)
score.approach(rpart.age.na.sex.and.pclass)
score.approach(rpart.age.cut)


create.submit(titanic, "regression.simpliest") # 0.76077
create.submit(titanic, "regression.age.sex")
create.submit(titanic, "regression.age.sex.and.pclass")
create.submit(titanic, "regression.flare.cut.auto")
create.submit(titanic, "regression.by.sex") # 0.76555
create.submit(titanic, "regression.by.sex.and.child") # 0.76555
create.submit(titanic, "regression.by.sex.and.pclass") # 0.76555
create.submit(titanic, "regression.by.sex.and.fare") # 0.76077
create.submit(titanic, "regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child") # 0.76555

create.submit(titanic, "manual.by.sex.and.fare.and.pclass.and.child") # 0.78469
create.submit(titanic, "manual.age.cut.manual.by.sex.fare.pclass.age") # 0.77033

create.submit(titanic, "rpart.simpliest") # 0.78469
create.submit(titanic, "rpart.by.sex") # 0.76555
create.submit(titanic, "rpart.by.sex.and.child") # 0.76555
create.submit(titanic, "rpart.by.sex.and.pclass") # 0.76555
create.submit(titanic, "rpart.by.sex.and.fare") # 0.77512
create.submit(titanic, "rpart.age.na.sex") # 0.78469
create.submit(titanic, "rpart.age.na.sex.and.pclass") # 0.78469
create.submit(titanic, "rpart.age.cut") # 0.78469


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

# intializing environment -------------------------------------------------

# http://datascienceplus.com/perform-logistic-regression-in-r/
library(pscl) # install.packages("pscl")
library(caret) # install.packages("caret")
library(ROCR) # install.packages("ROCR")
library(dplyr) # install.packages("dplyr")

setwd("D:\\Reps\\gopath\\src\\github.com\\WiseBird\\kaggle_titanic")
#setwd("C:\\Users\\пк\\Google Диск\\Tasks\\titanic")
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

par(mfrow=c(1,1))
scores <- compare.approaches(titanic, n = 50, 
                   regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child,
                   manual.by.sex.and.fare.and.pclass.and.child)

scores = list(one = scores[,1], two = scores[,2])

par(mfrow=c(1,2))
hist(scores[[1]])
hist(scores[[2]])

t.test(scores$one, scores$two)


regression.simpliest$details(titanic)
regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child$details(titanic)



# dummy


# Completed approaches ----------------------------------------------------

score.classifier(split.res, regression.simpliest) # 0.76077
score.classifier(split.res, regression.age.sex)
score.classifier(split.res, regression.age.sex.and.pclass)
score.classifier(split.res, regression.flare.cut.auto)
score.classifier(split.res, regression.by.sex) # 0.76555
score.classifier(split.res, regression.by.sex.and.child) # 0.76555
score.classifier(split.res, regression.by.sex.and.pclass) # 0.76555
score.classifier(split.res, regression.by.sex.and.fare) # 0.76077
score.classifier(split.res, regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child) # 0.76555
score.classifier(split.res, manual.by.sex.and.fare.and.pclass.and.child) # 0.78469
score.classifier(split.res, manual.age.cut.manual.by.sex.fare.pclass.age) # 0.77033




create.submit(regression.simpliest, titanic, "regression.simpliest")
create.submit(regression.age.sex, titanic, "regression.age.sex")
create.submit(regression.age.sex.and.pclass, titanic, "regression.age.sex.and.pclass")
create.submit(regression.flare.cut.auto, titanic, "regression.flare.cut.auto")
create.submit(regression.by.sex, titanic, "regression.by.sex")
create.submit(regression.by.sex.and.child, titanic, "regression.by.sex.and.child")
create.submit(regression.by.sex.and.pclass, titanic, "regression.by.sex.and.pclass")
create.submit(regression.by.sex.and.fare, titanic, "regression.by.sex.and.fare")
create.submit(regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child, titanic, "regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child")
create.submit(manual.by.sex.and.fare.and.pclass.and.child, titanic, "manual.by.sex.and.fare.and.pclass.and.child")
create.submit(manual.age.cut.manual.by.sex.fare.pclass.age, titanic, "manual.age.cut.manual.by.sex.fare.pclass.age")


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

# intializing environment -------------------------------------------------

# http://datascienceplus.com/perform-logistic-regression-in-r/
library(caret) # install.packages("caret", dependencies = TRUE)
library(pscl) # install.packages("pscl", dependencies = TRUE)
library(ROCR) # install.packages("ROCR", dependencies = TRUE)
library(plyr)
library(dplyr) # install.packages("dplyr", dependencies = TRUE)
library(rattle) # install.packages("rattle", dependencies = TRUE)
library(C50) # install.packages("C50", dependencies = TRUE)
library(mice) # install.packages("mice", dependencies = TRUE)
library(yaImpute) # install.packages("yaImpute", dependencies = TRUE)

#setwd("D:\\Reps\\gopath\\src\\github.com\\WiseBird\\kaggle_titanic")
#setwd("C:\\Users\\sergey.sokolov\\Documents\\projects_\\kaggle_titanic")
setwd("D:\\repositories\\ExternalCode\\src\\github.com\\WiseBird\\kaggle_titanic")
rm(list = ls())
cat("\014") 

source("helpers.R")
source("processing.R")
source("approachs.base.R")
source("approachs.regression.R")
source("approachs.manual.R")
source("approachs.rpart.R")
source("approachs.c50.R")
source("approachs.cforest.R")

# Loading data ------------------------------------------------------------

titanic <- read.titanic()
split.res <- split.test.train(titanic)

# Analysing/testing -------------------------------------------------------

regression.simpliest$details(split.res$training, split.res$testing)
regression.by.sex$details(split.res$training, split.res$testing)
regression.by.sex.and.pclass$details(split.res$training, split.res$testing)

rpart.simpliest$details(split.res$training, split.res$testing)
rpart.by.sex$details(split.res$training, split.res$testing)
rpart.overfitted$details(split.res$training, split.res$testing)

c50.simpliest$details(split.res$training, split.res$testing)
c50.by.sex.pclass.fare.age$details(split.res$training, split.res$testing)
c50.by.sex.pclass.fare.age.embarked$details(split.res$training, split.res$testing)

compare.approaches(titanic,
                   cv.k.folds,
                   stat=calc.kappa,
                   regression.simpliest,
                   regression.add.title,
                   regression.add.familySize,
                   regression.add.title.familySize)

compare.approaches(titanic,
                   cv.k.folds,
                   stat=calc.kappa,
                   rpart.add.title,
                   rpart.add.title.familySize,
                   cforest.add.title,
                   cforest.add.title.familySize)

compare.approaches(titanic,
                   cv.k.folds,
                   stat=calc.kappa,
                   regression.add.title,
                   rpart.add.title,
                   c50.add.title)


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


create.submit(titanic, "regression.simpliest") # 0.76077 scale 0.76555
create.submit(titanic, "regression.age.sex")
create.submit(titanic, "regression.age.sex.and.pclass")
create.submit(titanic, "regression.flare.cut.auto")
create.submit(titanic, "regression.by.sex") # 0.76555
create.submit(titanic, "regression.by.sex.and.child") # 0.76555
create.submit(titanic, "regression.by.sex.and.pclass") # 0.76555
create.submit(titanic, "regression.by.sex.and.fare") # 0.76077
create.submit(titanic, "regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child") # 0.76555
create.submit(titanic, "regression.add.title") # 0.77512 scale 0.78947
create.submit(titanic, "regression.add.title.familySize") # 0.78947

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
create.submit(titanic, "rpart.add.title") # 0.78947
create.submit(titanic, "rpart.add.title.familySize") # 0.77990

create.submit(titanic, "c50.simpliest") # 0.78469
create.submit(titanic, "c50.by.sex") # 0.76555
create.submit(titanic, "c50.by.sex.and.fare") # 0.76555
create.submit(titanic, "c50.by.sex.and.pclass") # 0.76555
create.submit(titanic, "c50.by.sex.pclass.fare.age") # 0.77512
create.submit(titanic, "c50.by.sex.pclass.fare.age.embarked") # 0.77512
create.submit(titanic, "c50.add.title") # 0.75120

create.submit(titanic, "cforest.add.title.familySize") # 0.79904
create.submit(titanic, "cforest.add.title") # 0.77990

# Visualizations ------------------------------------------------------------

df <- titanic

# Missing embarkeds has fare = 80
df[is.na(df$Embarked), c("Name", "Pclass", "Ticket", "Fare")]

# 80 is close to C's median
ggplot(df, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=1) +
  scale_y_continuous()



# add new feature
df$Family.Size <- df$SibSp + df$Parch + 1

# looks like families of size 2 to 4 are more survivable
ggplot(df, aes(x = Family.Size, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=seq(max(df$Family.Size))) +
  labs(x = 'Family Size', fill='Survived')



# compare how age imputation affects distribution
compare.age.distribution <- function(age.imputation.func) {
  age.original = df[!is.na(df$Age), c("Age"), drop = F] %>%
    mutate(AgeType = "Age (original)")
  
  age.imputed = age.imputation.func(df)[is.na(df$Age), c("Age"), drop = F] %>%
    mutate(AgeType = "Age (imputed)")
  
  age.type <- bind_rows(age.original, age.imputed)
  
  ggplot(age.type, aes(x = Age)) +
    geom_density(aes(fill = factor(AgeType)), alpha = 0.6) +
    #scale_fill_manual(values = c(alpha("green", 0.25), alpha("blue", 0.4))) +
    labs(x = 'Age', fill = '')
}

compare.age.distribution(tf.na.age.mean$prepare(df))
compare.age.distribution(tf.na.age.mean.by.sex$prepare(df))
compare.age.distribution(tf.na.age.mean.by.sex.and.pclass$prepare(df))
compare.age.distribution(mice_impute)
compare.age.distribution(caret_impute)
compare.age.distribution(tf.na.age.yai.ica$prepare(df))

mice_impute <- function(df) {
  complete(mice(df[,c("Pclass","Sex","SibSp","Parch","Embarked","Age","Fare")], method="rf"))
} 

caret_impute <- function(df) {
  df$Sex = as.integer(df$Sex)
  df$Pclass = as.integer(df$Pclass)
  preProcValues_x <- preProcess(df[,c("Sex", "Pclass", "Age", "Fare", "SibSp", "Parch")], method = c("center", "scale", "knnImpute"))
  predict(preProcValues_x, df[,c("Sex", "Pclass", "Age", "Fare", "SibSp", "Parch")])
}


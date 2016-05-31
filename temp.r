library(yaImpute) # install.packages("yaImpute", dependencies = TRUE)

data(iris)
# set the random number seed so that example results are consistent
# normally, leave out this command
set.seed(12345)
# form some test data, s are defined only for reference
# observations.
refs=sample(rownames(iris),50)
x <- iris[,1:2]      # Sepal.Length Sepal.Width

y <- iris[refs,3:4]  # Petal.Length Petal.Width
# build yai objects using 2 methods
msn <- yai(x=x,y=y)
mal <- yai(x=x,y=y,method="mahalanobis")
# compare these results using the generalized mean distances. mal wins!
grmsd(mal,msn)

z <- predict(mal, iris)

head(iris)
head(z)
head(y)
refs[order(refs)]


rownums <- which(!is.na(split.res$training$Age))

head(split.res$training)
tail(split.res$training)

x <- split.res$training[, c("Fare", "SibSp", "Parch"), drop = F]
x_1 <- split.res$training[, c("Pclass","Sex", "Fare", "SibSp", "Parch"), drop = F]
y <- split.res$training[rownums, c("Age"), drop = F]

msn <- yai(x = x, y = y)
mal <- yai(x = x, y = y, method = "mahalanobis")
rf <- yai(x = x_1, y = y, method = "randomForest")
ica <- yai(x = x, y = y, method = "ica")

grmsd(mal,msn, rf, ica)

a <- predict(rf, split.res$training[is.na(split.res$training$Age),])
head(a)
a <- predict(rf, split.res$testing[is.na(split.res$testing$Age), ])
head(a)
head(split.res$testing, 30)



library(mlr) # install.packages("mlr", dependencies = TRUE)

imp = impute(titanic, target = "Survived", classes = list(integer = imputeMean(), factor = imputeMode()))





training <- split.res$training[, c("Age", "Fare", "SibSp", "Parch")]
testing <- split.res$testing[, c("Age", "Fare", "SibSp", "Parch")]

preProcValues <- preProcess(training, method = c("center", "scale"))
x <- predict(preProcValues, training)

head(training)
head(x)

y <- tf.na.age.mean$prepare(training)(training)
head(y)
preProcValues_y <- preProcess(y, method = c("center", "scale"))
z <- predict(preProcValues_y, y)

preProcValues_x <- preProcess(training, method = c("center", "scale", "knnImpute"))
a <- predict(preProcValues_x, training)

head(z)
head(a)

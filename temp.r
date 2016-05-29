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

predict(mal, iris)

# use projection pursuit and specify ppControl (loads package ccaPP)
if (require(ccaPP))
{
  msnPP <- yai(x=x,y=y,method="msnPP",ppControl=c(method="kendall",search="proj"))
  grmsd(mal,msnPP,msn)
}


z <- yai(
  x = split.res$training[, c("Survived"), drop = F], 
  y = split.res$training[, c("Pclass","Sex","SibSp","Parch","Embarked","Age","Fare")],
  method = "randomForest")
predict(z, split.res$training)



library(mlr) # install.packages("mlr", dependencies = TRUE)

imp = impute(titanic, target = "Survived", classes = list(integer = imputeMean(), factor = imputeMode()))

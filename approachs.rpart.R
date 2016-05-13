train.rpart <- function(formula, df) {
  train(formula, df, method = "rpart")#, cp=0.002, maxdepth=8)
}

rpart.base <- approach.create.from(
  approach.caret.base,
  train.func = function(titanic) {
    train.rpart(Survived ~ ., titanic)
  },
  details.func = function(model, testing) {
    approach.caret.base$details.func(model, testing)
    
    fancyRpartPlot(model$finalModel)
  })

rpart.simpliest <- approach.create.from(
  rpart.base,
  tf.remove.name,
  tf.na.age.mean)

rpart.by.sex <- approach.create.from(
  rpart.simpliest,
  train.func = function(titanic) {
    train.rpart(Survived ~ Sex, titanic)
  })

rpart.by.sex.and.child <- approach.create.from(
  rpart.simpliest,
  tf.add.child,
  train.func = function(titanic) {
    train.rpart(Survived ~ Sex + Child, titanic)
  })

rpart.by.sex.and.pclass <- approach.create.from(
  rpart.simpliest,
  train.func = function(titanic) {
    train.rpart(Survived ~ Sex + Pclass, titanic)
  })


rpart.by.sex.and.fare <- approach.create.from(
  rpart.simpliest,
  train.func = function(titanic) {
    train.rpart(Survived ~ Sex + Fare, titanic)
  })


rpart.age.na.sex <- approach.create.from(
  rpart.base,
  tf.remove.name,
  tf.na.age.mean.by.sex)

rpart.age.na.sex.and.pclass <- approach.create.from(
  rpart.base,
  tf.remove.name,
  tf.na.age.mean.by.sex.and.pclass)


rpart.age.cut <- approach.create.from(
  rpart.simpliest,
  tf.age.cut.manual)
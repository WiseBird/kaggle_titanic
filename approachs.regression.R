train.regression <- function(formula, df) {
  train(formula, data = df, method = "glm")
}

regression.base <- approach.create.from(
  approach.caret.base,
  train.func = function(titanic) {
    train.regression(Survived ~ ., titanic)
  })

regression.simpliest <- approach.create.from(
  regression.base,
  tf.remove.name,
  tf.na.age.mean)

regression.age.sex <- approach.create.from(
  regression.base,
  tf.remove.name,
  tf.na.age.mean.by.sex)

regression.age.sex.and.pclass <- approach.create.from(
  regression.base,
  tf.remove.name,
  tf.na.age.mean.by.sex.and.pclass)

regression.flare.cut.auto <- approach.create.from(
  regression.simpliest,
  tf.fare.cut.auto)

regression.by.sex <- approach.create.from(
  regression.simpliest,
  train.func = function(titanic) {
    train.regression(Survived ~ Sex, titanic)
  })

regression.by.sex.and.child <- approach.create.from(
  regression.simpliest,
  tf.add.child,
  train.func = function(titanic) {
    train.regression(Survived ~ Sex + Child, titanic)
  })

regression.by.sex.and.pclass <- approach.create.from(
  regression.simpliest,
  train.func = function(titanic) {
    train.regression(Survived ~ Sex + Pclass, titanic)
  })


regression.by.sex.and.fare <- approach.create.from(
  regression.simpliest,
  train.func = function(titanic) {
    train.regression(Survived ~ Sex + Fare, titanic)
  })

regression.fare.cut.manual.by.sex.and.fare.and.pclass.and.child <- approach.create.from(
  regression.simpliest,
  tf.fare.cut.manual,
  tf.add.child,
  train.func = function(titanic) {
    train.regression(Survived ~ Sex + Fare + Pclass + Child, titanic)
  })


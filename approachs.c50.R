train.c50 <- function(formula, df) {
  train(formula, df, method = "C5.0")
}

c50.base <- approach.create.from(
  approach.caret.base,
  train.func = function(titanic) {
    train.c50(Survived ~ ., titanic)
  })

c50.simpliest <- approach.create.from(
  c50.base,
  tf.remove.name,
  tf.na.age.mean)

c50.by.sex <- approach.create.from(
  c50.simpliest,
  train.func = function(titanic) {
    train.c50(Survived ~ Sex, titanic)
  })

c50.by.sex.and.pclass <- approach.create.from(
  c50.simpliest,
  train.func = function(titanic) {
    train.c50(Survived ~ Sex + Pclass, titanic)
  })


c50.by.sex.and.fare <- approach.create.from(
  c50.simpliest,
  train.func = function(titanic) {
    train.c50(Survived ~ Sex + Fare, titanic)
  })


c50.by.sex.pclass.fare.age <- approach.create.from(
  c50.simpliest,
  train.func = function(titanic) {
    train.c50(Survived ~ Sex + Pclass + Fare + Age, titanic)
  })

c50.by.sex.pclass.fare.age.embarked <- approach.create.from(
  c50.simpliest,
  train.func = function(titanic) {
    train.c50(Survived ~ Sex + Pclass + Fare + Age + Embarked, titanic)
  })
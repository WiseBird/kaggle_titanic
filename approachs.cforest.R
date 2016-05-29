train.cforest <- function(formula, df) {
  train(formula, df, method = "cforest")
}

cforest.base <- approach.create.from(
  approach.caret.base,
  train.func = function(titanic) {
    train.cforest(Survived ~ ., titanic)
  })

cforest.add.title <- approach.create.from(
  cforest.base,
  tf.add.title,
  tf.remove.name,
  tf.na.age.mean)
cforest.add.title.familySize <- approach.create.from(
  cforest.base,
  tf.add.title,
  tf.add.familySize,
  tf.remove.name,
  tf.na.age.mean)

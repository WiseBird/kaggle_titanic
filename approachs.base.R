approach.base <- approach.create(
  NULL,
  NULL,
  tf.remove.ticket,
  tf.remove.passengerId,
  tf.remove.cabin,
  tf.na.embarked.use.c,
  tf.na.fare.mean)

approach.caret.base <- approach.create.from(
  approach.base,
  predict.func = function(model, testing, ...) {
    caret::predict.train(model, newdata = testing, ...)
  },
  details.func = function(model, testing) {
    raw = caret::predict.train(model, newdata = testing, type = "raw")
    print(confusionMatrix(raw, testing$Survived))
    
    print(varImp(model))
  })
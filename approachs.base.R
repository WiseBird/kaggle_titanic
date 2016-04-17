approach.base <- approach.create(
  NULL,
  NULL,
  tf.remove.ticket,
  tf.remove.passengerId,
  tf.remove.cabin,
  tf.na.embarked.use.s,
  tf.na.fare.mean)

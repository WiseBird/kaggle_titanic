manual.create.prediction <- function(survived, type) {
  if(type == "raw") {
    return(survived)
  } else {
    return(data.frame("0" = 1-survived, "1" = survived))
  }  
}

manual.by.sex.and.fare.and.pclass.and.child <- approach.create.from(
  approach.base,
  tf.fare.cut.manual,
  tf.add.child,
  train.func = function(training) {training},
  predict.func = function(training, test, type = "raw") {
    test$Survived <- 0
    test$Survived[test$Sex == 'female'] <- 1
    test$Survived[test$Sex == 'female' & test$Pclass == 3] <- 0
    test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare == "Lowest"] <- 1
    test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare == "Low" & test$Child == "Child"] <- 1
    test$Survived[test$Sex == 'male' & test$Pclass == 1 & test$Fare == "Highest" & test$Child == "Child"] <- 1
    test$Survived[test$Sex == 'male' & test$Pclass == 2 & test$Fare == "High" & test$Child == "Child"] <- 1
    test$Survived[test$Sex == 'male' & test$Pclass == 2 & test$Fare == "Highest" & test$Child == "Child"] <- 1
    
    manual.create.prediction(test$Survived, type)
  },
  details.func = function(training) {
    x <- training %>%
      group_by(Sex, Pclass, Fare, Child) %>%
      summarise(p = if (length(which(Survived == "1")) / length(Survived) < 0.5) "Dead" else "Alive", length = length(Survived))
    
    print(as.data.frame(x))
  })

manual.age.cut.manual.by.sex.fare.pclass.age <- approach.create.from(
  approach.base,
  tf.fare.cut.manual,
  tf.na.age.mean,
  tf.age.cut.manual,
  train.func = function(training) {training},
  predict.func = function(training, test, type = "raw") {
    test$Survived <- 0
    
    test$Survived[test$Sex == 'female'] <- 1
    test$Survived[test$Sex == 'female' & test$Pclass == 3] <- 0
    test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Age == "Infant"] <- 1
    test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare == "Lowest"] <- 1
    test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare == "Low" & test$Age == "Child"] <- 1
    
    test$Survived[test$Sex == 'male' & test$Pclass == 1 & test$Fare == "High" & test$Age == "Adult"] <- 1
    test$Survived[test$Sex == 'male' & test$Pclass == 1 & (test$Age == "Child" | test$Age == "Infant")] <- 1
    test$Survived[test$Sex == 'male' & test$Pclass == 2 & (test$Age == "Child" | test$Age == "Infant")] <- 1
    test$Survived[test$Sex == 'male' & test$Pclass == 3 & (test$Fare == "Low" | test$Fare == "Lowest") & (test$Age == "Child" | test$Age == "Infant")] <- 1
    
    manual.create.prediction(test$Survived, type)
  },
  details.func = function(training) {
    x <- training %>%
      group_by(Sex, Pclass, Fare, Age) %>%
      summarise(p = if (length(which(Survived == "1")) / length(Survived) < 0.5) "Dead" else "Alive", length = length(Survived))
    
    print(as.data.frame(x))
  })
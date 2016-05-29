read.titanic <- function(file.name = "train.csv") {
  titanic <- read.csv(file.name, na.strings=c(""))
  titanic$Name <- as.character(titanic$Name)
  titanic$Pclass <- factor(titanic$Pclass)
  if(!is.null(titanic$Survived)) {
    titanic$Survived <- factor(titanic$Survived)
  }
  
  return(titanic)
}



tf.remove.ticket <- transformer.create(function(titanic) {
  titanic$Ticket <- NULL
  titanic
})
tf.remove.passengerId <- transformer.create(function(titanic) {
  titanic$PassengerId <- NULL
  titanic
})
tf.remove.name <- transformer.create(function(titanic) {
  titanic$Name <- NULL
  titanic
})
tf.remove.cabin <- transformer.create(function(titanic) {
  titanic$Cabin <- NULL
  titanic
})
tf.na.embarked.use.s <- transformer.create(function(titanic) {
  titanic$Embarked[is.na(titanic$Embarked)] <- "S"
  titanic
})

tf.na.fare.mean <- transformer.create(function(titanic, mean.fare) {
  titanic$Fare[is.na(titanic$Fare)] <- mean.fare
  titanic
}, function(titanic) {
  list(mean(titanic$Fare, na.rm=T))
})
tf.fare.cut.auto <- transformer.create(function(titanic) {
  titanic$Fare <- cut(titanic$Fare, 4)
  titanic
})
tf.fare.cut.manual <- transformer.create(function(titanic, mean.fare) {
  titanic$Fare[titanic$Fare == 0] <- mean.fare
  titanic$Fare <- cut(titanic$Fare, breaks = c(0,7.91,14.45,31,1000), labels=c("Lowest", "Low", "High", "Highest"), include.lowest = T)
  
  titanic
}, function(titanic) {
  list(mean(titanic$Fare[titanic$Fare != 0], na.rm=T))
})

tf.na.age.mean <- transformer.create(function(titanic, mean.age) {
  titanic$Age[is.na(titanic$Age)] <- mean.age
  titanic
}, function(titanic) {
  list(mean(titanic$Age, na.rm=T))
})
tf.na.age.mean.by.sex <- transformer.create(function(titanic, age.means) {
  rows.na.age <- titanic[is.na(titanic$Age),]
  titanic[is.na(titanic$Age),]$Age <- 
    by(rows.na.age, 
       1:nrow(rows.na.age), 
       function(row) {
         age.means[age.means$Sex == row$Sex,]$avg
       })
  
  titanic
}, function(titanic) {
  list(titanic %>%
         group_by(Sex) %>%
         summarise(avg = mean(Age, na.rm = T)))
})
tf.na.age.mean.by.sex.and.pclass <- transformer.create(function(titanic, age.means) {
  rows.na.age <- titanic[is.na(titanic$Age),]
  titanic[is.na(titanic$Age),]$Age <- 
    by(rows.na.age, 
       1:nrow(rows.na.age), 
       function(row) {
         age.means[age.means$Sex == row$Sex & age.means$Pclass == row$Pclass,]$avg
       })
  
  titanic
}, function(titanic) {
  list(titanic %>%
         group_by(Sex, Pclass) %>%
         summarise(avg = mean(Age, na.rm = T)))
})
tf.age.cut.manual <- transformer.create(function(titanic) {
  titanic$Age <- cut(titanic$Age, breaks = c(0,5,16,50,100), labels=c("Infant", "Child", "Adult", "Old"), include.lowest = T)
  
  titanic
})

tf.add.child <- transformer.create(function(titanic) {
  titanic$Child <- factor("Adult", levels = c("Child", "Adult"))
  titanic$Child[titanic$Age < 18] <- "Child"
  
  titanic
})
tf.add.title <- transformer.create(function(titanic) {
  titanic$Title <- sapply(titanic$Name, FUN=function(x) {strsplit(x, split = "[,.]")[[1]][2]})
  titanic$Title <- sub(' ', '', titanic$Title)
  titanic$Title[titanic$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
  titanic$Title[titanic$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  titanic$Title[titanic$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
  titanic$Title[titanic$Title %in% c('Col', 'Lady', 'Dr', 'Jonkheer', 'Master', 'Mlle', 'Ms', 'Rev', "Sir")] <- 'Noble'
  titanic$Title <- factor(titanic$Title)
  
  titanic
})
tf.add.familySize <- transformer.create(function(titanic) {
  titanic$FamilySize <- titanic$SibSp + titanic$Parch + 1
  
  titanic
})

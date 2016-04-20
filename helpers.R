# Will split data set in two parts: training and testing
split.test.train <- function(df, p=0.7) {
  intrain <- createDataPartition(df[[1]], p=p, list=FALSE)
  training <- titanic[intrain, ]
  testing <- titanic[-intrain, ]
  
  return(list(training = training, testing = testing))
}


# transformations: excluding variable, substituting NA with mean, factorizing etc
# goes in two steps:
#   1. Calc stats that required for transformation
#      Some transformation requires no stats, like removing variable
#      Substitution NA with mean requires calculation of mean
#   2. Transformation itself
#
# Two steps way is needed because we calc stats for trainig set and then use for both training and testing
transformer.create <- function(transform.func, stats.func = NULL) {
  list(
    prepare = function(training) {
      if(is.null(stats.func)) {
        transform.func
      } else {
        stats <- stats.func(training)
        function(df) {
          do.call(transform.func, append(list(df), stats))
        }
      }
    }
  )
}

approach.create <- function(train.func, predict.func, ..., details.func = NULL) {
  if (is.null(details.func)) {
    details.func = function() {}
  }
  
  Transformers <- list(...)
  prepareTransformers <- function(training) {
    if(length(Transformers) == 0) {
      function(df) {df}
    }
    
    transformers <- sapply(Transformers, function(Transformer) { Transformer$prepare(training) })
    
    function(df) {
      for(i in 1:length(Transformers)) {
        df <- transformers[[i]](df)
      }
      df
    }
  }
  
  
  list(
    Transformers = Transformers,
    train.func = train.func,
    predict.func = predict.func,
    details.func = details.func,
      
    predict = function(training, testing) {
      transform <- prepareTransformers(training)
      
      training <- transform(training)
      testing <- transform(testing)
      
      model <- train.func(training)
      predict.func(model, testing)
    },
    details = function(training) {
      transform <- prepareTransformers(training)
      
      training <- transform(training)
      
      model <- train.func(training)
      details.func(model)
    }
  )
}
approach.create.from <- function(parent, ..., train.func = NULL, predict.func = NULL, details.func = NULL) {
  params = list(
    train.func = if (is.null(train.func)) parent$train.func else train.func,
    predict.func = if (is.null(predict.func)) parent$predict.func else predict.func,
    details.func = if (is.null(details.func)) parent$details.func else details.func
  )

  params <- append(params, parent$Transformers)
  params <- append(params, list(...))
  
  do.call(approach.create, params)
}





score.classifier <- function(split.res, Approach, details = F) {
  predictions <- Approach$predict(split.res$training, split.res$testing)
  
  pred <- prediction(predictions, split.res$testing$Survived)
  auc <- performance(pred,"auc");
  
  if (details) {
    Approach$details(split.res$training)
  }
  
  auc@y.values
}

cross.validate.rand <- function(data, ..., n = 300) {
  sapply(list(...), function(Approach) {
    unlist(replicate(n, score.classifier(split.test.train(data), Approach)))
  })
}
cross.validate.k <- function(data, ..., k = 10) {
  folds <- createFolds(data[[1]], k = k, list = TRUE, returnTrain = FALSE)
  
  sapply(list(...), function(Approach) {
    unlist(sapply(names(folds), function(name) {
      training <- data[unlist(folds[names(folds) != name]), ]
      testing <- data[unlist(folds[name]), ]
      
      score.classifier(list(training = training, testing = testing), Approach)
    }))
  })
}

compare.approaches <- function(data, ..., n = 300) {
  scores <- sapply(list(...), function(Approach) {
    unlist(replicate(n, score.classifier(split.test.train(data), Approach)))
  })

  boxplot(scores)
  
  scores
}

# http://topepo.github.io/caret/training.html
# http://stats.stackexchange.com/questions/45569/what-is-the-cost-function-in-cv-glm-in-rs-boot-package
# https://stat.ethz.ch/R-manual/R-devel/library/boot/html/cv.glm.html
# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# http://pingax.com/logistic-regression-r-step-step-implementation-part-2/
# http://stats.stackexchange.com/questions/103459/how-do-i-know-which-method-of-cross-validation-is-best
# http://stats.stackexchange.com/questions/71184/cross-validation-or-bootstrapping-to-evaluate-classification-performance
learning.curve <- function(Approach, data, testing) {
  sapply(1:nrow(data), function(i) {
    trainig <- data[i, ]
    
  })
}


create.submit <- function(Approach, titanic, file.name = "submit") {
  test.final <- read.titanic("test.csv")
  predictions <- Approach$predict(titanic, test.final)
  
  submit <- data.frame(PassengerId = test.final$PassengerId, Survived = ifelse(predictions < 0.5, 0, 1))
  write.csv(submit, file = paste0(file.name, ".csv"), row.names = FALSE)
}
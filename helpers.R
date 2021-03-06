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
    
    transform = function(df) {
      transform <- prepareTransformers(df)
      
      transform(df)
    },
    predict = function(training, testing, ...) {
      #all.data <- rbind(testing, select(training, -Survived))
      
      transform <- prepareTransformers(training)
      
      training <- transform(training)
      testing <- transform(testing)
      
      model <- train.func(training)
      predict.func(model, testing, ...)
    },
    details = function(training, testing=training) {
      transform <- prepareTransformers(training)
      
      training <- transform(training)
      testing <- transform(testing)
      
      model <- train.func(training)
      details.func(model, testing)
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



calc.auc <- function(split.res, Approach, draw.plot = F) {
  predictions <- Approach$predict(split.res$training, split.res$testing, type = "prob")
  
  roc <- roc(split.res$testing$Survived,
      predictions[,2])
  
  if(draw.plot) {
    plot(roc, print.thres = c(.5), type = "S",
         print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
         print.thres.cex = .8,
         legacy.axes = TRUE)
  }
  
  roc$auc
}
calc.kappa <- function(split.res, Approach) {
  raw = Approach$predict(split.res$training, split.res$testing, type = "raw")
  cm <- confusionMatrix(raw, split.res$testing$Survived)
  cm$overall["Kappa"]
}
calc.accuracy <- function(split.res, Approach) {
  raw = Approach$predict(split.res$training, split.res$testing, type = "raw")
  cm <- confusionMatrix(raw, split.res$testing$Survived)
  cm$overall["Accuracy"]
}
calc.log.regr.cost <- function(split.res, Approach) {
  predictions <- Approach$predict(split.res$training, split.res$testing, type = "prob")
  
  # convert to integers 1 and 0
  y <- as.integer(as.character(split.res$testing$Survived))

  # doubles [0, 1]
  probs <- predictions[,2]
  
  costs <- y * log(probs) + (1-y) * log(1 - probs)
  -sum(costs)/length(y)
}

cv.leave.p.out <- function(data, ..., n = 50, p = 0.2, stat = calc.auc) {
  sapply(list(...), function(Approach) {
    unlist(replicate(n, stat(split.test.train(data, 1-p), Approach)))
  })
}
cv.bootstrap <- function(data, ..., n = 50, stat = calc.auc) {
  resamples <- createResample(data[[1]], times = n, list = TRUE)
  sapply(list(...), function(Approach) {
    sapply(1:length(resamples), function(ind) {
      training.inds <- resamples[[1]]
      testing.inds <- setdiff(1:nrow(data), training.inds[[1]])
      
      training <- data[training.inds, ]
      testing <- data[testing.inds, ]
      
      stat(list(training = training, testing = testing), Approach)
    })
  })
}
cv.k.folds <- function(data, ..., k = 10, stat = calc.auc) {
  folds <- createFolds(data[[1]], k = k, list = TRUE, returnTrain = FALSE)

  sapply(list(...), function(Approach) {
    sapply(names(folds), function(name) {
      training <- data[unlist(folds[names(folds) != name]), ]
      testing <- data[unlist(folds[name]), ]
      
      stat(list(training = training, testing = testing), Approach)
    })
  })
}


compare.approaches <- function(data, cv = cv.k.folds, ..., is.plot.box = T, is.plot.hist = F) {
  scores <- cv(data, ...)
  
  means = lapply(1:ncol(scores), function(i) { mean(scores[,i]) })
  print(means)
  
  if(is.plot.box) {
    plot.box <- ggplot()
    for(i in 1:ncol(scores)) {
      plot.box <- plot.box + geom_boxplot(aes_string(i, scores[,i]))
    }
    print(plot.box)
  }
  
  if(is.plot.hist) {
    par(mfrow=c(1, ncol(scores)))
    lapply(1:ncol(scores), function(i) { hist(scores[,i]) })
    
    par(mfrow=c(1,1))
  }
}

plor.roc.curves <- function(split.res, ...) {
  results <- data.frame(Class = split.res$testing$Survived)
  
  approaches <- list(...)
  
  names <- letters[1:length(approaches)]
  
  for(ind in 1:length(approaches)) {
    Approach <- approaches[[ind]]
    
    results[, names[ind]] <- Approach$predict(split.res$training, split.res$testing, type = "prob")[,1]
  }
  
  print(head(results))
  
  trellis.par.set(caretTheme())
  liftData <- lift(reformulate(termlabels = names, response = 'Class'), data = results)
  plot(liftData, values = 60, auto.key = list(columns = 3,
                                              lines = TRUE,
                                              points = FALSE))
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


create.submit <- function(titanic, approach.name) {
  approach <- get(approach.name)
  
  test.final <- read.titanic("test.csv")
  
  predictions <- approach$predict(titanic, test.final)
  predictions <- as.integer(as.character(predictions))
  
  submit <- data.frame(PassengerId = test.final$PassengerId, Survived = predictions)
  write.csv(submit, file = paste0(approach.name, ".csv"), row.names = FALSE)
}

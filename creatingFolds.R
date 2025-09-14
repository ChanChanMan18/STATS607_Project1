### Creating Folds
folds <- createFolds(outputValues, 
                     k = numfolds,
                     list = TRUE,
                     returnTrain = TRUE)

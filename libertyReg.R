# load libraries and source files

library(caret)
source('~/Github/Competition/liberty/libertyGini.R')

# load data

train <- read.csv("/Users/Tom/Github/Competition/liberty/libertyTrain.csv")
test <- read.csv("/Users/Tom/Github/Competition/liberty/libertyTest.csv")

# set train control

giniControl <- trainControl(method = "cv", number = 5, summaryFunction = GiniSummary)
lmFit <- train(Hazard ~., data = train[, -1], method = "lm", trGrid = giniControl,
               metric = "GINI", maximize = TRUE)

ctrl <- trainControl(method = "cv", number = 10) #repeats = 3)

glmnetGrid <- expand.grid(alpha = seq(0, 1, length = 4), 
                          lambda = seq(.001, .01, length = 4))

glmNetFit <- train(Hazard ~., data = train[, -1], method = "glmnet", 
                   trControl = giniControl,  preProc = c("center", "scale"), 
                   tuneGrid = glmnetGrid, metric = "GINI")

glmInteractionFit <- train(Hazard ~(.)^2, data = train[, -1], method = "glmnet", 
                   trControl = ctrl,  preProc = c("center", "scale"), 
                   tuneGrid = glmnetGrid, metric = "RMSE")    

svmGrid <- expand.grid(sigma = seq(.0002, .2, length = 3), C = seq(.01, .5, length = 3))

svmFit <- train(Hazard ~., data = train[, -1], method = "svmRadial", 
                trControl = ctrl,  preProc = c("center", "scale"), 
                tuneGrid = svmGrid, metric = "RMSE")

#rfGrid <- data.frame(mtry = floor(seq(2, ncol(train[, -(1:2)]), length = 5)))

rfFit <- train(train[, -(1:2)], train$Hazard, method = "rf", ntree = 1000, #tuneGrid = rfGrid,
                importance = FALSE, trControl = ctrl, metric = "RMSE")

gbmGrid <- expand.grid(n.trees = 500,
                      interaction.depth = seq(1, 3, by = 2),
                       shrinkage = c(.01, .1),
                      n.minobsinnode = c(3, 5))
                       
gbmFit2 <- train(train[, -(1:2)], train[, ]$Hazard, method = "gbm",
                tuneGrid = gbmGrid, metric = "RMSE", verbose = F)

WriteSubmissionFile <- function(model) {
  test$Hazard <- predict(model, test[, -1])
  submission <- as.data.frame(cbind(test$Id, test$Hazard))
  colnames(submission) <- c("Id", "Hazard")
  submission <- submission[order(-submission$Hazard),]
  write.csv(submission, 
            file = "/Users/Tom/Github/Competition/liberty/libertySubmission.csv", 
            row.names = FALSE, quote = FALSE)
}


gbmHazard <- predict(gbmFit2, test[, -1])
glmHazard <- predict(glmNetFit, test[, -1])

# from Kaggle website

SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  
  df$random = (1:nrow(df))/nrow(df)
  
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  
  return(sum(df$Gini))
}

# from Kaggle Wesbite

nGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}

# custom made but closely following this post from Kuhn
# http://stackoverflow.com/questions/22434850/user-defined-metric-in-caret-package

GiniSummary <- function(data, level = NULL, model = NULL) {
  out <- nGini(data$obs, data$pred)
  names(out) <- "GINI"
  out
}
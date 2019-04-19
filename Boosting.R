# Luke Price 
library(gbm)
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(1166)
d <- read.csv("fball_all_years.csv")[, -c(1,3,24,22,35:37)]

############################ BOOSTING MODELS ###############################

boost.list <- list(
  RB = function(data, int.depth) {
    gbm(off.points ~ . - IDS - year - week - top.10 - pass.2pt -
          total.yards.allowed -
          comp - pass.TD - pass.yards - pass.int -
          rec - rush.att,
        data = data[,-6],
        distribution = "gaussian", n.trees = 5000, interaction.depth = int.depth, shrinkage = 0.01)},
  WR = function(data, int.depth) {
    gbm(off.points ~ . - IDS - year - week - top.10 - pass.2pt -
          comp - pass.TD - pass.yards - pass.int -
          rec - rush.att - rush.2pt,
        data = data[,-6],
        distribution = "gaussian", n.trees = 5000, interaction.depth = int.depth, shrinkage = 0.01)},
  QB =  function(data, int.depth) {
    gbm(off.points ~ . - IDS - year - week - top.10 - 
          total.yards.allowed -
          rush.2pt - rush.att - 
          comp  - pass.att - 
          rec - rec.2pt,
        data = data[,-6],
        distribution = "gaussian", n.trees = 5000, interaction.depth = int.depth, shrinkage = 0.01)}
)

############################ PARAMETER TUNING ###############################
### argument 'position' take on values 1, 2, or 3: 1 for RB, 2 for WR, 3 for QB. 
### max_depth determines the max interaction depth value tested (1:max_depth)
### returns a list containing: $best.depth, $summary, $int.depth.errors

tuning <- function(position, max_depth) {
  
  begin.tuning <- Sys.time()
  
  d <- filter(d, position == position)
  test.weeks <- as.data.frame(table(filter(d, year == 2018)$week))
  test.weeks$Var1 <- as.integer(as.character(test.weeks$Var1))
  
  boost.tune <- vector("list", max_depth)
  boost.yhats <- vector("list", max_depth)
  boost.errors <- vector("list", max_depth)
  boost.matrix <- seq(1:max_depth)
  
  for (i in min(test.weeks$Var1):max(test.weeks$Var1)) {
    
    tune.data <- filter(d, year != 2018 | year == 2018 & week < i)
    iter <- sample(1:dim(tune.data)[1], floor(dim(tune.data)[1]*(3/4)))
    train <- tune.data[iter,]
    test <- tune.data[-iter,]
    y.tune.test <- test[,5]
    
    for (j in 1:max_depth) {
      
      boost.tune[[j]] <- boost.list[[position]](data = train, int.depth = j)
      
      boost.yhats[[j]] <- predict(boost.tune[[j]], newdata = test, n.trees = 5000)
      boost.errors[[j]] <- sqrt(mean((boost.yhats[[j]] - y.tune.test)^2))
      
    }
    
    boost.matrix <- cbind(boost.matrix, unlist(boost.errors))
    
  }
  
  colnames(boost.matrix) <- as.list(seq(1:10))
  colnames(boost.matrix)[colnames(boost.matrix) == "1"] <- "depth"
  
  depth.means <- cbind(c(1,2), rowMeans(boost.matrix[,-1]))
  colnames(depth.means) <- c("depth", "RMSE")
  
  best.depth <- which.min(rowMeans(boost.matrix[,-1]))
  
  end.tuning <- Sys.time()
  print(end.tuning - begin.tuning)
  
  return(list('best.depth' = best.depth, 'summary' = boost.matrix, 'depth.errors' = depth.means))
  
}

### DO NOT RUN TUNING FUNCTION; IT TAKE OVER 10 MINUTES TO RUN
# RB.tune <- tuning(1, 15)
# WR.tune <- tuning(1, 15)
# QB.tune <- tuning(1, 15)



############################ TRAIN AND TEST BEST MODELS ###############################
### argument 'position' take on values 1, 2, or 3: 1 for RB, 2 for WR, 3 for QB. 
### int.depth is used for typical gbm interaction.depth
### returns a list containing: $models, $RMSE, $MSE

train_test <- function(position, int.depth) {
  
  begin.boost <- Sys.time()
  
  d <- filter(d, position == position)
  test.weeks <- as.data.frame(table(filter(d, year == 2018)$week))
  test.weeks$Var1 <- as.integer(as.character(test.weeks$Var1))
  y.test <- vector("list", dim(test.weeks)[1])
  
  boosts <- vector("list", dim(test.weeks)[1])
  boost.yhats <- vector("list", dim(test.weeks)[1])
  boost.RMSE <- vector("list", dim(test.weeks)[1])
  
  for (i in min(test.weeks$Var1):max(test.weeks$Var1)) {
    
    train <- filter(d, year != 2018 | year == 2018 & week < i)
    test <- filter(d, year == 2018 & week == i)
    y.test[[i]] <- test[,5]
    
    
    boosts[[i]] <- boost.list[[position]](data = train, int.depth = int.depth)
    
    boost.yhats[[i]] <- predict(boosts[[i]], newdata = test, n.trees = 5000)
    boost.RMSE[[i]] <- sqrt(mean((boost.yhats[[i]] - y.test[[i]])^2))
    
  }
  
  RMSE.table <- as.data.frame(cbind(seq(from = 2, to = 10), unlist(boost.RMSE)))
  colnames(RMSE.table) <- c("Week", "Boost")
  
  MSE = mean(RMSE.table$Boost^2)
  
  end.boost <- Sys.time()
  print(end.boost - begin.boost)
  
  return(list('models' = boosts, 'RMSE' = RMSE.table, 'MSE' = MSE))
  
}

RB.Test <- train_test(1, 1)
WR.Test <- train_test(2, 1)
QB.Test <- train_test(3, 1)




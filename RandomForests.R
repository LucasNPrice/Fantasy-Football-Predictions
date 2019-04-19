# Luke Price 
library(randomForest)
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(1166)
d <- read.csv("fball_all_years.csv")[, -c(1,3,24,22,35:37)]

############################ RANDOM FOREST MODELS ###############################
forest.list <- list(
  RB = function(data, mtry) {
    randomForest(off.points ~ . - IDS - year - week - top.10 - pass.2pt - 
                   total.yards.allowed -
                   comp - pass.TD - pass.yards - pass.int - 
                   rec - rush.att, 
                 mtry = mtry, ntree = 500, 
                 data = data[,-6])},
  WR = function(data, mtry) {
    randomForest(off.points ~ . - IDS - year - week - top.10 - pass.2pt - 
                   total.yards.allowed -
                   comp - pass.TD - pass.yards - pass.int - 
                   rec - rush.att - rush.2pt, 
                 mtry = mtry, ntree = 500, 
                 data = data[,-6])},
  QB =  function(data, mtry) {
    randomForest(off.points ~ . - IDS - year - week - top.10 - 
                   total.yards.allowed -
                   rush.2pt - rush.att - 
                   comp  - pass.att - 
                   rec - rec.2pt,
                 mtry = mtry, ntree = 500, 
                 data = data[,-6])}
)

############################ PARAMETER TUNING ###############################
### argument 'position' take on values 1, 2, or 3: 1 for RB, 2 for WR, 3 for QB. 
### max_mtry determines the max mtry value tested (1:max.mtry)
### returns a list containing: $best.mtry, $summary, $mtry.errors
tuning <- function(position, max_mtry) {
  
  begin.tuning <- Sys.time()
  
  d <- filter(d, position == position)
  test.weeks <- as.data.frame(table(filter(d, year == 2018)$week))
  test.weeks$Var1 <- as.integer(as.character(test.weeks$Var1))
  
  forest.tune <- vector("list", max_mtry)
  forest.yhats <- vector("list", max_mtry)
  forest.errors <- vector("list", max_mtry)
  forest.matrix <- seq(1:max_mtry)
  
  for (i in min(test.weeks$Var1):max(test.weeks$Var1)) {
    
    tune.data <- filter(d, year != 2018 | year == 2018 & week < i)
    iter <- sample(1:dim(tune.data)[1], floor(dim(tune.data)[1]*(3/4)))
    train <- tune.data[iter,]
    test <- tune.data[-iter,]
    y.tune.test <- test[,5]
    
    for (j in 1:max_mtry) {
      
      forest.tune[[j]] <- forest.list[[position]](data = train, mtry = j)

      forest.yhats[[j]] <- predict(forest.tune[[j]], newdata = test)
      forest.errors[[j]] <- sqrt(mean((forest.yhats[[j]] - y.tune.test)^2))
      
    }
    
    forest.matrix <- cbind(forest.matrix, unlist(forest.errors))
    
  }
  
  colnames(forest.matrix) <- as.list(seq(1:10))
  colnames(forest.matrix)[colnames(forest.matrix) == "1"] <- "mtry"
  
  mtry.means <- cbind(c(1,2), rowMeans(forest.matrix[,-1]))
  colnames(mtry.means) <- c("mtry", "RMSE")
  
  best.mtry <- which.min(rowMeans(forest.matrix[,-1]))
  
  end.tuning <- Sys.time()
  print(end.tuning - begin.tuning)

  return(list('best.mtry' = best.mtry, 'summary' = forest.matrix, 'mtry.errors' = mtry.means))
  
}

### DO NOT RUN TUNING FUNCTION; IT TAKE OVER 10 MINUTES TO RUN
# RB.tune <- tuning(1, 15)
# WR.tune <- tuning(1, 15)
# QB.tune <- tuning(1, 15)



############################ TRAIN AND TEST BEST MODELS ###############################
### argument 'position' take on values 1, 2, or 3: 1 for RB, 2 for WR, 3 for QB. 
### mtry is used for typical random forest 
### returns a list containing: $forests, $RMSE, $MSE
train_test <- function(position, mtry) {
  
  begin.forest <- Sys.time()
  
  d <- filter(d, position == position)
  test.weeks <- as.data.frame(table(filter(d, year == 2018)$week))
  test.weeks$Var1 <- as.integer(as.character(test.weeks$Var1))
  y.test <- vector("list", dim(test.weeks)[1])
  
  forests <- vector("list", dim(test.weeks)[1])
  forest.yhats <- vector("list", dim(test.weeks)[1])
  forest.RMSE <- vector("list", dim(test.weeks)[1])
  
  for (i in min(test.weeks$Var1):max(test.weeks$Var1)) {
    
    train <- filter(d, year != 2018 | year == 2018 & week < i)
    test <- filter(d, year == 2018 & week == i)
    y.test[[i]] <- test[,5]
    
    forests[[i]] <- forest.list[[position]](data = train, mtry = mtry)
    
    forest.yhats[[i]] <- predict(forests[[i]], newdata = test)
    forest.RMSE[[i]] <- sqrt(mean((forest.yhats[[i]] - y.test[[i]])^2))
    
  }
  
  RMSE.table <- as.data.frame(cbind(seq(from = 2, to = 10), unlist(forest.RMSE)))
  colnames(RMSE.table) <- c("Week", "R.Forest")
  
  MSE = mean(RMSE.table$R.Forest^2)
  
  end.forest <- Sys.time()
  print(end.forest - begin.forest)
  
  return(list('models' = forests, 'RMSE' = RMSE.table, 'MSE' = MSE))
  
}

RB.Test <- train_test(1, 3)
WR.Test <- train_test(2)
QB.Test <- train_test(3)

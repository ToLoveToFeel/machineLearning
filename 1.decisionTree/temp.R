for (i in seq(dataConvertStart, dataConvertEnd)) {
  if ('numeric' == dataConvertType) {
    data[ , i] <- as.character(data[ , i])
    data[ , i] <- as.numeric(data[ , i])
  } else if ('factor' == dataConvertType) {
    data[ , i] <- as.factor(data[ , i])
  } 
}
data[ , 1] <- as.character(data[ , 1])  # 重新转为factor需要下面三步
data[ , 1] <- as.numeric(data[ , 1])
data[ , 1] <- as.factor(data[ , 1])


library(mlbench)
data(Sonar)
str(Sonar[, 1:10])

library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]



write.table(iris, file = 'iris.csv', sep = ',')







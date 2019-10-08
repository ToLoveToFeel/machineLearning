if (FALSE) {
  "@author: Wang Xiaoxiao
   @date: 2019-10-02
  "
}
################################数据包导入###########################
rm(list = ls())        # 清空所有变量
library(lattice)
library(ggplot2)
library(caret)         # 预处理包
library(rpart)         # 决策树
library(rpart.plot)    # 决策树作图相关包
library(pROC)
###########################################################导入结束


################################变量初始化###########################
filePath = './Leukemia-2-7129-72.csv'  # 文件路径
cvTimes <- 50       # 交叉验证次数
cvFold <- 5         # 数据分为折数

changeColumnName = TRUE     # 设置为TRUE开启更改 列名
deleteRow <- TRUE           # 设置为TRUE删除某些行
deleteRowNumber <- c(1)     # 删除的 行 编号
changeRowName = TRUE        # 设置为TRUE开启更改 行标
deleteColumn <- FALSE       # 设置为TRUE删除某些 列
deleteColumnNumber <- c(1)  # 删除的 列 编号
###########################################################初始化结束


################################数据预处理###########################
# 读入数据
data <- read.table(file = filePath, sep = ',')
# 数据转置，保证每一行为一个为一个样本，每一列为一个特征
data <- t(data)
# 转置后数据变为矩阵，再次将数据转换为数据帧
data <- as.data.frame(data)
# 查看数据格式
# head(data)
#########################以下为可选内容
dataSize <- dim(data)
# 更改列名
if (changeColumnName) {
  for (i in seq(dataSize[2])) {
    names(data)[i] <- paste(data[1,i])
  }
}
names(data)[1] <- paste('class')
# 删除某些行
if (deleteRow) {
  for (i in deleteRowNumber) {
    data <- data[-i, ]
  }
}
# 更改行标
if (changeRowName) {
  for (i in seq(dataSize[1] - 1)) {
    row.names(data)[i] <- paste(i)
  }
}
# 删除某些列
if (deleteColumn) {
  for (i in deleteColumnNumber) {
    data <- data[ , -i]
  }
}
# 查看数据格式
# head(data)
#########################可选内容结束
# 转置后每一列数据全部变为factor类型，根据其实际类型转换
dataConvertStart <- 1  # 数据类型转换起始位置
dataConvertEnd <- dataSize[2]  # 数据类型转换终止位置
data[ , dataConvertStart:dataConvertEnd] = sapply(data[ , dataConvertStart:dataConvertEnd], as.character)
data[ , dataConvertStart:dataConvertEnd] = sapply(data[ , dataConvertStart:dataConvertEnd], as.numeric)
data[ , 1] <- as.factor(data[ , 1])
# 查看数据格式，最终结果，每一行一个样本，每一列一个特征
# head(data)
###########################################################预处理结束


################################算法执行主体###########################
if (FALSE) {
  "函数名称：label
   函数作用：生成cvFold组个数相等的，数据元素从1到cvFold的向量，用作交叉验证分组使用
   函数参数：seed         随机种子
   函数返回值：cvLabel    生成的随机数向量
   演示用例：label(2)
  "
}
label <- function(seed) {
  variableNum <- nrow(data) # 样本个数
  cvNum <- as.integer(variableNum/cvFold) # 划分后每个集合的个数
  lastCvNum <- variableNum - (cvFold - 1) * cvNum # 最后一个集合的个数
  times <- numeric(cvFold)
  for (i in seq(cvFold)) {
    times[i] <- cvNum
  }
  times[cvFold] <- lastCvNum
  cvLabel <- rep(1:cvFold, times = times)
  set.seed(seed)  # 设置随机种子，保证可再现
  cvLabel <- sample(cvLabel, variableNum, replace = F)  # 生成伪随机序列
  
  return (cvLabel)
}

if (FALSE) {
  "函数名称：Scaling
   函数作用：数据缩放
   函数参数：data         需要缩放的数据
             method       缩放的方法
   函数返回值：data       缩放后的数据
   演示用例：train <- Scaling(train, 'UVScaling')
  "
}
Scaling <- function(data, method = 'UVScaling') {
  dimension <- dim(data)
  if ('UVScaling' == method) {
    data[, 2:dimension[2]] <- sapply(as.data.frame(data[ , 2:dimension[2]]), function(x) (x - mean(x))/sd(x) )
  } else if ('Par' == method) {
    data[, 2:dimension[2]] <- sapply(as.data.frame(data[ , 2:dimension[2]]), function(x) (x - mean(x))/sqrt(sd(x)) )
  }

  return (data)
}

if (FALSE) {
  "函数名称：decisionTree
   函数作用：决策树算法
   函数参数：seed         随机种子
   函数返回值：无
   演示用例：decisionTree(2)
  "
}
factorNum <- length(levels(data$class))
decisionTree <- function(seed = 1) {
  cvLabel <- label(seed)  # 生成伪随机序列
  n <- ncol(data)
  res <- array(0, dim = c(factorNum, factorNum, cvFold))
  for (i in 1:cvFold) {
    train <- data[cvLabel != i, ]
    train <- Scaling(train, 'UVScaling')
    test <- data[cvLabel == i, ]
    test <- Scaling(test, 'UVScaling')
    model <- rpart(class ~ . , data = train, control = rpart.control(cp = 0.1))
    pre <- predict(model, test[ , -1], type = 'class')
    res[ , , i] <- as.matrix(table(pre, test[ , 1]))
  }
  table <- apply(res, MARGIN = c(1, 2), sum)
  print(table)
  rate <- sum(diag(table))/sum(table)
  print(rate)
  returnValue <- list(table, rate)
  
  return (returnValue)
}

if (FALSE) {
  "函数名称：fileOutput
   函数作用：将准确率和混淆矩阵输出到文件
   函数参数：times         已经交叉验证的次数
   函数返回值：无
   演示用例：fileOutput(1)
  "
}
fileOutput <- function(times) {
  acc <- numeric(times)
  accNum <- 1
  confusionMatrix <- array(0, dim = c(factorNum, factorNum, times))
  confusionMatrixNum <- 1
  for (i in seq(2*times)) {
    if (0 == i %% 2) {
      acc[accNum] <- confusionMatrixList[[i]]
      accNum <- accNum + 1
    } else {
      confusionMatrix[ , , confusionMatrixNum] <- confusionMatrixList[[i]]
      confusionMatrixNum <- confusionMatrixNum + 1
    }
  }
  write.csv(as.data.frame(acc), file = 'acc.csv')
  write.csv(confusionMatrix, file = 'confusionMatrix.csv')
}

confusionMatrixList <- list()  # 存储着混淆矩阵和准确率
for (i in seq(cvTimes)) {
  pastTime = proc.time()
  confusionMatrixList <- c(confusionMatrixList, decisionTree(seed = i))
  fileOutput(times = i)
  nowTime = proc.time()
  print(nowTime - pastTime)
}
############################################################算法执行主体结束


################################数据分析以及输出到文件###########################



###########################################################数据分析以及输出到文件结束

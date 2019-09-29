################################数据包导入###########################
# 清空所有变量
rm(list = ls())
library(caret)         # 预处理包
library(rpart)         # 决策树
library(rpart.plot)    # 决策树作图相关包
library(pROC)
###########################################################导入结束



################################数据预处理###########################
# 读入数据
filePath = './PimaIndiansDiabetes2.csv'  # 文件路径
data <- read.table(file = filePath, sep = ',')
# 数据转置，保证每一行为一个为一个样本，每一列为一个特征
data <- t(data)
# 转置后数据变为矩阵，再次将数据转换为数据帧
data <- as.data.frame(data)
# 查看数据格式
head(data)
#########################以下为可选内容
# 更改列名
changeColumnName = TRUE  # 设置为TRUE开启更改列名
dataSize <- dim(data)
if (changeColumnName) {
  for (i in seq(dataSize[2])) {
    names(data)[i] <- paste(data[1,i])
  }
}
# 删除某些行
deleteRow <- TRUE  # 设置为TRUE删除某些行
deleteRowNumber <- c(1) # 删除的行编号
if (deleteRow) {
  for (i in deleteRowNumber) {
    data <- data[-i, ]
  }
}
# 删除某些列
deleteColumn <- TRUE  # 设置为TRUE删除某些列
deleteColumnNumber <- c(1) # 删除的列编号
if (deleteRow) {
  for (i in deleteRowNumber) {
    data <- data[ , -i]
  }
}
# 更改行标
changeRowName = TRUE  # 设置为TRUE开启更改行标
dataSize <- dim(data)
if (changeColumnName) {
  for (i in seq(dataSize[1])) {
    row.names(data)[i] <- paste(i)
  }
}
#########################可选内容结束
# 转置后每一列数据全部变为factor类型，根据其实际类型转换
dataConvertStart <- 1  # 数据类型转换起始位置
dataConvertEnd <- 8  # 数据类型转换终止位置
dataConvertType <- 'numeric'  # 'numeric', 'factor', ...
for (i in seq(dataConvertStart, dataConvertEnd)) {
  if ('numeric' == dataConvertType) {
    data[ , i] <- as.numeric(data[ , i])
  } else if ('factor' == dataConvertType) {
    data[ , i] <- as.factor(data[ , i])
  } 
}
data[ , 9] <- as.character(data[ , 9])  # 重新转为factor需要下面两步
data[ , 9] <- as.factor(data[ , 9])
# 查看数据格式
head(data)

# 标准化，scaleddata中存储着标准化后的结果
preProcValues <- preProcess(data[ , -9], method = c("center", "scale")) # 该函数默认剔除含有缺失值的数据后用剩下的数据计算均值和标准差
scaleddata <- predict(preProcValues, data[ , -9])
# 观察各变量的分布以及和目标变量之间的关系
featurePlot(scaleddata, data[ , 9], plot = 'box')
# YeoJohnson转换，让数据向正态分布靠拢，boxdata存储着处理后的结果
preProcbox <- preProcess(scaleddata, method = c('YeoJohnson'))
boxdata <- predict(preProcbox, scaleddata)
# 缺失值插补
preProcimp <- preProcess(boxdata, method = "bagImpute")
procdata <- predict(preProcimp, boxdata)
procdata$class <- data[ , 9]
###########################################################预处理结束



################################算法执行主体###########################
# 交叉验证
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,  # 10重交叉验证
                           repeats = 10   # 重复10重交叉验证10次
)
tunedf <- data.frame(.cp = seq(0.001, 0.1, length.out = 10)) # 尝试从0.001到0.1的等距的10个参数
treemodel <- train(x = procdata[ , -9],
                   y = procdata[ , 9],
                   method = 'rpart',  
                   trControl = fitControl,
                   tuneGrid = tunedf
)
# ###########################################################算法执行主体结束



################################数据分析以及后处理###########################
treemodel
plot(treemodel)

ggplot(treemodel)
# ROC曲线
pre <- predict(treemodel, type = 'prob')$pos
modelroc <- roc(procdata$class, pre)
plot(modelroc,
     print.auc = TRUE,
     auc.polygon = TRUE,
     grid = c(0.1, 0.2),
     grid.col = c('green', 'red'),
     max.auc.polygon = TRUE,
     auc.polygon.col = 'skyblue',
     print.thres = TRUE
)

# 预测
pre <- predict(treemodel, type = 'raw')
(preTable <- table(pre, procdata$class))  # 混淆矩阵
(accuracy <- sum(diag(preTable))/sum(preTable)) # 精确度
preTable[1, 1]/sum(preTable[ , 1]) # 特异度，正确判断非病人的比率
preTable[2, 2]/sum(preTable[ , 2]) # 灵敏度，正确判断病人的比率
(P <- preTable[2, 2]/sum(preTable[ 2, ]))  # P值
(R <- preTable[2, 2]/sum(preTable[ , 2]))  # R值
(F1 <- (2*P*R)/(P+R))  # F1值


rpartModel <- rpart(class ~ . ,
                    data = procdata,
                    method ="class",
                    parms = list(split="information"),  # ID3
                    control = rpart.control(cp = 0.023)
                    )
# rpartModel <- rpart(class ~ . ,
#                     data = procdata,
#                     method ="class", 
#                     parms = list(split="gini"),  # CART
#                     control = rpart.control(cp = 0.023)
# )
rpart.plot(rpartModel)
###########################################################数据分析以及后处理







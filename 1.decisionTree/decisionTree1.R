if (FALSE) {
  "@author: Wang Xiaoxiao
   @date: 2019-10-02
  "
}
################################���ݰ�����###########################
rm(list = ls())        # ������б���
library(lattice)
library(ggplot2)
library(caret)         # Ԥ������
library(rpart)         # ������
library(rpart.plot)    # ��������ͼ��ذ�
library(pROC)
###########################################################�������


################################������ʼ��###########################
filePath = './Leukemia-2-7129-72.csv'  # �ļ�·��
cvTimes <- 50       # ������֤����
cvFold <- 5         # ���ݷ�Ϊ����

changeColumnName = TRUE     # ����ΪTRUE�������� ����
deleteRow <- TRUE           # ����ΪTRUEɾ��ĳЩ��
deleteRowNumber <- c(1)     # ɾ���� �� ���
changeRowName = TRUE        # ����ΪTRUE�������� �б�
deleteColumn <- FALSE       # ����ΪTRUEɾ��ĳЩ ��
deleteColumnNumber <- c(1)  # ɾ���� �� ���
###########################################################��ʼ������


################################����Ԥ����###########################
# ��������
data <- read.table(file = filePath, sep = ',')
# ����ת�ã���֤ÿһ��Ϊһ��Ϊһ��������ÿһ��Ϊһ������
data <- t(data)
# ת�ú����ݱ�Ϊ�����ٴν�����ת��Ϊ����֡
data <- as.data.frame(data)
# �鿴���ݸ�ʽ
# head(data)
#########################����Ϊ��ѡ����
dataSize <- dim(data)
# ��������
if (changeColumnName) {
  for (i in seq(dataSize[2])) {
    names(data)[i] <- paste(data[1,i])
  }
}
names(data)[1] <- paste('class')
# ɾ��ĳЩ��
if (deleteRow) {
  for (i in deleteRowNumber) {
    data <- data[-i, ]
  }
}
# �����б�
if (changeRowName) {
  for (i in seq(dataSize[1] - 1)) {
    row.names(data)[i] <- paste(i)
  }
}
# ɾ��ĳЩ��
if (deleteColumn) {
  for (i in deleteColumnNumber) {
    data <- data[ , -i]
  }
}
# �鿴���ݸ�ʽ
# head(data)
#########################��ѡ���ݽ���
# ת�ú�ÿһ������ȫ����Ϊfactor���ͣ�������ʵ������ת��
dataConvertStart <- 1  # ��������ת����ʼλ��
dataConvertEnd <- dataSize[2]  # ��������ת����ֹλ��
data[ , dataConvertStart:dataConvertEnd] = sapply(data[ , dataConvertStart:dataConvertEnd], as.character)
data[ , dataConvertStart:dataConvertEnd] = sapply(data[ , dataConvertStart:dataConvertEnd], as.numeric)
data[ , 1] <- as.factor(data[ , 1])
# �鿴���ݸ�ʽ�����ս����ÿһ��һ��������ÿһ��һ������
# head(data)
###########################################################Ԥ��������


################################�㷨ִ������###########################
if (FALSE) {
  "�������ƣ�label
   �������ã�����cvFold�������ȵģ�����Ԫ�ش�1��cvFold������������������֤����ʹ��
   ����������seed         �������
   ��������ֵ��cvLabel    ���ɵ����������
   ��ʾ������label(2)
  "
}
label <- function(seed) {
  variableNum <- nrow(data) # ��������
  cvNum <- as.integer(variableNum/cvFold) # ���ֺ�ÿ�����ϵĸ���
  lastCvNum <- variableNum - (cvFold - 1) * cvNum # ���һ�����ϵĸ���
  times <- numeric(cvFold)
  for (i in seq(cvFold)) {
    times[i] <- cvNum
  }
  times[cvFold] <- lastCvNum
  cvLabel <- rep(1:cvFold, times = times)
  set.seed(seed)  # ����������ӣ���֤������
  cvLabel <- sample(cvLabel, variableNum, replace = F)  # ����α�������
  
  return (cvLabel)
}

if (FALSE) {
  "�������ƣ�Scaling
   �������ã���������
   ����������data         ��Ҫ���ŵ�����
             method       ���ŵķ���
   ��������ֵ��data       ���ź������
   ��ʾ������train <- Scaling(train, 'UVScaling')
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
  "�������ƣ�decisionTree
   �������ã��������㷨
   ����������seed         �������
   ��������ֵ����
   ��ʾ������decisionTree(2)
  "
}
factorNum <- length(levels(data$class))
decisionTree <- function(seed = 1) {
  cvLabel <- label(seed)  # ����α�������
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
  "�������ƣ�fileOutput
   �������ã���׼ȷ�ʺͻ�������������ļ�
   ����������times         �Ѿ�������֤�Ĵ���
   ��������ֵ����
   ��ʾ������fileOutput(1)
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

confusionMatrixList <- list()  # �洢�Ż��������׼ȷ��
for (i in seq(cvTimes)) {
  pastTime = proc.time()
  confusionMatrixList <- c(confusionMatrixList, decisionTree(seed = i))
  fileOutput(times = i)
  nowTime = proc.time()
  print(nowTime - pastTime)
}
############################################################�㷨ִ���������


################################���ݷ����Լ�������ļ�###########################



###########################################################���ݷ����Լ�������ļ�����
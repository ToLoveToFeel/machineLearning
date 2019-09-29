################################���ݰ�����###########################
# ������б���
rm(list = ls())
library(caret)         # Ԥ������
library(rpart)         # ������
library(rpart.plot)    # ��������ͼ��ذ�
library(pROC)
###########################################################�������



################################����Ԥ����###########################
# ��������
filePath = './PimaIndiansDiabetes2.csv'  # �ļ�·��
data <- read.table(file = filePath, sep = ',')
# ����ת�ã���֤ÿһ��Ϊһ��Ϊһ��������ÿһ��Ϊһ������
data <- t(data)
# ת�ú����ݱ�Ϊ�����ٴν�����ת��Ϊ����֡
data <- as.data.frame(data)
# �鿴���ݸ�ʽ
head(data)
#########################����Ϊ��ѡ����
# ��������
changeColumnName = TRUE  # ����ΪTRUE������������
dataSize <- dim(data)
if (changeColumnName) {
  for (i in seq(dataSize[2])) {
    names(data)[i] <- paste(data[1,i])
  }
}
# ɾ��ĳЩ��
deleteRow <- TRUE  # ����ΪTRUEɾ��ĳЩ��
deleteRowNumber <- c(1) # ɾ�����б��
if (deleteRow) {
  for (i in deleteRowNumber) {
    data <- data[-i, ]
  }
}
# ɾ��ĳЩ��
deleteColumn <- TRUE  # ����ΪTRUEɾ��ĳЩ��
deleteColumnNumber <- c(1) # ɾ�����б��
if (deleteRow) {
  for (i in deleteRowNumber) {
    data <- data[ , -i]
  }
}
# �����б�
changeRowName = TRUE  # ����ΪTRUE���������б�
dataSize <- dim(data)
if (changeColumnName) {
  for (i in seq(dataSize[1])) {
    row.names(data)[i] <- paste(i)
  }
}
#########################��ѡ���ݽ���
# ת�ú�ÿһ������ȫ����Ϊfactor���ͣ�������ʵ������ת��
dataConvertStart <- 1  # ��������ת����ʼλ��
dataConvertEnd <- 8  # ��������ת����ֹλ��
dataConvertType <- 'numeric'  # 'numeric', 'factor', ...
for (i in seq(dataConvertStart, dataConvertEnd)) {
  if ('numeric' == dataConvertType) {
    data[ , i] <- as.numeric(data[ , i])
  } else if ('factor' == dataConvertType) {
    data[ , i] <- as.factor(data[ , i])
  } 
}
data[ , 9] <- as.character(data[ , 9])  # ����תΪfactor��Ҫ��������
data[ , 9] <- as.factor(data[ , 9])
# �鿴���ݸ�ʽ
head(data)
###########################################################Ԥ��������



################################�㷨ִ������###########################
# ��׼����scaleddata�д洢�ű�׼����Ľ��
preProcValues <- preProcess(data[ , -9], method = c("center", "scale")) # �ú���Ĭ���޳�����ȱʧֵ�����ݺ���ʣ�µ����ݼ����ֵ�ͱ�׼��
scaleddata <- predict(preProcValues, data[ , -9])
# �۲�������ķֲ��Լ���Ŀ�����֮��Ĺ�ϵ
featurePlot(scaleddata, data[ , 9], plot = 'box')
# YeoJohnsonת��������������̬�ֲ���£��boxdata�洢�Ŵ�����Ľ��
preProcbox <- preProcess(scaleddata, method = c('YeoJohnson'))
boxdata <- predict(preProcbox, scaleddata)
# ȱʧֵ�岹
preProcimp <- preProcess(boxdata, method = "bagImpute")
procdata <- predict(preProcimp, boxdata)
procdata$class <- data[ , 9]


# ������֤
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,  # 10�ؽ�����֤
                           repeats = 10   # �ظ�10�ؽ�����֤10��
)
tunedf <- data.frame(.cp = seq(0.001, 0.1, length.out = 10)) # ���Դ�0.001��0.1�ĵȾ��10������
treemodel <- train(x = procdata[ , -9],
                   y = procdata[ , 9],
                   method = 'rpart',  
                   trControl = fitControl,
                   tuneGrid = tunedf
)
# ###########################################################�㷨ִ���������



################################���ݷ����Լ�����###########################
treemodel
plot(treemodel)

ggplot(treemodel)
# ROC����
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
###########################################################���ݷ����Լ�����






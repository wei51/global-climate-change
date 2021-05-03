library(tree)
library(rpart)
library(rpart.plot) 
load("./all_info.RData")

df <- data
data <- df[ ,c("dMSS","AAM","BL","DR","LRS","TL","HA")]
cart.model <- rpart(dMSS~., data=data)
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE # 讓樹枝以垂直方式呈現
    #shadow.col="gray"  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
  )  
printcp(cart.model)


library(caret)
library(e1071)
# 選則resampling的方法
train_control <- trainControl(method = "cv",number = 10) # k = 10
# specify the model 
train_control.model <- train(survived ~ ., data = trainingData, method = 'rpart',na.action = na.pass, trControl = train_control)
train_control.model

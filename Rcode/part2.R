dat<-read.csv("IBM data.csv" , header=T , stringsAsFactors = T)

library(ROSE)
library(tree)
library(ROCR)

set.seed(1)

index<-sample(1:nrow(dat),nrow(dat)*0.7)
train<-dat[index,]
test<-dat[-index,]

table(train$Attrition)

##original
tree.ori<-tree(Attrition~.,data=train)
plot(tree.ori);text(tree.ori)
cv.ori.fit<-cv.tree(tree.ori,K=5)
plot(cv.ori.fit$size,cv.ori.fit$dev,type='b')
m<-min(cv.ori.fit$dev)
points(which(cv.ori.fit$dev==m),m,col="red",pch=16)

prune.ori.fit<-prune.tree(tree.ori,best=8)
plot(prune.ori.fit);text(prune.ori.fit)

pred.tree.ori <- predict(tree.ori, newdata = test)
pred.hat<-ifelse(pred.tree.ori[,1]>0.5,0,1)
ct<-table(pred.hat,test$Attrition)
sum(diag(ct))/sum(ct)
roc.curve(test$Attrition, pred.tree.ori[,2])

##oversampling
data.over <- ovun.sample(Attrition~., data = train, method="over",N=table(train$Attrition)[1]*2)$data
table(data.over$Attrition)

tree.over<-tree(Attrition~.,data=data.over)
plot(tree.over);text(tree.over)
cv.over.fit<-cv.tree(tree.over,K=5)
plot(cv.over.fit$size,cv.over.fit$dev,type='b')

pred.tree.over <- predict(tree.over, newdata = test)
pred.hat<-ifelse(pred.tree.over[,1]>0.5,0,1)
ct<-table(pred.hat,test$Attrition)
sum(diag(ct))/sum(ct)
roc.curve(test$Attrition, pred.tree.over[,2])

##undersampling
data.under <- ovun.sample(Attrition~.,data=train,method = "under",N=table(train$Attrition)[2]*2,seed = 1)$data
table(data.under$Attrition)

tree.under<-tree(Attrition~.,data=data.under)
plot(tree.under);text(tree.under)
cv.under.fit<-cv.tree(tree.under,K=5)
plot(cv.under.fit$size,cv.under.fit$dev,type='b')
prune.under.fit<-prune.tree(tree.under,best=7)
plot(prune.under.fit);text(prune.under.fit)

pred.tree.under <- predict(prune.under.fit, newdata = test)
roc.curve(test$Attrition, pred.tree.under[,2])$auc

##rose
data.rose <- ROSE(Attrition ~ ., data = train, seed = 1)$data
table(data.rose$Attrition)

tree.rose<-tree(Attrition~.,data=data.rose)
plot(tree.rose);text(tree.rose)
cv.rose.fit<-cv.tree(tree.rose,K=5)
plot(cv.rose.fit$size,cv.rose.fit$dev,type='b')
m<-min(cv.rose.fit$dev)
points(which(cv.rose.fit$dev==m),m,col="red",pch=16)
prune.rose.fit<-prune.tree(tree.rose,best=8)

plot(prune.rose.fit);text(prune.rose.fit)

pred.tree.rose <- predict(prune.rose.fit, newdata = test)
pred.hat<-ifelse(pred.tree.rose[,1]>0.5,0,1)
ct<-table(pred.hat,test$Attrition)
sum(diag(ct))/sum(ct)
roc.curve(test$Attrition, pred.tree.rose[,2])

##SMOTE
library(DMwR)
newData <- SMOTE(Attrition ~ ., train, perc.over = 600, perc.under=100)
table(newData$Attrition)

tree.smote<-tree(Attrition~.,data=newData)
plot(tree.smote);text(tree.smote)
cv.smote.fit<-cv.tree(tree.smote,K=5)
plot(cv.smote.fit$size,cv.smote.fit$dev,type='b')
prune.smote.fit<-prune.tree(tree.smote,best=10)
plot(prune.smote.fit);text(prune.smote.fit)

pred.tree.smote <- predict(prune.smote.fit, newdata = test)
pred.hat<-ifelse(pred.tree.rose[,1]>0.5,0,1)
ct<-table(pred.hat,test$Attrition)
sum(diag(ct))/sum(ct)
roc.curve(test$Attrition, pred.tree.rose[,2])

#random forest - after rose 
library(randomForest)
rf.fit<-randomForest(Attrition~.,data=data.rose,ntree=500,mtry=5)
varImpPlot(rf.fit)

pred.rf<-predict(rf.fit,newdata=test)
pred<-prediction(as.integer(pred.rf),test$Attrition)
auc<-performance(pred,measure = "auc")
auc<-auc@y.values[[1]]
auc
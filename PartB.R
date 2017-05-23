rm(list=ls())

##reading data

data.wine <- read.csv('C:/ISEN 613/Final/Dataset1.csv',header = T,sep=',')
data.wine$quality <- as.factor(data.wine$quality)
str(data.wine)
sum(is.na(data.wine))
summary(data.wine)
dim(data.wine)
dev.new()
pairs(data.wine)
dev.off()

## split into training and test
set.seed(1001)
train <- sample(nrow(data.wine),600)
data.train <- data.wine[train,]
data.test <- data.wine[-train,]


### fitting logistic regression model
logit.reg <- glm(data.train$quality ~ .,data = data.train,family = binomial)
summary(logit.reg)
logit.probs <- predict(logit.reg,data.test,type='response')
logit.pred <- rep(0,nrow(data.test))
logit.pred[logit.probs >= .35] =1
table(logit.pred,data.test$quality)

library(car)
vif(logit.reg)

logit.reg1 <- glm(data.train$quality ~ .-den,data = data.train,family = binomial)
vif(logit.reg1)

logit.reg2 <- glm(data.train$quality ~ .-den-col,data = data.train,family = binomial)
summary(logit.reg2)
vif(logit.reg2)

logit.probs <- predict(logit.reg2,data.test,type='response')


threshold <- seq(from = 0, to = .5, by = 0.01)
Accuracy <- rep(0,length(threshold))
bacc <- rep(0,length(threshold))
sens <- as.numeric(rep(0,length(threshold)))
spec <- as.numeric(rep(0,length(threshold)))

## BACC (balanced accuracy) curve for imbalance distribution
for(i in 1:length(threshold)){
logit.pred <- rep(0,nrow(data.test))
logit.pred[logit.probs >= threshold[i]] =1
sens[i] <- mean(logit.pred[data.test$quality==1]==1)
spec[i] <- mean(logit.pred[data.test$quality==0]==0)
bacc[i] <- .40*sens[i]+.60*spec[i]
}

plot(threshold,bacc,type="l",ylab='balanced accuracy',main='BACC Curve')
threshold[which.max(bacc)]
bacc[which.max(bacc)]

logit.pred <- rep(0,nrow(data.test))
logit.pred[logit.probs >= .22] =1
cm <- table(data.test$quality,logit.pred)
sens <- mean(logit.pred[data.test$quality==1]==1)
spec <- mean(logit.pred[data.test$quality==0]==0)
accuracy <- (cm[1,1]+cm[2,2])/sum(cm)


### trees with pruning
library(tree)
data.wine$qualitycls <- as.factor(ifelse(data.wine$quality=='0','Low',"High"))
set.seed(1001)
train <- sample(nrow(data.wine),600)
data.train <- data.wine[train,]
data.test <- data.wine[-train,]
tree.wine <- tree(qualitycls~.-quality,data.train)

dev.new()
plot(tree.wine)
text(tree.wine,pretty=0,cex=.6) 
dev.off()

pred.tree <- predict(tree.wine,data.test,type='class')
cm <- table(data.test$qualitycls,pred.tree)

sens <- cm[1,1]/(cm[1,1]+cm[1,2])
spec <- cm[2,2]/(cm[2,1]+cm[2,2])
accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
bacc <-  .40*sens+.60*spec


##pruning
set.seed(1001)
cv.wine <- cv.tree(tree.wine,FUN = prune.misclass)
cv.wine$dev
cv.wine$size
plot(cv.wine$size,cv.wine$dev,type='b',main='selecting best tree depth')

prune.wine <- prune.misclass(tree.wine,best=10)
dev.new()
plot(prune.wine)
text(prune.wine,pretty=0)
dev.off()

pred.prune <- predict(prune.wine,data.test,type='class')
cm <- table(data.test$qualitycls,pred.prune)

sens <- cm[1,1]/(cm[1,1]+cm[1,2])
spec <- cm[2,2]/(cm[2,1]+cm[2,2])
accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
bacc <-  .40*sens+.60*spec



## knn

library(class)
data.wine$colnum <- as.factor(ifelse(data.wine$col=='white','1',"0"))
set.seed(1001)
train <- sample(nrow(data.wine),600)
data.train <- data.wine[train,]
data.test <- data.wine[-train,]

train.X <- data.train[,-c(12,13,14)]
test.X <- data.test[,-c(12,13,14)]

quality.train <- data.train$quality
quality.test <- data.test$quality

bacc <- rep(0,length(threshold))
sens <- as.numeric(rep(0,length(threshold)))
spec <- as.numeric(rep(0,length(threshold)))


for(i in 1:100){
  set.seed(1001)
  knn.pred <- knn(data.frame(train.X),data.frame(test.X),quality.train,k=i)
  sens[i] <- mean(knn.pred[data.test$quality==1]==1)
  spec[i] <- mean(knn.pred[data.test$quality==0]==0)
  bacc[i] <- .50*sens[i]+.50*spec[i]
}
plot(1:100,bacc,xlab="K value",ylab="Accuracy",type="l",main="K VS bacc")
which.max(bacc)

set.seed(1001)
knn.pred <- knn(data.frame(train.X),data.frame(test.X),quality.train,k=1)
cm <- table(quality.test,knn.pred)
spec <- cm[1,1]/(cm[1,1]+cm[1,2])
sens <- cm[2,2]/(cm[2,1]+cm[2,2])
accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
bacc <-  .40*sens+.60*spec

## lda

### select variables
pairs(data.wine)
boxplot(data.wine$fix ~ data.wine$quality)
boxplot(data.wine$vol ~ data.wine$quality)##
boxplot(data.wine$cit ~ data.wine$quality)#
boxplot(data.wine$res ~ data.wine$quality)#
boxplot(data.wine$chl ~ data.wine$quality)
boxplot(data.wine$fre ~ data.wine$quality)
boxplot(data.wine$tot ~ data.wine$quality)
boxplot(data.wine$den ~ data.wine$quality)##
boxplot(data.wine$pH ~ data.wine$quality)##
boxplot(data.wine$sul ~ data.wine$quality)
boxplot(data.wine$alc ~ data.wine$quality)##

###LDA

library(MASS)

lda.fit <- lda(quality~.-col-qualitycls,data=data.train)
lda.pred <- predict(lda.fit,data.test)

cm.lda<- table(data.test$quality,lda.pred$class)
accuracy <- (cm.lda[1,1]+cm.lda[2,2])/sum(cm.lda)
sens <- mean(lda.pred$class[data.test$quality==1]==1)
spec <- 1-mean(lda.pred$class[data.test$quality==0]==1)
bacc <-  .40*sens+.60*spec

## with few variables.

lda.fit1 <- lda(quality~ vol+cit+res+den+pH+alc ,data=data.train)
lda.pred1 <- predict(lda.fit1,data.test)

cm.lda1<- table(data.test$quality,lda.pred1$class)
accuracy <- (cm.lda1[1,1]+cm.lda1[2,2])/sum(cm.lda1)
sens <- mean(lda.pred1$class[data.test$quality==1]==1)
spec <- 1-mean(lda.pred1$class[data.test$quality==0]==1)
bacc <-  .40*sens+.60*spec

###qda
qda.fit <- qda(quality~.-col-qualitycls,data=data.train)
qda.pred <- predict(qda.fit,data.test)

cm.qda<- table(data.test$quality,qda.pred$class)
accuracy <- (cm.qda[1,1]+cm.qda[2,2])/sum(cm.qda)
sens <- mean(qda.pred$class[data.test$quality==1]==1)
spec <- 1-mean(qda.pred$class[data.test$quality==0]==1)
bacc <-  .40*sens+.60*spec

## less variables

qda.fit1 <- qda(quality~ vol+cit+res+den+pH+alc ,data=data.train)
qda.pred1 <- predict(qda.fit1,data.test)

cm.qda1<- table(qda.pred1$class,data.test$quality)


## bagging

library(randomForest)
set.seed(1001)

trees <- seq(5,200,by=10)
accuracy <- rep(0,length(trees))

for(i in 1:length(trees)){
  bag.wine=randomForest(quality~.-col-qualitycls,data=data.train,mtry=12,ntree=trees[i], importance=TRUE)
  yhat.bag=predict(bag.wine,newdata=data.test,type='class')
  cm.bag <- table(data.test$quality,yhat.bag)
  accuracy[i] <- (cm.bag[1,1]+cm.bag[2,2])/sum(cm.bag)
}

trees[which.max(accuracy)]
set.seed(1001)
bag.wine=randomForest(quality~.-col-qualitycls,data=data.train,mtry=12,ntree=85, importance=TRUE)
yhat.bag=predict(bag.wine,newdata=data.test,type='class')
cm.bag <- table(data.test$quality,yhat.bag)
accuracy <- (cm.bag[1,1]+cm.bag[2,2])/sum(cm.bag)
spec <- cm.bag[1,1]/(cm.bag[1,1]+cm.bag[1,2])
sens <- cm.bag[2,2]/(cm.bag[2,1]+cm.bag[2,2])
bacc <-  .40*sens+.60*spec



yhat.bag12 <- predict(bag.wine,newdata=valid.test,type='class')

plot(trees,accuracy,type='b',main='Bagging accuracy with different tree sizes')

## random forests

accuracy <- rep(0,12)
set.seed(1001)
for(i in 1:12){
  bag.wine=randomForest(quality~.-col-qualitycls,data=data.train,mtry=i,ntree=165, importance=TRUE)
  yhat.bag=predict(bag.wine,newdata=data.test,type='class')
  cm.bag <- table(data.test$quality,yhat.bag)
  accuracy[i] <- (cm.bag[1,1]+cm.bag[2,2])/sum(cm.bag)
}

plot(1:12,accuracy,type='b',main='Accuracy for different ntree')

## for 5 tree depth select no of trees
trees <- seq(5,200,by=10)
accuracy <- rep(0,length(trees))

set.seed(1001)
for(i in 1:length(trees)){
  bag.wine=randomForest(quality~.-col-qualitycls,data=data.train,mtry=5,ntree=trees[i], importance=TRUE)
  yhat.bag=predict(bag.wine,newdata=data.test,type='class')
  cm.bag <- table(data.test$quality,yhat.bag)
  accuracy[i] <- (cm.bag[1,1]+cm.bag[2,2])/sum(cm.bag)
}

plot(trees,accuracy,type='b',main='accuracy with different tree sizes for tree depth = 5')
trees[which.max(accuracy)]

set.seed(1001)
bag.wine=randomForest(quality~.,data=data.train[,-c(12,14)],mtry=5,ntree=105, importance=TRUE)
yhat.bag=predict(bag.wine,newdata=data.test,type='class')
cm.bag <- table(data.test$quality,yhat.bag)
accuracy <- (cm.bag[1,1]+cm.bag[2,2])/sum(cm.bag)
spec <- cm.bag[1,1]/(cm.bag[1,1]+cm.bag[1,2])
sens <- cm.bag[2,2]/(cm.bag[2,1]+cm.bag[2,2])
bacc <-  .40*sens+.60*spec






yhat.bag1 <-predict(bag.wine,newdata=valid.test,type='class') 

###boosting
library(gbm)
set.seed(1001)
boost.wine <- gbm(quality~.,data = data.train[,-c(12,14)],distribution = 'multinomial',n.trees=5000,interaction.depth = 4,shrinkage = .005,cv.folds = 10)
pred.boost <- predict(boost.wine,data.test,n.trees=5000)
pred1.boost <- apply(pred.boost, 1, which.max)
pred1.boost[pred1.boost==1] <- 0
pred1.boost[pred1.boost==2] <- 1
cm.boost <- table(data.test$quality,pred1.boost)
accuracy <- (cm.boost[1,1]+cm.boost[2,2])/(sum(cm.boost))
spec <- cm.boost[1,1]/(cm.boost[1,1]+cm.boost[1,2])
sens <- cm.boost[2,2]/(cm.boost[2,1]+cm.boost[2,2])
bacc <-  .40*sens+.60*spec

### prediction of dataset2


valid.test <- read.csv('C:/ISEN 613/Final/Dataset2.csv',header = T,sep=',')
valid.test$colnum <- as.factor(ifelse(valid.test$col=='white','1',"0"))

set.seed(1001)
boost.wine1 <- gbm(quality~.,data = data.wine[,-c(12,14)],distribution = 'multinomial',n.trees=5000,interaction.depth = 4,shrinkage = .005,cv.folds = 10)
pred.boost1 <- predict(boost.wine1,valid.test[,-12],n.trees=5000)
pred1.boost <- apply(pred.boost1, 1, which.max)
pred1.boost[pred1.boost==1] <- 0
pred1.boost[pred1.boost==2] <- 1
pred1.boost


##svm with linear kernel
library(e1071)

set.seed(1001)
tune.out <- tune(svm,quality~.,data=data.train[,-c(12,14)],kernel='linear',ranges=list(cost=c(.01,.1,.5,.10,.50,1,5,10)))
summary(tune.out)
bestmod1 <- tune.out$best.model
summary(bestmod1)

set.seed(1001)
svmfit1 <- svm(quality~.,data=data.train[,-c(12,14)],kernel="linear",cost=5,scale=FALSE)
svm.pred1 <- predict(svmfit1,data.test,type="class")
cm.svm <- table(data.test$quality,svm.pred1)
accuracy <- (cm.svm[1,1]+cm.svm[2,2])/(sum(cm.svm))
spec <- cm.svm[1,1]/(cm.svm[1,1]+cm.svm[1,2])
sens <- cm.svm[2,2]/(cm.svm[2,1]+cm.svm[2,2])
bacc <-  .40*sens+.60*spec

##svm with radial kernel
set.seed(1001)
svm.tun.rad <- tune(svm,quality~.,data=data.train[,-c(12,14)],kernel='radial',ranges=list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3)))
bestmod.rad <- svm.tun.rad$best.model
summary(bestmod.rad)

svm.pred.radb <- predict(svm.tun.rad$best.model,data.test,type='class')
cm.svm <- table(data.test$quality,svm.pred.radb)
accuracy <- (cm.svm[1,1]+cm.svm[2,2])/(sum(cm.svm))
spec <- cm.svm[1,1]/(cm.svm[1,1]+cm.svm[1,2])
sens <- cm.svm[2,2]/(cm.svm[2,1]+cm.svm[2,2])
bacc <-  .40*sens+.60*spec

##svm with polynomial kernel

set.seed(1001)
svm.tun.poly <- tune(svm,quality~.,data=data.train[,-c(12,14)],kernel='polynomial',ranges=list(cost=c(0.1,1,10,100),degree=c(2,3,4,5)))
summary(svm.tun.poly)
bestmod.tun.poly <- svm.tun.poly$best.model

svm.pred.polyb <- predict(svm.tun.poly$best.model,data.test,type='class')
cm.svm <- table(data.test$quality,svm.pred.polyb)
accuracy <- (cm.svm[1,1]+cm.svm[2,2])/(sum(cm.svm))
spec <- cm.svm[1,1]/(cm.svm[1,1]+cm.svm[1,2])
sens <- cm.svm[2,2]/(cm.svm[2,1]+cm.svm[2,2])
bacc <-  .40*sens+.60*spec



## Model Fitting in R
### Ordinary Logistic Regression, Logistic Regression with Ridge penalty, 
### Logistic Regression with LASSO, Feature Selection, Elastic Net Logistic Regression, 
### K Nearest Neighbors, Naive



# load packages
require(corrplot)
require(ISLR)
require(MASS)
require(glmnet)
require(dplyr)
require(caret)
require(glmnet)
require(pROC)
require(FSelector)
require(infotheo)
require(kknn)
library(e1071)
library(caTools)
library(Metrics)
require(klaR)

spam = read.csv("spambase_csv.csv") ; originalData = spam
spam = unique(spam)

n = nrow(spam)
p = ncol(spam)

x = spam[,-p]
y = factor(ifelse(spam[,p] == 1, "spam", "not spam"))

highCorr = findCorrelation(cor(x), .85, names = TRUE)
print(highCorr)

x = subset(x,select = -c(word_freq_857))

trainIndex = createDataPartition(y, p = 0.8, list = FALSE)


##### Ordinary Logistic regression 
##### Preprocess : None

Xtrain = x[trainIndex,]
Xtest = x[-trainIndex,]
Ytrain = y[trainIndex]
Ytest  = y[-trainIndex]

trControl    = trainControl(method = 'none')
outLogistic  = train(x = Xtrain, y = Ytrain, 
                     method = 'glm', trControl = trControl)

YhatTestProb =  predict(outLogistic, Xtest, type = 'prob')

sum(Ytrain == predict(outLogistic, Xtrain, type = 'raw'))/length(Ytrain)
sum(Ytest == predict(outLogistic, Xtest, type = 'raw'))/length(Ytest)


##### Ridge Logistic regression 
##### Preprocess : None
##### For feature selection

K            = 10
trainControl = trainControl(method = "cv", number = K)

tuneGrid     = expand.grid( alpha = 0, lambda = seq(0, 1, length.out = 100))

outRidge = train(x = Xtrain, 
                 y = Ytrain,
                 method = "glmnet", 
                 trControl = trainControl, 
                 tuneGrid = tuneGrid
)

outRidge$bestTune

##### Lasso Logistic regression 
##### Preprocess : None
##### For feature selection

tuneGrid     = expand.grid( alpha = 1, lambda = seq(0, 1, length.out = 100))

outLasso = train(x = Xtrain, 
                 y = Ytrain,
                 method = "glmnet", 
                 trControl = trainControl, 
                 tuneGrid = tuneGrid
                 )

outLasso$bestTune

### Ridge/Lasso NOT effective in this case


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

Xqual = x[,1:(ncol(x)-3)]
Xquan = x[,(ncol(x)-2):ncol(x)]

# Convert to Binary Variables
Xqual = as.data.frame(ifelse(Xqual > 0, 1, 0))

### Quanitative data Preprocess
Xquan = Xquan %>%
  preProcess(method  = 'center', 'scale', 'YeoJohnson') %>%
  predict(newdata    = Xquan)


### Categorical Feature selection

# Chi squared test feature selection
temp = cbind(Xqual,y)
Chi2Score<- chi.squared(y~., temp)
print(Chi2Score)
colorTemp = ifelse(Chi2Score > 0.4,"Accept","Decline")
ggplot(data = Chi2Score, aes(x= 1:ncol(Xqual),y = attr_importance, fill = colorTemp)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_manual("legend", values=c("Accept" = 'gold1', "Decline" = "grey")) + 
  theme(legend.position = "none") + 
  geom_line(y=0.4, lty = 2) +
  labs(x = "Categorical Features", y = "Feature Importance")

subset<- cutoff.k(Chi2Score, 4)
newXqual = subset(Xqual, select = c(subset))
rm(temp,colorTemp)


### Mutual Information Score feature selection
#MIScore = rep(0,ncol(Xqual))
#for(i in 1:ncol(Xqual)){
#  MIScore[i] = mutinformation(Xqual[,i], y)
#}
#ggplot(data = data.frame(MIScore), aes( x= 1:ncol(Xqual),y = MIScore)) + geom_bar(stat = 'identity') + geom_line(y=0.07, lty = 2)

reducedx = cbind(newXqual,Xquan)
newx = cbind(Xqual,Xquan)


##### Ordinary Logistic regression 
##### Preprocess : Yes

Xtrain = newx[trainIndex,]
Xtest = newx[-trainIndex,]
Ytrain = y[trainIndex]
Ytest  = y[-trainIndex]

trControl    = trainControl(method = 'none')
outLogistic  = train(x = Xtrain, y = Ytrain, 
                     method = 'glm', trControl = trControl)

YhatTestProb =  predict(outLogistic, Xtest, type = 'prob')

sum(Ytrain == predict(outLogistic, Xtrain, type = 'raw'))/length(Ytrain)
sum(Ytest == predict(outLogistic, Xtest, type = 'raw'))/length(Ytest)



##### Ridge Logistic regression 
##### Preprocess : Yes

K            = 10
trainControl = trainControl(method = "cv", number = K)

tuneGrid     = expand.grid(alpha = 0, lambda = seq(0, 1, length.out = 100))

outRidge = train(x = Xtrain, 
                 y = Ytrain,
                 method = "glmnet", 
                 trControl = trainControl, 
                 tuneGrid = tuneGrid
)

outRidge$bestTune



##### Lasso Logistic regression 
##### Preprocess : Yes

tuneGrid     = expand.grid(alpha = 1, lambda = seq(0,1, length.out = 100))

outLasso = train(x = Xtrain, 
                 y = Ytrain,
                 method = "glmnet", 
                 trControl = trainControl, 
                 tuneGrid = tuneGrid
)

outLasso$bestTune

##### Elastic Net Logistic regression 
##### Preprocess : Yes

tuneGrid     = expand.grid(lambda = seq(1e-6, 0.25, length = 100),
                           alpha  = c(0, 0.25, 0.5, 0.75, 1))

outElasticNet = train(x = Xtrain, 
                 y = Ytrain,
                 method = "glmnet", 
                 trControl = trainControl, 
                 tuneGrid = tuneGrid
)

outElasticNet$bestTune

plot(outElasticNet, xlab = "Penalty(λ)", ylab = 'K-fold CV')

matplot(x = outElasticNet$finalModel$lambda, t(outElasticNet$finalModel$beta),
        type='l', ylab='Coefficient Path', xlab = 'glmnet lambda grid')
abline(v = outElasticNet$bestTune$lambda)


ElaNetFinal     = glmnet(x = Xtrain, y = Ytrain, alpha = outElasticNet$bestTune$alpha,
                        family = 'binomial', standardize = FALSE)

probYElaNet    = predict(ElaNetFinal, as.matrix(Xtest), s=outElasticNet$bestTune$lambda, type = 'response')
yHatElaNet    = predict(ElaNetFinal, as.matrix(Xtest), s=outElasticNet$bestTune$lambda, type = 'class')

mean(Ytest == yHatElaNet)

table(yHatElaNet, Ytest)

rocElaNet <- roc(Ytest, probYElaNet)
rocElaNet$auc
ggroc(rocElaNet,colour = 'steelblue', size = 2) +
  theme_minimal()
ElaNetPrecisionprecision(predicted, actual)

ElaNetprecision = precision(ifelse(Ytest == "spam", 1, 0),ifelse(yHatElaNet == "spam", 1, 0))
ElaNetrecall = recall(ifelse(Ytest == "spam", 1, 0),ifelse(yHatElaNet == "spam", 1, 0))
ElaNetf1 = 2*ElaNetprecision * ElaNetrecall/(ElaNetprecision + ElaNetrecall)
print(c(ElaNetprecision,ElaNetrecall,ElaNetf1))
## Elastic Net doesn't work well


##### KNN
##### Preprocess : Yes
  
Xtrain = newx[trainIndex,]
Xtest = newx[-trainIndex,]
Ytrain = y[trainIndex]
Ytest  = y[-trainIndex]

trControl = trainControl(method = "cv", number = 10)

tuneGrid = expand.grid(data.frame(k = seq(5,20)))
knnOut = train(x = Xtrain, y = Ytrain,
               method = "knn",
               trControl = trControl,
               tuneGrid =  tuneGrid,
               preProc = c("center", "scale", "YeoJohnson")
               )
plot(knnOut)
knnOut$bestTune

mean(Ytest == predict(knnOut,Xtest, type = 'raw'))

probYKNN    = predict(knnOut, as.matrix(Xtest), type = 'prob')
yHatKNN    = predict(knnOut, as.matrix(Xtest), type = 'raw')

mean(Ytest == yHatKNN)

table(yHatKNN, Ytest)

rocKNN <- roc(Ytest, probYKNN[,2])
rocKNN$auc
ggroc(rocElaNet,colour = 'steelblue', size = 2) +
  theme_minimal()

KNNprecision = precision(ifelse(Ytest == "spam", 1, 0),ifelse(yHatKNN == "spam", 1, 0))
KNNrecall = recall(ifelse(Ytest == "spam", 1, 0),ifelse(yHatKNN == "spam", 1, 0))
KNNf1 = 2*KNNprecision * KNNrecall/(KNNprecision + KNNrecall)
print(c(KNNprecision,KNNrecall,KNNf1))


##### Naive Bayes
##### Preprocess : None


Xtrain = newx[trainIndex,]
Xtest = newx[-trainIndex,]
Ytrain = y[trainIndex]
Ytest  = y[-trainIndex]

tuneGrid = expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

NBout <- train(
  x = Xtrain,
  y = Ytrain,
  method = "nb", 
  trControl = trainControl,
  tuneGrid = tuneGrid,
  preProc = c("center","scale","YeoJohnson","pca")
)

plot(NBout)
NBout$bestTune
mean(Ytest == predict(NBout,Xtest))


probYNB    = predict(NBout, as.matrix(Xtest), type = 'prob')
yHatNB    = predict(NBout, as.matrix(Xtest), type = 'raw')

mean(Ytest == yHatNB)

table(yHatNB, Ytest)

rocNB <- roc(Ytest, probYNB[,2])
rocNB$auc
ggroc(rocNB,colour = 'steelblue', size = 2) +
  theme_minimal()

NBprecision = precision(ifelse(Ytest == "spam", 1, 0),ifelse(yHatNB == "spam", 1, 0))
NBrecall = recall(ifelse(Ytest == "spam", 1, 0),ifelse(yHatNB == "spam", 1, 0))
NBf1 = 2*NBprecision * NBrecall/(NBprecision + NBrecall)
print(c(NBprecision,NBrecall,NBf1))

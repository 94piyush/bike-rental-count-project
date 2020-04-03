##################################    RENTAL COUNT  ####################################



#----------------------------------importing data---------------------------------------------
dataset<-read.csv("day.csv")

#---------------------------------Encoding variable-------------------------------------------

#Since date has an order, it is considered an ordinal variable
str(dataset)

#we will convert date into numerical variable
dataset$dteday<-as.numeric(gsub('-',"",dataset$dteday))

#-----------------------------histograms  and boxplot before outlier anlaysis------------------
par(mfrow=c(4,4))
for(i in 1:15){
  hist(dataset[,i],
       xlab="",
       main=colnames(dataset)[i],
       col="grey",
       border='white',
       probability = T)
  d<-density(dataset[,i])
  lines(d, col='red')
}

par(mfrow=c(4,4))
for(i in 1:15){
  boxplot(dataset[,i],
          main=colnames(dataset)[i],
          col=c('grey'),
          horizontal=T,border = 'indianred')
}

#--------------------------------Outlier analysis---------------------------------------

ds<- dataset

#######droping outlairs and replacing it with mean

#VARIABLE HUM
val=dataset$hum[dataset$hum %in% boxplot.stats(dataset$hum)$out]
dataset$hum[dataset$hum %in% val]=NA
dataset$hum[is.na(dataset$hum)]=mean(dataset$hum,na.rm=T)

#VARIABLE WINDSPEED
val=dataset$windspeed[dataset$windspeed %in% boxplot.stats(dataset$windspeed)$out]
dataset$windspeed[dataset$windspeed %in% val]=NA
dataset$windspeed[is.na(dataset$windspeed)]=mean(dataset$windspeed,na.rm = T)

#VARIABLE CASUAL
val=dataset$casual[dataset$casual %in% boxplot.stats(dataset$casual)$out]
dataset$casual[dataset$casual %in% val]=NA
dataset$casual[is.na(dataset$casual)]=mean(dataset$casual,na.rm=T)



#------------------------histograms  and boxplot after outlier anlaysis-------------
par(mfrow=c(4,4))
for(i in 1:15){
  hist(dataset[,i],
       xlab="",
       main=colnames(dataset)[i],
       col="grey",
       border='white',
       probability = T)
  d<-density(dataset[,i])
  lines(d, col='red')
}

par(mfrow=c(4,4))
for(i in 1:15){
  boxplot(dataset[,i],
          main=colnames(dataset)[i],
          col=c('grey'),
          horizontal=T,border = 'indianred')
}

#----------------------------------correlational analysis---------------------------
library(corrgram)
corrgram(dataset,
         order=FALSE,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         main='correlational plot')  



#removing multicollinear variables(instant,dteday,season,yr,weathersit,temp,atemp)
dataset=subset(dataset,select = -c(instant,dteday,season,yr,weathersit,temp,atemp))


#----------------------------------feature scaling---------------------------------
dataset[-9]<-scale(dataset[-9])

#----------------------------------dividing data into train and split-------------
library(caTools)
set.seed(123)
split<-sample.split(dataset$cnt,SplitRatio = 0.8)
training_set<-subset(dataset,split==TRUE)
test_set<-subset(dataset,split==FALSE)


#---------------------------------using MULTIPLE LINEAR REGRESSION----------------
library(caret)
regressor_lm<-lm(formula = cnt~.,
              data = training_set)

summary(regressor_lm)
y_pred_lm<-predict(regressor_lm,test_set[-9])
R2(y_pred_lm,test_set[9])

#---------------------------------------backward elemination----------------------
backwardElimination <- function(x,sl){
  numVar <- length(x)
  for(i in c(1:numVar)){
    regressor_lm <- lm(formula= cnt ~.,data= x)
    maxVar <- max(coef(summary(regressor_lm))[c(2:numVar),"Pr(>|t|)"])
    if(maxVar > sl){
      j <- which(coef(summary(regressor_lm))[c(2:numVar),"Pr(>|t|)"]==maxVar)
      x <- x[,-j]
    }
    numVar = numVar - 1
  }        
  return(summary(regressor_lm))
} 
sl<- 0.05
dataset <- dataset[,c(1,2,3,4,5,6,7,8,9)]
backwardElimination(dataset,sl)

#removing insignificant variables
dataset<-dataset[,c(1,3,4,7,8,9)]
#----------------------dividing data into traain and test set again-----------------
library(caTools)
set.seed(123)
split<-sample.split(dataset$cnt,SplitRatio = 0.8)
training_set<-subset(dataset,split==TRUE)
test_set<-subset(dataset,split==FALSE)

#--------------------------------k folds cross validation--------------------------
library(caret)
folds<-createFolds(training_set$cnt,k=10)
cv<-lapply(folds,function(x){
  training_fold<-training_set[-x,]
  test_fold<-training_set[x,]
  regressor_lm<-lm(cnt~.,
                data = training_fold)
  y_pred_lm<-predict(regressor_lm,test_fold[-6])
  var_explained<-R2(y_pred_lm,test_fold[6])
  return(var_explained)
})

mean(as.numeric(cv))

#----------------------------------using decision tree regression--------------------
library(rpart)
regressor_dt<-rpart(formula = cnt~.,
                  data=training_set,
                  control = rpart.control(minsplit = 2))

y_pred_dt<-predict(regressor_dt,test_set[-6])
R2(y_pred_dt,test_set[6])

#with k fold cross validation
folds1<-createFolds(training_set$cnt,k=10)
cv1<-lapply(folds1, function(x){
  training_fold1<-training_set[-x,]
  test_fold1<-training_set[x,]
  regressor_dt<-rpart(formula = cnt~.,
                           data=training_fold1,
                           control = rpart.control(minsplit = 2))
  y_pred_dt<-predict(regressor_dt,test_fold1[-6])
  variance_explained<-R2(y_pred_dt,test_fold1[6])
  return(variance_explained)
})
mean(as.numeric(cv1))

#--------------------------------------using RANDOM FOREST---------------------------
library(randomForest)
regressor_rf<-randomForest(x=training_set[-6],
                         y=training_set$cnt,
                         ntree=500)

y_pred_rf<-predict(regressor_rf,test_set[-6])
R2(test_set[6],y_pred_rf)



#with k fold cross validation
library(randomForest)
folds2<-createFolds(training_set$cnt,k=10)
cv2<-lapply(folds2, function(x){
  training_fold2<-training_set[-x,]
  test_fold2<-training_set[x,]
  regressor_rf<-randomForest(x=training_fold2[-6],
                          y=training_fold2$cnt,
                          ntree=500)
  y_pred_rf<-predict(regressor_rf,test_fold2[-6])
  variance_explained<-R2(y_pred_rf,test_fold2[6])
  return(variance_explained)
})
mean(as.numeric(cv2))

#--------------------------------------------using XGBOOST-------------------------
library(xgboost)
regressor_xgb<-xgboost(data = as.matrix(training_set[-6]),
                    label = training_set$cnt,
                    nrounds = 1000)
y_pred_xgb<-predict(regressor_xgb,as.matrix(test_set[-6]))
RMSE(y_pred_xgb,test_set[,6])
R2(y_pred_xgb,test_set[,6])
#with k fold cross validation
library(xgboost)
folds3<-createFolds(training_set$cnt,k=10)
cv3<-lapply(folds3, function(x){
  training_folds3<-training_set[-x,]
  test_folds3<-training_set[x,]
  regressor_xgb<-xgboost(data = as.matrix(training_folds3[-6]),
                      label = training_folds3$cnt,
                      nrounds = 1000,eta=0.1,
                      max_depth=15
                      )
  y_pred_xgb<-predict(regressor_xgb,as.matrix(test_folds3[-6]))
  variance_explained<-R2(y_pred_xgb,as.matrix(test_folds3[6]))
  return(variance_explained)
})
mean(as.numeric(cv3))

#----------------------------------------------model evaluation------------------------
library(caret)

#multiple linear regression
RMSE(y_pred_lm,test_set[,6])

#decision tree regression

RMSE(y_pred_dt,test_set[,6])

#random forrest regression

RMSE(y_pred_rf,test_set[,6])

#xgboost regression

RMSE(y_pred_xgb,test_set[,6])




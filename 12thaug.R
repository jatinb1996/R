lsd=read.csv("F:\\finalimarticus\\project1r\\R_Module_Day_5.2_Data_Case_Study_Loss_Given_Default (3).csv")
library(mosaic)
inspect(lsd)
mod_data=lsd[,-1]#exclude the accontno col
names(mod_data)
names(mod_data)[6]="y"#renaming the target variable
N=nrow(mod_data)
N
rand=sample(1:N,N,replace=F)
train_row=rand[rand<=length(rand)*0.8]
training=mod_data[train_row,]
test=mod_data[-train_row,]
cor(training[,c(1,2,3,6)])



sa1=lm(y~Age,training);summary(sa1)
sa2=lm(y~Years.of.Experience,training);summary(sa2)
sa3=lm(y~Gender,training);summary(sa3)
sa4=lm(y~Married,training);summary(sa4)
mlr1=lm(y~Age+Years.of.Experience+Gender+Married,training);summary(mlr1)#years of exp insignificant
mlr2=lm(y~Age+Gender+Married,training);summary(mlr2)


library(lmtest);
library(car)
library(nortest)
library(normtest)

res=mlr2$residuals
t.test(res,mu=0,alternative="two.sided")
#ho is constant variance,higher p value required
ncvTest(mlr2)
#low p value there is a problem with assumption,higher p value required
ad.test(res)
#higher p value required
lillie.test(res)
#higher p value required
durbinWatsonTest(mlr2);
vif(mlr2)
#should be less than 5
test$prediction_loss=predict(mlr2,test)
predictionerror=test$y-test$prediction_loss
mean(predictionerror)
RMSE=sqrt(mean(predictionerror^2));RMSE




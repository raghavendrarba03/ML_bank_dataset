mydata<-read.csv(file="C:\\Users\\Raghavendra Reddy\\Desktop\\bank-full.csv", header=TRUE, sep=";")
nrow(mydata)
str(mydata)
unique(mydata$job)
table(mydata$job)
barplot(table(mydata$job))

mydata1<-subset(mydata,subset=(job!="unknown"))
nrow(mydata1)
barplot(table(mydata$education))
barplot(table(mydata$contact))

mydata2<-subset(mydata1,subset=(education!="unknown"))
nrow(mydata2)

mydata3<-subset(mydata2,subset=(contact!="unknown"))
nrow(mydata3)

table(mydata3$default)
table(mydata3$poutcome=="unknown")

#Variable default, duration and poutcome to be removed
mydata4<-subset(mydata3, select=-c(default, duration, poutcome, contact))
nrow(mydata4)
table(mydata4$y)

#Recoding the variable since R consider 1st variable as point of interest but actually we are interested in Yes.
mydata4$yvar<-ifelse(mydata4$y=="yes",1,2)
table(mydata4$yvar)

#Converting yvar to factor variable
mydata4$yvar<-as.factor(mydata4$yvar)

#Removing the variable y since we have recoded the Y variable.
mydata5<-subset(mydata4, select=-c(y))
table(mydata5$yvar)


ggplot()+geom_bar(data=mydata5, aes(x=(mydata5$age), fill=factor(mydata5$yvar)), position="fill")

#Modeling
set.seed(123)
nrow(mydata5)
datamixed=mydata5[order(runif(30907)), ]
traindata<-datamixed[1:21635, ]
testdata<-datamixed[21636:30907, ]
table(traindata$yvar)

#Descending order for yvar for traindata
ordereddata<-traindata[ order(-xtfrm(traindata$yvar)),]
NROW(ordereddata)

#Model 1
library(C50)
library(caret)
model1<-C5.0(ordereddata$yvar~.,data=ordereddata)
predicted1=predict(model1,testdata[,1:12])
confusionMatrix(predicted1,testdata[,13])

#Model 2
table(ordereddata$yvar)
sampletraindata1<-ordereddata[11130:21635,]
table(sampletraindata1$yvar) #i.e 30% and 70%
model2<-C5.0(sampletraindata1$yvar~.,data= sampletraindata1)
predicted2=predict(model2,testdata[,1:12])
confusionMatrix(predicted2,testdata[,13])

#Model 3
sampletraindata2<-sampletraindata1[4301:10506,]
table(sampletraindata2$yvar) #i.e 50% and 50%
model3<-C5.0(sampletraindata2$yvar~.,data= sampletraindata2)
predicted3=predict(model3,testdata[,1:12])
confusionMatrix(predicted3,testdata[,13])

#ROC Curve
library(ROCR)
Predicted_prob=predict(model3,testdata[,1:12], type="prob")
mult_measures=prediction(Predicted_prob[,2],testdata[,13])
ROC=performance(mult_measures,measure="tpr",x.measure="fpr")
ROC2=performance(mult_measures,measure="auc")
auc= as.numeric(ROC2@y.values)
auc_lgnd = paste(c("AUC", auc),  collapse=" ")
plot(ROC, col="red")
abline(a=0, b=1)
legend(.6,.3, auc_lgnd, lty =1, lwd=1, col="red")

#to sort data 
descdata<-Predicted_prob[order(-Predicted_prob[,1])]
head(descdata)

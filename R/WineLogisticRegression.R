#Wine Type and Quality using loginstic
library(class)
library(stats)

getwd()
mydata <- read.csv("T-61_3050_training_dataset.csv",header=TRUE)
mytestdata <- read.csv("T-61_3050_test_dataset.csv",header=TRUE)

for (i in 1:5000){
  if(mydata[i,13]=='White'){a=0}else{a=1}
  mydata[i,14]<-a
}
for (i in 1:1000){
  if(mytestdata[i,13]=='White'){a=0}else{a=1}
  mytestdata[i,14]<-a
}


#Quality
print('Quality')
logi1<-glm(quality~fixedAcidity + volatileAcidity+citricAcid+ residualSugar+chlorides+freeSulfurDioxide+totalSulfurDioxide+density+pH+sulphates+alcohol,data=mydata[,-c(14)] )
confint(logi1)
ret1<-sum(round(predict(logi1, newdata= mytestdata[,-(13)]))==mytestdata[,12])/1000

#TYPE
print('Type')
logi2<-glm(V14~fixedAcidity + volatileAcidity+citricAcid+ residualSugar+chlorides+freeSulfurDioxide+totalSulfurDioxide+density+pH+sulphates+alcohol,data=mydata[,-c(12)] )
confint(logi2)
aux1<-round(predict(logi2, newdata= mytestdata[,-(12)]))
ret2<-sum(aux1==mytestdata[,14])/1000

#Quality With type
print('Quality with type')
logi3<-glm(quality~fixedAcidity + volatileAcidity+citricAcid+ residualSugar+chlorides+freeSulfurDioxide+totalSulfurDioxide+density+pH+sulphates+alcohol+V14,data=mydata[,-c(13)] )
confint(logi3)
aux2<-round(predict(logi3, newdata= mytestdata[,-(13)]))
ret3<-sum(aux2==mytestdata[,12])/1000

print('Quality')
ret1
print('Type')
ret2
print('Quality with type')
ret3

matriz1<-matrix(c(0,0,0,0),ncol=2,nrow=2)
for (i in 1:1000) {
  if(aux1[i]=='0'){b=1}else {b=2}
  if(mytestdata[i,14]=='0'){a=1}else {a=2}
  matriz1[a,b]<-matriz1[a,b] +1
}
matriz1

matriz2<-matrix(0, ncol=7,nrow=7)
for (i in 1:1000) {
  b<-aux2[i]
  a<-mytestdata[i,12]
  matriz2[a,b]<-matriz2[a,b] +1
}
matriz2
matriz2[7,1]<-100


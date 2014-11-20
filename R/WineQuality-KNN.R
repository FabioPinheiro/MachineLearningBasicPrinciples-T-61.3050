###                             KNN Wine-QUALITY


library(class)

getwd()
data <- read.csv("T-61_3050_training_dataset.csv")

train<-data[1:3750,]
validation<-data[3751:5000,]


datascale<-scale(data[-c(12,13)])
trainscale<-datascale[1:3750,]
validationscale<-datascale[3751:5000,]



accuracy={}
for (i in 1:20) {
  accuracy[i]<-sum(knn(train[,1:11], validation[,-c(12,13)], train[,12], k = i)==validation[,12])/1250
}
which.max(accuracy) #To see which "k" had the maximum accuracy
plot(accuracy,type="l")


accuracy={}
for (i in 1:20) {
  accuracy[i]<-sum(knn(trainscale, validationscale, train[,12], k = i)==validation[,12])/1250
}
which.max(accuracy)
plot(accuracy,type="l")








##############RESULTADOS
test<-read.csv("T-61_3050_test_dataset.csv")

sum(knn(data[,1:11], test[,-c(12,13)], data[,12], k = 11)==test[,12])/1000    #Accuracy for Non-Scaled

wholedata<-rbind(data,test)
wholedatascaled<-scale(wholedata[,-c(12,13)])
sum(knn(wholedatascaled[1:5000,],wholedatascaled[5001:6000,],data[,12],k=1)==test[,12])/1000 #Accuracy for Scaled

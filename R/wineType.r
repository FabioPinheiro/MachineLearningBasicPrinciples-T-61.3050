                            #Tipos de vuinhos 



library(class)

getwd()
data <- read.csv("T-61_3050_training_dataset.csv")

train<-data[1:3750,]
test<-data[3751:5000,]
                  

datascale<-scale(data[-c(12,13)])
trainscale<-datascale[1:3750,]
testscale<-datascale[3751:5000,]



accuracy={}
for (i in 1:20) {
  accuracy[i]<-sum(knn(train[,1:11], test[,-c(12,13)], train[,13], k = i)==test[,13])/1250
}
accuracy
plot(accuracy,type="l")


accuracy={}
for (i in 1:20) {
  accuracy[i]<-sum(knn(trainscale, testscale, train[,13], k = i)==test[,13])/1250
}
accuracy
plot(accuracy,type="l")



truedata <- read.csv("T-61_3050_test_dataset.csv")
truedatascale<-scale(truedata[-c(12,13)])


sum(knn(datascale, truedatascale, data[,13], k = 1)==truedata[,13])/1000

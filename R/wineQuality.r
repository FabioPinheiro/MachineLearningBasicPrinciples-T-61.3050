            #Qualidade dos vinhos



library(class)
library(stats)

getwd()
data <- read.csv("T-61_3050_training_dataset.csv",header=TRUE)
data[1,]

train<-data[1:2500,]
validation<-data[2501:3750,]
test<-data[3751:5000,]

logi<-glm(quality~fixedAcidity + volatileAcidity+citricAcid+ residualSugar+chlorides+freeSulfurDioxide+totalSulfurDioxide+density+pH+sulphates+alcohol,data=train[,-13] )

confint(logi)

predict(logi, newdata= test[,-c(13)])
sum(round(predict(logi, newdata= test[,-c(13)]))==test[,12])/1250

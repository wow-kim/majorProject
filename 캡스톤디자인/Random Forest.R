library(tidyverse)
library(tidymodels)
library(dplyr)
library(randomForest)

# 데이터 불러오기      
bike = read.csv("C:/Users/ad/Desktop/강의자료4-1/캡스톤/대여소_결합(0716).csv")
comp = read.csv("C:/Users/ad/Desktop/강의자료4-1/캡스톤/대여소후보_결합(0717).csv")

bike = na.omit(bike)  %>% mutate(이용량=대여+반납)
bike = bike %>% filter(이용량<quantile(이용량,0.95))

#랜덤포레스트
str(bike)
bike_in <- bike[,c(5,11:49)]
bike_out <- bike[,c(4,11:49)]
bike_sum <- bike[,c(11:50)]
str(bike_sum)
k=5

#파라미터 튜닝
tuneRF(bike_in[,c(2:40)], bike_in[,1], stepFactor = 1.5, improve=1e-5,ntree=500)
tuneRF(bike_out[,c(2:40)], bike_out[,1], stepFactor = 1.5, improve=1e-5,ntree=500)
tuneRF(bike_sum[,c(1:39)], bike_sum[,40], stepFactor = 1.5, improve=1e-5,ntree=500)

bike_in$id <- sample(1:k, nrow(bike_in), replace = TRUE)
bike_out$id <- sample(1:k, nrow(bike_out), replace = TRUE)
bike_sum$id <- sample(1:k, nrow(bike_sum), replace = TRUE)
list <- 1:k

prediction_out <- data.frame()
testsetCopy_out <- data.frame()
prediction_in <- data.frame()
testsetCopy_in <- data.frame()
prediction_sum <- data.frame()
testsetCopy_sum <- data.frame()

for(i in 1:k){
  trainingset <- subset(bike_in, id %in% list[-i])
  testset <- subset(bike_in, id %in% c(i))
  mymodel_in <- randomForest(trainingset$반납 ~ .-id, data = trainingset,ntree=500,mtry=19)
  temp_in <- as.data.frame(predict(mymodel_in, testset[,-1]))
  prediction_in <- rbind(prediction_in, temp_in)
  testsetCopy_in <- rbind(testsetCopy_in, as.data.frame(testset[,1]))
}
imp_in = data.frame(mymodel_in$importance)
imp_in[order(imp_in[,1])]

plot(mymodel_in, main="반납")
plot(mymodel_out, main="대여")
plot(mymodel_sum, main="이용량")
str(bike_in)

a = as.data.frame(mymodel_in$importance)
a[order(a$IncNodePurity),]


varImpPlot(mymodel_in, n.var=40)
varImpPlot(mymodel_out, n.var=40)
?varImpPlot

a = bike %>% filter(이용량 > 350)
a


#버스_경유, 면적_아파, 면적_주거, 거리_초중, 주거_60대, near, medium, far, 유동_10대미 ,유동_60대,
for(i in 1:k){
  trainingset <- subset(bike_out, id %in% list[-i])
  testset <- subset(bike_out, id %in% c(i))
  mymodel_out <- randomForest(trainingset$대여 ~ .-id , data = trainingset,ntree=500,mtry=28)
  temp_out <- as.data.frame(predict(mymodel_out, testset[,-1]))
  prediction_out <- rbind(prediction_out, temp_out)
  testsetCopy_out <- rbind(testsetCopy_out, as.data.frame(testset[,1]))
}
imp_out = data.frame(mymodel_out$importance)


for(i in 1:k){
  trainingset <- subset(bike_sum, id %in% list[-i])
  testset <- subset(bike_sum, id %in% c(i))
  mymodel_sum <- randomForest(trainingset$이용량 ~ .-id , data = trainingset,ntree=500,mtry=19)
  temp_sum <- as.data.frame(predict(mymodel_sum, testset[,-40]))
  prediction_sum <- rbind(prediction_sum, temp_sum)
  testsetCopy_sum <- rbind(testsetCopy_sum, as.data.frame(testset[,40]))
}
imp_sum = data.frame(mymodel_sum$importance)
str(testset)
imp_in
result <- cbind(prediction_out, testsetCopy_out[, 1],prediction_in, testsetCopy_in[, 1])
names(result) <- c("Predicted대여", "Actual대여","Predicted반납","Actual반납")
result$Difference <- (result$Actual대여+result$Actual반납 - result$Predicted대여 -result$Predicted반납 )**2
mean(result$Difference)
str(prediction_sum)

mean((prediction_sum[,1] - testsetCopy_sum[,1] )**2)





comp = read.csv("C:/Users/ad/Desktop/강의자료4-1/캡스톤/대여소후보_결합(0717).csv")

final_out = predict(mymodel_out,comp)
final_in = predict(mymodel_in,comp)
final_sum = predict(mymodel_sum,comp)

final = cbind(comp, 대여반납 = final_out+final_in, 이용량=final_sum)
write.csv(final, file="C:/Users/ad/Desktop/강의자료4-1/캡스톤/final.csv")


func = function(x){
  round(x,3)
}


comp = comp[,c(9:47)]
comp = apply(comp,2, scale)
comp = scale(comp)


final = apply(final,2,func)

final = data.frame(final) %>% rename("pred"="final")
str(final)
ggplot(final) + geom_point(aes(x=pred, y=거리_지하), color="darkblue")


a10 = apply(final[which(final$pred >= quantile(final$pred, 0.9, na.rm = TRUE)),],2,mean)

a9 = apply(final[which( (final$pred < quantile(final$pred, 0.9, na.rm = TRUE)) &
                   (final$pred >= quantile(final$pred, 0.8, na.rm = TRUE)) ),],2,mean)

a8 = apply(final[which( (final$pred < quantile(final$pred, 0.8, na.rm = TRUE)) &
                     (final$pred >= quantile(final$pred, 0.7, na.rm = TRUE)) ),],2,mean)

a7 = apply(final[which( (final$pred < quantile(final$pred, 0.7, na.rm = TRUE)) &
                     (final$pred >= quantile(final$pred, 0.6, na.rm = TRUE)) ),],2,mean)

a6 = apply(final[which( (final$pred < quantile(final$pred, 0.6, na.rm = TRUE)) &
                     (final$pred >= quantile(final$pred, 0.5, na.rm = TRUE)) ),],2,mean)

a5 = apply(final[which( (final$pred < quantile(final$pred, 0.5, na.rm = TRUE)) &
                     (final$pred >= quantile(final$pred, 0.4, na.rm = TRUE)) ),],2,mean)

a4 = apply(final[which( (final$pred < quantile(final$pred, 0.4, na.rm = TRUE)) &
                     (final$pred >= quantile(final$pred, 0.3, na.rm = TRUE)) ),],2,mean)

a3 = apply(final[which( (final$pred < quantile(final$pred, 0.3, na.rm = TRUE)) &
                     (final$pred >= quantile(final$pred, 0.2, na.rm = TRUE)) ),],2,mean)

a2 = apply(final[which( (final$pred < quantile(final$pred, 0.2, na.rm = TRUE)) &
                     (final$pred >= quantile(final$pred, 0.1, na.rm = TRUE)) ),],2,mean)

a1 = apply(final[which( final$pred < quantile(final$pred, 0.1, na.rm = TRUE)),],2,mean)
a1
 c2 = unname(a1[1])
 c = c(c1,c2)
barplot(c)
c
length(names(a1))
 for (i in 6:22){
  df = c( unname(a1[i]), unname(a2[i]), unname(a3[i]), unname(a4[i]), unname(a5[i]),
                     unname(a6[i]), unname(a7[i]), unname(a8[i]), unname(a9[i]), unname(a10[i]))
  barplot(df, col="darkblue", ylim=c(-0.5, 1.5),main = names(a1)[i])
 }
df
i=6
barplot()

imp <- cbind(var = rownames(imp), imp)
rownames(imp) <- 1:nrow(imp)
imp
imp = imp %>% rename("impo"="IncNodePurity")
imp = arrange(imp,-impo)


for(i in 1:k){
  trainingset <- subset(bike_in, id %in% list[-i])
  testset <- subset(bike_in, id %in% c(i))
  mymodel_in <- randomForest(trainingset$반납 ~ ., data = trainingset,ntree=500,mtry=13)
  temp <- as.data.frame(predict(mymodel_in, testset[,-1]))
  prediction_in <- rbind(prediction_in, temp)
  testsetCopy_in <- rbind(testsetCopy_in, as.data.frame(testset[,1]))
}

oout = predict(mymodel_out,comp)
iin = predict(mymodel_in,comp)

final_out =  data.frame(comp, oout)
final_in = data.frame(comp, iin)
final[final$pre > quantile(final$pre, 0.9,na.rm=TRUE)]


o = quantile(final_out$oout, 0.9, na.rm = TRUE)
summary(comp)
summary(final_out[which(final_out$oout >= o),])
i = quantile(final_in$iin, 0.9, na.rm = TRUE)
summary(final_in[which(final_in$iin >= o),])


result <- cbind(prediction_out, testsetCopy_out[, 1],prediction_in, testsetCopy_in[, 1])
names(result) <- c("Predicted대여", "Actual대여","Predicted반납","Actual반납")
result$Difference <- abs(result$Actual대여+result$Actual반납 - result$Predicted대여 -result$Predicted반납 )
# As an example use Mean Absolute Error as Evalution
summary(result$Difference)


#변수선택

bike_split = bike_in %>% initial_split(prop=0.7)
bike_split %>% training() %>% recipe(반납~.) %>% step_corr(all_predictors()) %>%
  prep() -> bike_recipe

a = bike_in %>% recipe(반납~.) %>% step_corr(all_predictors())%>% prep()
str(a)
bike_testing = bike_recipe %>% bake(bike_split %>% testing())
bike_training = bike_recipe %>% juice()

bike_rf = randomForest


bike_rf = bike %>% recipe(대여~.) %>% step_corr(all_predictors())





library(dplyr)
library(ggplot2)
install.packages("animation")
library(animation)


bike = read.csv("C:/Users/ad/Desktop/강의자료4-1/캡스톤/대여소_결합(0716).csv")
pred = read.csv("C:/Users/ad/Desktop/강의자료4-1/캡스톤/final.csv")

bike = bike %>% mutate(이용량 = 대여+반납)
b = bike[,c(11:50)]
b1 = b %>% filter(이용량 > quantile(이용량,0.9))
b2 = b %>% filter((이용량 > quantile(이용량,0.8))&
                       (이용량 <= quantile(이용량, 0.9)))
b3 = b %>% filter((이용량 > quantile(이용량,0.7))&
                    (이용량 <= quantile(이용량, 0.8)))
b4 = b %>% filter((이용량 > quantile(이용량,0.6))&
                       (이용량 <= quantile(이용량, 0.7)))
b5 = b %>% filter((이용량 > quantile(이용량,0.5))&
                    (이용량 <= quantile(이용량, 0.6)))
b6 = b %>% filter((이용량 > quantile(이용량,0.4))&
                       (이용량 <= quantile(이용량, 0.5)))
b7 = b %>% filter((이용량 > quantile(이용량,0.3))&
                    (이용량 <= quantile(이용량, 0.4)))
b8 = b %>% filter((이용량 > quantile(이용량,0.2))&
                    (이용량 <= quantile(이용량, 0.3)))
b9 = b %>% filter((이용량 > quantile(이용량,0.1))&
                    (이용량 <= quantile(이용량, 0.2)))
b10 = b %>% filter(이용량 <= quantile(이용량, 0.1))
m1 = apply(b1,2,mean,na.rm=T)
m2 = apply(b2,2,mean,na.rm=T)
m3 = apply(b3,2,mean,na.rm=T)
m4 = apply(b4,2,mean,na.rm=T)
m5 = apply(b5,2,mean,na.rm=T)
m6 = apply(b6,2,mean,na.rm=T)
m7 = apply(b7,2,mean,na.rm=T)
m8 = apply(b8,2,mean,na.rm=T)
m9 = apply(b9,2,mean,na.rm=T)
m10 = apply(b10,2,mean,na.rm=T)
m11 = m1/m10
cb = cbind(stack(m1)[,c(2,1)], stack(m2)[,1],stack(m3)[,1],stack(m4)[,1],stack(m5)[,1],
           stack(m6)[,1], stack(m7)[,1], stack(m8)[,1], stack(m9)[,1], stack(m10)[,1],stack(m11)[,1])
names(cb) = c("변수","10","20","30","40","50","60","70","80","90","100","기준")
var_dist = cb[order(cb$기준,decreasing=T),][,c(1:11)]
rownames(var_dist) = NULL
var_dist
options(digits=2)

ggplot(var_dist) + geom_bar(aes(x))


for (i in 2:23){
  barplot(as.numeric(var_dist[i,2:11]),xlab="",ylab="",main=as.character(var_dist[i,1]),col="royalblue")
}
for (i in 24:40){
  barplot(as.numeric(var_dist[i,2:11]),xlab="",ylab="",main=as.character(var_dist[i,1]),col="royalblue")
}
warnings()
?barplot
ggplot(bike) + geom_point(aes(x=거리_자전, y=))
ggplot(bike) + geom_histogram(aes(x=거리_지하))
ggplot(gu) + geom_bar(aes(y=n))

bike$대여 

library(ggcorrplot)
station = read.csv("C:/Users/ad/Desktop/강의자료4-1/캡스톤/대여소_결합(0716).csv", stringsAsFactor=FALSE)
station = na.omit(station)
str(station)
ggcorrplot(cor(station[,c(17,28,16,18,20,21,22,29)]), type = "lower",outline.color = "white", lab=TRUE)
ggcorrplot(cor(station[,16:32]),hc.order = TRUE, type = "lower",outline.color = "white", lab=TRUE)
ggcorrplot(cor(station[,c(33:38,42:49)]),hc.order = TRUE, type = "lower",outline.color = "white", lab=TRUE)
str(station)
distance = station[,c(4,5,6,16:32)]
str(distance)
corr <- cor(distance[,c(7,11,14,15,16)])
corr
ggcorrplot(corr, hc.order = TRUE, type = "lower",outline.color = "white", lab=TRUE)
str(station)
station = station %>% drop_na()
corr <- cor(station[,c(11:49)])

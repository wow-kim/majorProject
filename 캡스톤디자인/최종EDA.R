
install.packages("plotly")
install.packages("viridis")
install.packages("RColorBrewer")
install.packages("tidyverse")
install.packages("dlookr")
install.packages("gridExtra")
install.packages("ggforce")
install.packages("ggcorrplot")
library(ggcorrplot)
library(gridExtra)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(viridis)
library(ggplot2)
library(ggforce)
library(dlookr)
library(tidyverse)

RColorBrewer::display.brewer.all()
myblue = RColorBrewer::brewer.pal(n=5, name="Blues")
myred = RColorBrewer::brewer.pal(n=5, name="Reds")
plot_outlier(station,대여_반납)
plot_correlate(station, font_size=0.001)

station = read.csv("C:/Users/ad/Desktop/캡스톤실습/대여소_결합(0716).csv", stringsAsFactor=FALSE)
str(station)
summary(station)

station = station %>% mutate(대여_반납 = (대여-반납)/대여,이용량 = 대여+반납)
station


#대여, 반납
a1 = ggplot(station) + 
  geom_boxplot(aes(y=대여),
               fill=myred[4],color="black",outlier.color=myred[4], outlier.size=1) + ggtitle("대여") +
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(color="red",size=14,face="bold")) 
a2 = ggplot(station) + 
  geom_boxplot(aes(y=반납),
               fill=myblue[4],color="black",outlier.color=myblue[4], outlier.size=1) + ggtitle("반납") +
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(color="blue",size=14,face="bold")) 
a3 = ggplot(station) + 
  geom_boxplot(aes(y=이용량),
               fill=gray(0.3),color="black",outlier.color="black", outlier.size=1) + ggtitle("이용량") +
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(color="black",size=14,face="bold")) 

grid.arrange(a1,a2,a3,nrow=1, ncol=3)

#대여/반납 plot
plot_ly(data=station,
        x = ~대여,
        y = ~반납,
        type = "scatter", mode="markers", 
        hoverinfo = "text", text = paste("정류장 : ", station$STATION_NA))
ggplot(station, aes(x=대여, y=반납)) +
  geom_point(shape=21, size=1, color= gray(0.15)) + stat_smooth( color = "blue") +
  scale_x_continuous(breaks=seq(0,250,50)) + scale_y_continuous(breaks=seq(0,250,50))
  
cor(station$대여, station$반납)

#이상치들 QGIS로 확인

#대여/반납 비율
ggplot(station, aes(x=대여, y=반납, fill= 대여_반납)) +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient(low="blue",high= "gray") + 
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(0,250,50)) +
  scale_y_continuous(breaks = seq(0,250,50))

ggplot(station, aes(x=대여_반납)) +
  geom_histogram(bins=30, color="black", fill=myblue[5]) + 
  theme(axis.title.y= element_blank(),axis.title = element_text(size=15, face="bold"))+
  scale_x_continuous(breaks=seq(-0.5,0.5,0.25)) +
  xlab("(대여-반납) / 대여")

ggplot(station, aes(y=대여_반납)) +
  geom_boxplot()
ggplot(station) + 
  geom_boxplot(aes(y=대여_반납),
               fill=myblue[5],color="black",outlier.color=myblue[5], outlier.size=1) +
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(color="blue",size=14,face="bold")) +
  scale_y_continuous(breaks=seq(-0.5,0.75,0.25))
summary(station)

# 대여가 더 많은 정류소 
par( mfrow = c(1,1))
g1 = ggplot( station %>% filter(대여_반납 > quantile(station$대여_반납, probs= 0.95)) )+
  geom_point(aes(x=평균_경사, y=대여), color="red") + 
  geom_point(aes(x=평균_경사, y=반납), color="blue") +
  ggtitle("대여 > 반납")

# 반납이 더 많은 정류소
g2 = ggplot( station %>% filter(대여_반납 < quantile(station$대여_반납, probs= 0.05)) )+
  geom_point(aes(x=평균_경사, y=대여), color="red") + 
  geom_point(aes(x=평균_경사, y=반납), color="blue") + 
  ggtitle("반납 > 대여")
grid.arrange(g1,g2,nrow=2,ncol=1)


#경사도
ggplot(station) + 
  geom_boxplot(aes(y=평균_경사),
               fill="darkgreen",color="black",outlier.color="darkgreen", outlier.size=1) +
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(color="darkgreen",size=14,face="bold")) +
  scale_y_continuous(breaks=seq(0,12.5,2.5))

ggplot(station, aes(x=평균_경사)) +
  geom_histogram(bins=30, color="black", fill="darkgreen") + 
  theme(axis.title.y= element_blank(),axis.title = element_text(size=15, face="bold"))+
  scale_x_continuous(breaks=seq(0,12.5,2.5)) +
  xlab("평균경사도")

#경사도~대여, 경사도~반납

ggplot(station,aes(y=대여,x=평균_경사)) + geom_point( color=myred[4], pch=18, size=1.5) + 
  scale_x_continuous(breaks=seq(0,12.5,2.5)) + scale_y_continuous(breaks=seq(0,250,50))
ggplot(station,aes(y=반납,x=평균_경사)) + geom_point( color=myblue[5],pch=18, size=1.5) + 
  scale_x_continuous(breaks=seq(0,12.5,2.5)) + scale_y_continuous(breaks=seq(0,250,50)) 

# 경사도~대여/반납
func = function(x){
  -(x-0.25)*(x+0.25)*80
}
circle()
?annotate
ggplot(station) + geom_point(aes(y=평균_경사, x=대여_반납),shape=18,size=1.6) +
  ylab("평균경사도") + xlab("(대여-반납)/대여") + ylim(c(0,12.5)) + 
  stat_function(fun = func, geom="line",aes(x=seq(-0.5,0.5,1552)),color="red",linetype="dashed" ,size=1.7) +
  geom_line(data= data.frame(x=c(0,0),y=c(5,12.5)),aes(x=x,y=y), color="red",linetype="dashed" ,size=1.7)
?geom_vline

#평균_경사가 높아질수록 반납 대비 대여가 큼
a =  mean(station$대여_반납)
slope_over1 = station %>% filter(평균_경사 > 1 & 평균_경사<2)
a1 =  mean(slope_over1$대여_반납)
slope_over2 = station %>% filter(평균_경사 > 2 & 평균_경사<3)
a2 =  mean(slope_over2$대여_반납)
slope_over3 = station %>% filter(평균_경사 > 3 & 평균_경사<4)
a3 =  mean(slope_over3$대여_반납)
slope_over4 = station %>% filter(평균_경사 > 4 & 평균_경사<5)
a4 =  mean(slope_over4$대여_반납)
slope_over5 = station %>% filter(평균_경사 > 5 & 평균_경사<6)
a5 =  mean(slope_over5$대여_반납)
slope_over6 = station %>% filter(평균_경사 > 6 & 평균_경사<7)
a6 =  mean(slope_over6$대여_반납)
slope_over7 = station %>% filter(평균_경사 > 7 & 평균_경사<8)
a7 =  mean(slope_over7$대여_반납)
slope_over8 = station %>% filter(평균_경사 > 8)
a8 =  mean(slope_over8$대여_반납)

df = data.frame(index = c("all",seq(1,8)),slope = c(a,a1,a2,a3,a4,a5,a6,a7,a8))
df
?barplot
barplot(df$slope, col="gray",names.arg = df$index)
ggplot(df,aes(x=index, y)) + geom_bar(size=2, color="darkblue") +
  scale_x_continuous(breaks=seq(1,9))

#거리시리즈
distance = station[,c(4,5,6,16:32)]
summary(distance)
#boxplot
d1= ggplot(distance) + geom_boxplot(aes(y=거리_지하), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("지하철")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d2= ggplot(distance) + geom_boxplot(aes(y=거리_자전), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("자전거도로")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d3= ggplot(distance) + geom_boxplot(aes(y=거리_공공), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("공공기관")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d4=ggplot(distance) + geom_boxplot(aes(y=거리_문화), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("문화시설")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d5=ggplot(distance) + geom_boxplot(aes(y=거리_영화), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("영화관")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d6=ggplot(distance) + geom_boxplot(aes(y=거리_관광), fill="darkred",color="black",outlier.color="darkred", outlier.size=1) + ggtitle("관광지")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkred",size=14,face="bold"))
d7=ggplot(distance) + geom_boxplot(aes(y=거리_대학), fill="darkred",color="black",outlier.color="darkred", outlier.size=1) + ggtitle("대학")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkred",size=14,face="bold"))
d8=ggplot(distance) + geom_boxplot(aes(y=거리_초중), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("초중")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d9=ggplot(distance) + geom_boxplot(aes(y=거리_상업), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("상업지역")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d10=ggplot(distance) + geom_boxplot(aes(y=거리_의료), fill="darkred",color="black",outlier.color="darkred", outlier.size=1) + ggtitle("의료시설")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkred",size=14,face="bold"))
d11=ggplot(distance) + geom_boxplot(aes(y=거리_주차), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("주차장")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d12=ggplot(distance) + geom_boxplot(aes(y=거리_체육), fill="black",color=gray(0.4),outlier.color="black", outlier.size=1) + ggtitle("체육시설")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="black",size=14,face="bold"))
d13=ggplot(distance) + geom_boxplot(aes(y=거리_공원), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("공원")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))
d14=ggplot(distance) + geom_boxplot(aes(y=거리_특화), fill="black",color=gray(0.4),outlier.color="black", outlier.size=1) + ggtitle("특화지구")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="black",size=14,face="bold"))
d15=ggplot(distance) + geom_boxplot(aes(y=거리_교통), fill="black",color=gray(0.4),outlier.color="black", outlier.size=1) + ggtitle("교통")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="black",size=14,face="bold"))
d16=ggplot(distance) + geom_boxplot(aes(y=거리_하천), fill="darkblue",color="black",outlier.color="darkblue", outlier.size=1) + ggtitle("하천")+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(color="darkblue",size=14,face="bold"))

# (문화,영화,관광,대학,체육,특화,교통)
summary(distance)

grid.arrange(d1,d2,d3,d4,d5,d6,d7,d8,nrow=1,ncol=8)
grid.arrange(d9,d10,d11,d12,d13,d14,d15,d16,nrow=1,ncol=8)

corr <- cor(distance[,c(4:19,20)])
ggcorrplot(corr, hc.order = TRUE, type = "lower",outline.color = "white", lab=TRUE)
summary(distance)
cor(station$반납,station$평균_경사)
ggplot(distance,)

scaled_distance = as.data.frame(scale(distance[,4:19]))
s_corr <- cor(scaled_distance)
ggcorrplot(s_corr, hc.order = TRUE, type="upper", outline.color = "white", lab=TRUE)

station %>% filter(반납 < 1.55)

distance = distance %>% mutate(대여_반납 = (대여-반납)/대여,이용량 = 대여+반납)

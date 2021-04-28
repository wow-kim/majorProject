library(ggplot2)
library(dplyr)
library(gridExtra)

install.packages("d3heatmap")
library(corrplot)

heart

heart = read.csv("C:/Users/ad/Desktop/강의자료4-1/회귀분석2/회귀 기말프로젝트/heart.csv", encoding="utf-8")
heart = na.omit(heart)

corrplot(heart,method="circle")

c
corrplot(heart[,c(1,7)])

#기초통계

str(heart)
summary(heart)
table(heart$sex)
table(heart$cp)
table(heart$fbs)
table(heart$target)

ggplot(heart) + geom_bar(aes(x=target),color="black") + ylim(0,600)
  theme(axis.title = element_text(size=15), title = element_text(size=15))

ggplot(heart) + geom_boxplot(aes(y=age)) + 
  theme(axis.title = element_text(size=15), title = element_text(size=15)) 
ggplot(heart) + geom_histogram(aes(x=age),binwidth = 2.5, color="black") + 
  theme(axis.title = element_text(size=15), title = element_text(size=15))

ggplot(heart) + geom_boxplot(aes(y=trestbps)) +
  theme(axis.title = element_text(size=15), title = element_text(size=15))
ggplot(heart) + geom_histogram(aes(x=trestbps), binwidth=7, color="black") +
  theme(axis.title = element_text(size=15), title = element_text(size=15))

ggplot(heart) + geom_boxplot(aes(y=chol)) + 
  theme(axis.title = element_text(size=15), title = element_text(size=15))
ggplot(heart) + geom_histogram(aes(x=chol), binwidth=20, color="black") +
  theme(axis.title = element_text(size=15), title = element_text(size=15))

ggplot(heart) + geom_boxplot(aes(y=thalach)) + 
  theme(axis.title = element_text(size=15), title = element_text(size=15))
ggplot(heart) + geom_histogram(aes(x=thalach), binwidth=8, color="black") +
  theme(axis.title = element_text(size=15), title = element_text(size=15))




heart$target
ggplot(heart) + geom_bar(aes(x=sex),color="black") + 
  theme(axis.title = element_text(size=15), title = element_text(size=15))
ggplot(heart) + geom_bar(aes(x=cp),color="black") + 
  theme(axis.title = element_text(size=15), title = element_text(size=15))
ggplot(heart) + geom_bar(aes(x=fbs),color="black") + 
  theme(axis.title = element_text(size=15), title = element_text(size=15))


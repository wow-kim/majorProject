install.packages("pastecs")
library(pastecs)
library(ggplot2)

station = read.csv("C:/Users/ad/Desktop/캡스톤실습/대여소_결합(0716).csv")

station1= station[,c("STATION_NA","대여","반납","거리_지하","거리_자전",
                          "거리_공공","거리_문화","거리_영화","거리_관광",
                          "거리_대학","거리_초중","거리_상업","거리_의료",
                          "거리_주차","거리_체육","거리_공원","거리_특화",
                          "거리_교통","거리_하천","평균_경사")]
summary(station_infp)
station1$대여 = as.numeric(station$대여)


q1 = quantile(station1$대여, 0.25)
q2 = quantile(station1$대여, 0.5)
q3 = quantile(station1$대여, 0.75)
q4 = quantile(station1$대여, 1)

station1$대여[station1$대여 < q1] = "1Q"
station1$대여[station1$대여 < q2 & station1$대여 >= q1] = "2Q"
station1$대여[station1$대여 < q3 & station1$대여 >= q2] = "3Q"
station1$대여[station1$대여 >= q3] = "4Q"
station1$대여

station1$대여_cd
pairs(station_infp)
ggplot(station_infp,) + geom_boxplot()

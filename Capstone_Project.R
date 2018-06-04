library(data.table)
df<-fread('Batting.csv')
head(df)
str(df)
head(df$AB)
head(df[,'2B'])
df$BA<-df$H/df$AB
tail(df[,'BA'])
df$OBP<-(df$H+df$BB+df$HBP)/(df$AB+df$HBP+df$BB+df$SF)
head(df$OBP)
str(df)
df[,'1B']<-df$H-df$`2B`-df$`3B`-df$HR
df$SLG<-(df$`1B`+2*df$`2B`+3*df$`3B`+4*df$HR)/df$AB
str(df)
sal<-fread('Salaries.csv')
head(sal)
df<-subset(df,yearID>=1985)
summary(df)
combo<-merge(df,sal,by=c('playerID','yearID'))
summary(combo)
lost_players<-subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players<-subset(lost_players,yearID==2001)
library(dplyr)
lost_players<-lost_players[,c('playerID','H','2B','3B','HR','OBP','SLG','BA','AB')]
lost_players
avail_players<-subset(combo,yearID==2001)
avail_players<-select(avail_players,playerID,salary,OBP,AB)
avail_players
library(ggplot2)
pl_salary<-ggplot(avail_players,aes(x=OBP,y=salary))+geom_point()
pl_salary
max(avail_players$salary)
min(avail_players$salary)
lost_players%>%summarise(avg=mean(OBP))
avail_players<-filter(avail_players,OBP>0,salary<8000000)
sum(lost_players$AB)
avail_players<-filter(avail_players,AB>=500)
possible<-head(arrange(avail_players[,c("playerID","OBP","salary","AB")],desc(OBP)))
possible[2:4,]





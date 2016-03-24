setwd("C:/Users/Yolanda/Documents/courses-2016/customer analytics/Session 6.1 Pilgrim Bank A.2")
part1data<-read.csv("pilgrim A2 data part1.csv")
library(visreg)
head(part1data)
summary(part1data)
part1data$Online99<-as.factor(part1data$Online99)
maineffects <-lm(Profit99~.-ID,data=part1data)
summary(maineffects)
visreg(maineffects, "Age99", by="Online99", overlay ="TRUE")
visreg(maineffects, "Inc99", by="Online99", overlay ="TRUE")
visreg(maineffects, "Tenure99", by="Online99", overlay ="TRUE")

#part1.2

interactionAgeOnline<-lm(Profit99~Online99+Age99+Online99:Age99,data=part1data)
summary(interactionAgeOnline)
visreg(interactionAgeOnline, "Age99", by="Online99", overlay ="TRUE")


interactionIncOnline<-lm(Profit99~Online99+Inc99+Online99:Inc99,data=part1data)
summary(interactionIncOnline)
visreg(interactionIncOnline, "Inc99", by="Online99", overlay ="TRUE")

interactionTenureOnline<-lm(Profit99~Online99+Tenure99+Online99:Tenure99,data=part1data)
summary(interactionTenureOnline)
visreg(interactionTenureOnline, "Tenure99", by="Online99", overlay ="TRUE")


#1.3
interactionall<-lm(Profit99~Online99+Inc99+Tenure99+Age99+Online99:Inc99,data=part1data)
summary(interactionall)

#part 2 experiment analysis

part2expreiment<-read.csv("pilgrim A2 data part 2 experiment.csv")
head(part2expreiment)
control<-subset(part2expreiment,condition==0)
t.test(control$Profit99,control$Profit00)
experiment<-subset(part2expreiment,condition==1)
t.test(experiment$Profit99,experiment$Profit00)
t.test(control$Profit99,experiment$Profit99)
t.test(control$Profit00,experiment$Profit00)

#part 3 out of sample predictions
head(experiment)
all<-lm(Profit00~Profit99+Age99+Inc99+Tenure99, data=experiment)
summary(all)

experiment$Inc99<-as.factor(experiment$Inc99)
cate<-lm(Profit00~Profit99+Age99+Inc99+Tenure99, data=experiment)
summary(cate)

allinteraction<-lm(Profit00~Profit99+Age99+Inc99+Tenure99+Age99:Inc99+Age99:Tenure99+Inc99:Tenure99, data=experiment)
summary(allinteraction)

interaction<-lm(Profit00~Profit99+Age99+Inc99+Tenure99+Age99:Inc99, data=experiment)
summary(interaction)

tenure<-lm(Profit00~Tenure99,data=experiment)
visreg(tenure)
summary(tenure)    #tenure is not significant

bestmodel<-lm(Profit00~Profit99+Age99+Inc99+Age99:Inc99, data=experiment)
summary(bestmodel)

set.seed(123)
library(cvTools)
cvFit(all,y=experiment$Profit00, data=experiment, K=3, R=50)
cvFit(interaction,y=experiment$Profit00, data=experiment, K=3, R=50)
cvFit(bestmodel,y=experiment$Profit00, data=experiment, K=3, R=50)

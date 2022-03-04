
#### R Script for the production of figures for the paper as well as descriptive statistics.


#clear the workspace
rm(list=ls())


#install packages if necessary
list.of.packages <- c("here","ggplot2", "tidyverse","reshape2","gridExtra","multipanelfigure","magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#load necessary libraries

library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(multipanelfigure)
library(here)
library(magrittr)


#get the data in, the ones who play accurately
data<- read_csv(here("AgingInconsistent.csv"), col_names = TRUE)




#remove inaccuracte, below .60 & ## those less than 26

#old, remove for MoCA
data<-subset(data,data$subjectID!=3)
data<-subset(data,data$subjectID!=15)
data<-subset(data,data$subjectID!=18)
data<-subset(data,data$subjectID!=19)
data<-subset(data,data$subjectID!=21)
data<-subset(data,data$subjectID!=22)
data<-subset(data,data$subjectID!=24)
data<-subset(data,data$subjectID!=30)
data<-subset(data,data$subjectID!=31)

#old remove for Acc
data<-subset(data,data$subjectID!=33)
data<-subset(data,data$subjectID!=47)
data<-subset(data,data$subjectID!=51)
data<-subset(data,data$subjectID!=62)

#young, remove for inaccurate
data<-subset(data,data$subjectID!=107)
data<-subset(data,data$subjectID!=118)
data<-subset(data,data$subjectID!=126)
data<-subset(data,data$subjectID!=130)
data<-subset(data,data$subjectID!=133)


Old<-subset(data,data$Condition=="old")
Young<-subset(data,data$Condition=="young")



#function for Lines on the graphs, get Standard Error measures in the right format

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


##################Figure 3#####################################################################
##### Get in Horizon 1 and 6 Comparison probability of picking high info ######
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_ 5','all_p_hi_h6_ 5'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)


Order<-sort(rep(c(1,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)



ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))






head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
d1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.5) + geom_point(size=5) +ggtitle("Directed") + xlab("Horizon") + ylab("p(high info)") +
  theme_classic() + scale_color_manual(values=c('darkblue','darkred')) +ylim(0.27,0.61) + scale_x_discrete(limits=c(1,6))+
theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none"))

Point<-ggplot(data, aes(x=p_hi_13_1,y=p_hi_13_2,fill=Condition,color=Condition))
d2<-Point+geom_point(size=5)+ ggtitle("Directed") + xlab("p(high info, horizon 1)") + ylab("p(high info, horizon 6)") +
scale_color_manual(values=c('darkblue','darkred')) + scale_x_continuous(breaks= c(0,.2,.4,.6,.8,1),limits= c(0,1)) + scale_y_continuous(breaks= c(0,.2,.4,.6,.8,1),limits= c(.0,1.0)) +
theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none")) +
geom_abline(intercept = 0, slope = 1,linetype="dashed",size=1)



##### Get in Horizon 1 and 6 Comparison probability of picking low mean######
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h1_ 5','all_p_lm_h6_ 5'))
ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
print(summary(ANOVA))
TukeyHSD(ANOVA)


Order<-sort(rep(c(1,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
r1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
geom_line(size=1.5) + geom_point(size=5) +ggtitle("Random") + xlab("Horizon") + ylab("p(low mean)") +
theme_classic() + scale_color_manual(values=c('darkblue','darkred')) + ylim(.1,.44)+scale_x_discrete(limits=c(1,6))+
theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=30),legend.title = element_text(face="bold",size=30), legend.position=c(.99,.99),legend.justification = c("right","top"))+
labs(fill="Age Group")

Point<-ggplot(data, aes(x=p_lm_13_1,y=p_lm_13_2,fill=Condition,color=Condition))
r2<-Point+geom_point(size=5)+ ggtitle("Random") + xlab("p(low mean,horizon 1)") + ylab("p(low mean,horizon 6)") +
scale_color_manual(values=c('darkblue','darkred')) + scale_x_continuous(breaks= c(0,.2,.4,.6,.8,1),limits= c(0,1)) + scale_y_continuous(breaks= c(0,.2,.4,.6,.8,1.0),limits= c(0,1)) +
theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none"))+
geom_abline(intercept = 0, slope = 1,linetype="dashed",size=1)


figure3 <- multi_panel_figure(width=360,height=360,columns = 2, rows = 2, row_spacing=10)


figure3 %<>% 
  fill_panel(d1,column=1,row=1) %>%
  fill_panel(r1,column=2,row=1) %>%
  fill_panel(d2,column=1,row=2) %>%
  fill_panel(r2,column=2,row=2) 
figure3

#16,16





#####Figure 4####

##### Get in Horizon 6 probability of picking more informative option
Youngold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_ 5','all_p_hi_h6_ 5','all_p_hi_h6_ 6','all_p_hi_h6_ 7','all_p_hi_h6_ 8','all_p_hi_h6_ 9','all_p_hi_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)


Order<-sort(rep(c(5,5:10),nrow(data)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order,Horizon)



##### Get in Horizon 6 probability of picking more informative option#### for ANOVA
#RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_5','all_p_hi_h6_ 5','all_p_hi_h6_ 6','all_p_hi_h6_ 7','all_p_hi_h6_ 8','all_p_hi_h6_ 9','all_p_hi_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)


#AOrder<-sort(rep(c(5:10),nrow(data)))
#Horizon<-sort(rep(c(6,6,6,6,6,6),nrow(data)))
#ARepeatYoungold_long<-cbind(ARepeatYoungold_long,AOrder)
#oldA<-subset(ARepeatYoungold_long,Condition=="old")
#youngA<-subset(ARepeatYoungold_long,Condition=="young")

#ANOVA<-aov(data=ARepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#mean(oldA$value)
#sd(oldA$value)


#mean(youngA$value)
#sd(youngA$value)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition","Horizon"))
#head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
p1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("p(high info), Free Trials") + xlab("Free Trials") + ylab("p(high info)") +
scale_color_manual(values=c('darkblue','darkred')) + ylim(0,.61)+
theme(plot.title = element_text(hjust=.5,color = "black", size = 36, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position = "none")+
scale_shape_manual(values=c(15, 17))


fcer6<-subset(ReaPer,ReaPer$Horizon==6)
fcer1<-subset(ReaPer,ReaPer$Horizon==1)
fcer1doub<-subset(fcer6,fcer6$Order==5)
fcer6a<-rbind(fcer6,fcer1doub)

Line6<-ggplot(fcer6, aes(x=Order, y=value, group=Condition, color=Condition))

q1<- p1 + geom_line(aes(x=fcer6a$Order, y=fcer6a$value, group=fcer6a$Condition, color=fcer6a$Condition),size=1.5) 

q1
#hi1<-q1+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))




#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
##### Get in Horizon 6 probability of picking low mean
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h1_ 5','all_p_lm_h6_ 5','all_p_lm_h6_ 6','all_p_lm_h6_ 7','all_p_lm_h6_ 8','all_p_lm_h6_ 9','all_p_lm_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)

#BRepeatYoungold_long<-melt(PatternHor6, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h6_ 5','all_p_lm_h6_ 6','all_p_lm_h6_ 7','all_p_lm_h6_ 8','all_p_lm_h6_ 9','all_p_lm_h6_10'))

#BOrder<-sort(rep(c(5:10),nrow(data)))
#Horizon<-sort(rep(c(6,6,6,6,6,6),nrow(data)))
#BRepeatYoungold_long<-cbind(BRepeatYoungold_long,AOrder)
#oldB<-subset(BRepeatYoungold_long,Condition=="old")
#youngB<-subset(BRepeatYoungold_long,Condition=="young")

#ANOVA<-aov(data=BRepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#mean(oldB$value)
#sd(oldB$value)


#mean(youngB$value)
#sd(youngB$value)

Order<-sort(rep(c(5,5:10),nrow(data)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(data)))

RepeatYoungold_long<-cbind(RepeatYoungold_long,Order,Horizon)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition","Horizon"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
p2<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("p(low mean), Free Trials") + xlab("Free Trials") + ylab("p(high info)") +
  scale_color_manual(values=c('darkblue','darkred')) + ylim(0,.61)+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 36, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position = "none")+
  scale_shape_manual(values=c(15, 17))






#p2<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
#geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("p(low mean), Free Trials") + xlab("Free Trials") + ylab("p(low mean)") +
#scale_color_manual(values=c('darkblue','darkred')) + ylim(0,.61)+
#theme(plot.title = element_text(hjust=.5,color = "black", size = 36, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=24),legend.title = element_text(face="bold",size=30), legend.position=c(.99,.99),legend.justification = c("right","top"))
#scale_shape_manual(values=c(15, 17))

p2

fcer6<-subset(ReaPer,ReaPer$Horizon==6)
fcer1<-subset(ReaPer,ReaPer$Horizon==1)
fcer1doub<-subset(fcer6,fcer6$Order==5)
fcer6b<-rbind(fcer6,fcer1doub)

#Line6b<-ggplot(fcer6, aes(x=Order, y=value, group=Condition, color=Condition))

q2<- p2 + geom_line(aes(x=fcer6b$Order, y=fcer6b$value, group=fcer6b$Condition, color=fcer6b$Condition), size=1.5) 

q2

#q2+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))


#q2


#grid.arrange(q1, q2, nrow = 1)


figure4 <- multi_panel_figure(width=300,height=180,columns = 2, rows = 1,figure_name="Figure 4. Overall Performance",column_spacing = 20)

figure4 %<>% 
  fill_panel(q1,column=1,row=1) %>%
  fill_panel(q2,column=2,row=1) 

figure4



###Figure 4###


#<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))


## Fraction Correct
#fcYoungold<-Youngold[,c(7:12,27,28,29,31)]
## for ANOVA
#fcYoungold1<-Youngold[,c(7:12,28,29,31)]

#fcYoungold_long<-melt(fcYoungold, id.vars= c("SN", "Group"),measure.vars=c("fcorFirstHOne","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast"))
#fcYoungold_long1<-melt(fcYoungold1, id.vars= c("SN", "Group"),measure.vars=c("fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast"))

#ANOVA<-aov(data=fcYoungold_long1,value~variable*Group + Error(factor(SN)))
#summary(ANOVA)



#model.tables(ANOVA, type="means", se=TRUE)

#Old<-subset(fcYoungold_long1,fcYoungold_long1$Group=="Old")
#AccO<-describe(Old$value)
#AccO$se


#Young<-subset(fcYoungold_long1,fcYoungold_long1$Group=="Young")
#AccY<-describe(Young$value)
#AccY$se




#TukeyHSD(ANOVA)

#Old vs. Young FC


#get the data in, the ones who play accurately
HealthyOld <- read_csv(here("OldAcc.csv"), col_names = FALSE)
HealthyOld <- t(HealthyOld)
HealthyOld<-as.data.frame(HealthyOld)
colnames(HealthyOld)<- c("ShortHighInfo","LongHighInfo","DirectedExploration","ShortlowMean","LonglowMean","RandomExploration","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast","RepeatProbabilityFirst","RepeatProbabilitySecond","RepeatProbabilityThird","RepeatProbabilityFourth","RepeatProbabilityFifth","RepeatProbabilityLast","RTimeFirst","RTimeSecond","RTimeThird","RTimeFourth","RTimeFifth","RTimeLast","RepeatProbabilityFirstHOne","RTimeFirstHOne","fcorFirstHOne")
HealthyOld$SN<-c(1,2,4:14,16,17,20,23,25:29,32,34,39:43,45,46,48:50,53,54,56,57,59:61,63,64)

HealthyOld<- HealthyOld %>% mutate(HighInfo=(HealthyOld$ShortHighInfo + HealthyOld$LongHighInfo)/2)
mean(HealthyOld$HighInfo)
sd(HealthyOld$HighInfo)
HealthyOld<- HealthyOld %>% mutate(LowMean=(HealthyOld$ShortlowMean + HealthyOld$LonglowMean)/2)
#mean(HealthyOld$LowMean)
#sd(HealthyOld$LowMean)


#mean((Young$DirectedExploration + HealthyOld$DirectedExploration)/2)
#sd((Young$DirectedExploration + HealthyOld$DirectedExploration)/2)


Young <-read_csv(here("YoungIndAcc.csv"), col_names = FALSE)
Young <- t(Young)
Young<-as.data.frame(Young)
colnames(Young)<- c("ShortHighInfo","LongHighInfo","DirectedExploration","ShortlowMean","LonglowMean","RandomExploration","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast","RepeatProbabilityFirst","RepeatProbabilitySecond","RepeatProbabilityThird","RepeatProbabilityFourth","RepeatProbabilityFifth","RepeatProbabilityLast","RTimeFirst","RTimeSecond","RTimeThird","RTimeFourth","RTimeFifth","RTimeLast","RepeatProbabilityFirstHOne","RTimeFirstHOne","fcorFirstHOne")
Young$SN<-c(102:106,108:115,116,117,119:125,127:129,131,132,134:140)

Young<- Young %>% mutate(HighInfo=(Young$ShortHighInfo + Young$LongHighInfo)/2)
#mean(Young$HighInfo)
#sd(Young$HighInfo)

Young<- Young %>% mutate(LowMean=(Young$ShortlowMean + Young$LonglowMean)/2)
#mean(Young$LowMean)
#sd(Young$LowMean)

#subset the old
#AmbiguityAverse<-subset(HealthyOld,HealthyOld$ShortHighInfo <= .25 & HealthyOld$LongHighInfo <= .25)
#HealthyOld<- subset(HealthyOld,HealthyOld$ShortHighInfo > .25 | HealthyOld$LongHighInfo > .25)


#grouping variable
HealthyOld$Group<-"Old"
Young$Group<-"Young"
#AmbiguityAverse$Group<-"AA"

mean(HealthyOld$DirectedExploration)
sd(HealthyOld$DirectedExploration)

mean(Young$DirectedExploration)
sd(Young$DirectedExploration)





#cor.test(HealthyOld$DirectedExploration,HealthyOld$RandomExploration)

#Put Together
fcYoungold<-rbind(HealthyOld,Young)

fcYoungold_long<-melt(fcYoungold, id.vars= c("SN", "Group"),measure.vars=c("fcorFirstHOne","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast"))


#p <- ggplot(fcYoungold_long, aes(x=variable, y=value,fill=Group))
#p + geom_boxplot(size=1.5, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("lightblue","red"))+ ggtitle("         Fraction Correct by Age Group") + xlab("Free Choice")+ylab("Repeat Probability")+theme_classic()

Order<-sort(rep(c(1,1:6),nrow(fcYoungold)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(fcYoungold)))
fcYoungold_long<-cbind(fcYoungold_long,Order,Horizon)

fcerc <- summarySE(fcYoungold_long, measurevar="value", groupvars=c("Order","Group","Horizon"))

Line<-ggplot(fcerc, aes(x=Order, y=value, group=Group, color=Group))
p<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2)) +
  geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("           Percent Correct in Free Trials Across Age Groups") + xlab("Free Trials") + ylab("Percent Correct") + scale_y_continuous(breaks = c(.7,.8,.9,1)) +
  theme(plot.title = element_text(hjust = 0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=24),legend.title = element_text(face="bold",size=30), legend.position="right",legend.justification = c("right","top")) + 
  #for removing legend
  #theme(plot.title = element_text(hjust = 0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=24),legend.title = element_text(face="bold",size=30), legend.position="none",legend.justification = c("right","top")) + 
  scale_color_manual(values=c('darkblue','darkred'))+ scale_shape_manual(values=c(15, 17)) +
  scale_fill_manual(name=c("Age Group","Horizon"))+labs(color="Age Group",shape="Horizon") +
  scale_x_discrete(limits=c("1","2","3","4","5","6"))

fcer6c<-subset(fcerc,fcerc$Horizon==6)
fcer6doubc<-subset(fcer6c,fcer6c$Order==1)
fcer6c<-rbind(fcer6c,fcer6doubc)

Line6<-ggplot(fcer6c, aes(x=Order, y=value, group=Group, color=Group))

q3<- p + geom_line(aes(x=fcer6c$Order, y=fcer6c$value, group=fcer6c$Group, color=fcer6c$Group),size=1.5) 

Ac1<-q3+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))


oldlongac<-subset(fcYoungold_long,Group=="Old")
Younglongac<-subset(fcYoungold_long,Group=="Young")


t.test(oldlongac$value,Younglongac$value)

mean(oldlongac$value)
mean(Younglongac$value)

sd(oldlongac$value)
sd(Younglongac$value)

###### Response Time #####
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h6_ 1','all_RT_h6_ 2','all_RT_h6_ 3','all_RT_h6_ 4','all_RT_h6_ 5','all_RT_h6_ 6','all_RT_h6_ 7','all_RT_h6_ 8','all_RT_h6_ 9','all_RT_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
# + Error(factor(subjectID))
#print(summary(ANOVA))
#TukeyHSD(ANOVA)

#OldRT1<-subset(RepeatYoungold_long,Condition=="old")
#YoungRT1<-subset(RepeatYoungold_long,Condition=="young")


#describe(OldRT1$value)
#describe(YoungRT1$value)





Order<-sort(rep(c(1:10),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
rt2<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.6) + geom_point(size=5, shape=17) +ggtitle("RT, horizon 6") + ylab("Reaction Time(in secs)") +
  theme_classic() + scale_color_manual(values=c('darkblue','darkred')) + scale_x_discrete(name ="Trial", limits=c("1","2","3","4","5","6","7","8","9","10"))+
theme(plot.title = element_text(hjust=0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24), legend.title = element_blank(),legend.position = "none") +ylim(0,3)+ylab(NULL)


###
###### Reaction Time ##### Horizon 1
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h1_ 1','all_RT_h1_ 2','all_RT_h1_ 3','all_RT_h1_ 4','all_RT_h1_ 5'))
ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
print(summary(ANOVA))
TukeyHSD(ANOVA)



#OldRT1<-subset(RepeatYoungold_long,Condition=="old")
#YoungRT1<-subset(RepeatYoungold_long,Condition=="young")


#describe(OldRT1$value)
#describe(YoungRT1$value)





Order<-sort(rep(c(1:5),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
rt1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.6) + geom_point(size=5,shape=15) + ggtitle("RT, horizon 1") + ylab("Reaction Time(in secs)") +
  theme_classic() + scale_color_manual(values=c('darkblue','darkred')) + scale_x_discrete(name ="Trial", limits=c("1","2","3","4","5"))+
  theme(plot.title = element_text(hjust=0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.title = element_blank(),legend.position="none")+ylim(0,3)




figure2 <- multi_panel_figure(width=270,height=360,columns = 5, rows = 2,figure_name="Figure 4. Overall Performance")

figure2 %<>% 
fill_panel(Ac1,column=1:5,row=1) %>%
fill_panel(rt1,column=1:2,row=2) %>%
fill_panel(rt2, column = 3:5, row=2)
figure2

#16,12


# For analysis of all free trials RT
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h6_ 5','all_RT_h6_ 6','all_RT_h6_ 7','all_RT_h6_ 8','all_RT_h6_ 9','all_RT_h6_10','all_RT_h1_ 5'))

OldRT1<-subset(RepeatYoungold_long,Condition=="old")
YoungRT1<-subset(RepeatYoungold_long,Condition=="young")


t.test(OldRT1$value,YoungRT1$value)


sd(OldRT1$value)
sd(YoungRT1$value)





###now do ambiguity averse###
Old<-subset(data,data$Condition=="old")
AmbiguityAverse<-subset(Old,Old$p_hi_13_1 <= .25 & Old$p_hi_13_2 <= .25)
AmbiguityAverse$Condition<-"AA"

Old<- subset(Old,Old$p_hi_13_1 > .25 | Old$p_hi_13_2 > .25)

Young<-subset(data,data$Condition=="young")
Young<- subset(Young,Young$p_hi_13_1 > .25 | Young$p_hi_13_2 > .25)


data<-rbind(Old,AmbiguityAverse,Young)



RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_ 5','all_p_hi_h6_ 5'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)


Order<-sort(rep(c(1,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)



ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))


Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
d1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.5) + geom_point(size=5) +ggtitle("Directed") + xlab("Horizon") + ylab("p(high info)") +
  theme_classic() + scale_color_manual(values=c('lightblue','darkblue','darkred')) +ylim(0,.65) + scale_x_discrete(limits=c(1,6))+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none"))

Point<-ggplot(data, aes(x=p_hi_13_1,y=p_hi_13_2,fill=Condition,color=Condition))
d2<-Point+geom_point(size=5)+ ggtitle("Directed") + xlab("p(high info, horizon 1)") + ylab("p(high info, horizon 6)") +
  scale_color_manual(values=c('lightblue','darkblue','darkred')) + scale_x_continuous(breaks= c(0,.2,.4,.6,.8,1),limits= c(0,1)) + scale_y_continuous(breaks= c(0,.2,.4,.6,.8,1.0),limits= c(0,1)) +
  theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none")) +
  geom_abline(intercept = 0, slope = 1,linetype="dashed",size=1)



##### Get in Horizon 1 and 6 Comparison probability of picking low mean######
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h1_ 5','all_p_lm_h6_ 5'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)


Order<-sort(rep(c(1,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

ReaPer$Group<-ReaPer$Condition
Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Group, color=Group))
r1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.5) + geom_point(size=5) +ggtitle("Random") + xlab("Horizon") + ylab("p(low mean)") +
  theme_classic() + scale_color_manual(values=c('lightblue','darkblue','darkred')) + ylim(0,.65)+scale_x_discrete(limits=c(1,6))+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=30),legend.title = element_text(face="bold",size=30), legend.position=c(.99,.99),legend.justification = c("right","top"))

Point<-ggplot(data, aes(x=p_lm_13_1,y=p_lm_13_2,fill=Condition,color=Condition))
r2<-Point+geom_point(size=5)+ ggtitle("Random") + xlab("p(low mean,horizon 1)") + ylab("p(low mean,horizon 6)") +
  scale_color_manual(values=c('lightblue','darkblue','darkred')) + scale_x_continuous(breaks= c(0,.2,.4,.6,.8,1),limits= c(0,1)) + scale_y_continuous(breaks= c(0,.2,.4,.6,.8,1.0),limits= c(0,1)) +
  theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none"))+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",size=1)


figure5 <- multi_panel_figure(width=360,height=360,columns = 2, rows = 2,row_spacing = 10)


figure5 %<>% 
  fill_panel(d1,column=1,row=1) %>%
  fill_panel(r1,column=2,row=1) %>%
  fill_panel(d2,column=1,row=2) %>%
  fill_panel(r2,column=2,row=2) 
figure5




##### Get in Horizon 6 probability of picking more informative option
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_ 5','all_p_hi_h6_ 5','all_p_hi_h6_ 6','all_p_hi_h6_ 7','all_p_hi_h6_ 8','all_p_hi_h6_ 9','all_p_hi_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)


Order<-sort(rep(c(5,5:10),nrow(data)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order,Horizon)



##### Get in Horizon 6 probability of picking more informative option#### for ANOVA
#RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_5','all_p_hi_h6_ 5','all_p_hi_h6_ 6','all_p_hi_h6_ 7','all_p_hi_h6_ 8','all_p_hi_h6_ 9','all_p_hi_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)


#AOrder<-sort(rep(c(5:10),nrow(data)))
#Horizon<-sort(rep(c(6,6,6,6,6,6),nrow(data)))
#ARepeatYoungold_long<-cbind(ARepeatYoungold_long,AOrder)
#oldA<-subset(ARepeatYoungold_long,Condition=="old")
#youngA<-subset(ARepeatYoungold_long,Condition=="young")

#ANOVA<-aov(data=ARepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#mean(oldA$value)
#sd(oldA$value)


#mean(youngA$value)
#sd(youngA$value)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition","Horizon"))
#head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
p1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("p(high info),Free Trials") + xlab("Free Trials") + ylab("p(high info)") +
  scale_color_manual(values=c('lightblue','darkblue','darkred')) + ylim(0,.75)+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 36, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position = "none")+
  scale_shape_manual(values=c(15, 17))+
  labs(color="Age Group",shape="Horizon")

fcer6<-subset(ReaPer,ReaPer$Horizon==6)
fcer1<-subset(ReaPer,ReaPer$Horizon==1)
fcer1doub<-subset(fcer6,fcer6$Order==5)
fcer6a<-rbind(fcer6,fcer1doub)

Line6<-ggplot(fcer6, aes(x=Order, y=value, group=Condition, color=Condition))

q1<- p1 + geom_line(aes(x=fcer6a$Order, y=fcer6a$value, group=fcer6a$Condition, color=fcer6a$Condition),size=1.5) 

q1
#hi1<-q1+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))




#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
##### Get in Horizon 6 probability of picking low mean
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h1_ 5','all_p_lm_h6_ 5','all_p_lm_h6_ 6','all_p_lm_h6_ 7','all_p_lm_h6_ 8','all_p_lm_h6_ 9','all_p_lm_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)

#BRepeatYoungold_long<-melt(PatternHor6, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h6_ 5','all_p_lm_h6_ 6','all_p_lm_h6_ 7','all_p_lm_h6_ 8','all_p_lm_h6_ 9','all_p_lm_h6_10'))

#BOrder<-sort(rep(c(5:10),nrow(data)))
#Horizon<-sort(rep(c(6,6,6,6,6,6),nrow(data)))
#BRepeatYoungold_long<-cbind(BRepeatYoungold_long,AOrder)
#oldB<-subset(BRepeatYoungold_long,Condition=="old")
#youngB<-subset(BRepeatYoungold_long,Condition=="young")

#ANOVA<-aov(data=BRepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#mean(oldB$value)
#sd(oldB$value)


#mean(youngB$value)
#sd(youngB$value)

Order<-sort(rep(c(5,5:10),nrow(data)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(data)))

RepeatYoungold_long<-cbind(RepeatYoungold_long,Order,Horizon)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition","Horizon"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
p2<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("p(low mean),Free Trials") + xlab("Free Trials") + ylab("p(low mean)") +
  scale_color_manual(values=c('lightblue','darkblue','darkred')) + ylim(0,.75)+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 36, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position = "none")+
  scale_shape_manual(values=c(15, 17))+
  labs(color="Age Group",shape="Horizon")

fcer6<-subset(ReaPer,ReaPer$Horizon==6)
fcer1<-subset(ReaPer,ReaPer$Horizon==1)
fcer1doub<-subset(fcer6,fcer6$Order==5)
fcer6b<-rbind(fcer6,fcer1doub)

#Line6b<-ggplot(fcer6, aes(x=Order, y=value, group=Condition, color=Condition))

q2<- p2 + geom_line(aes(x=fcer6b$Order, y=fcer6b$value, group=fcer6b$Condition, color=fcer6b$Condition), size=1.5) 


#q2+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))


#q2


#grid.arrange(q1, q2, nrow = 1)


figure6 <- multi_panel_figure(width=300,height=180,columns = 2, rows = 1,figure_name="Figure 4. Overall Performance",column_spacing = 20)

figure6 %<>% 
  fill_panel(q1,column=1,row=1) %>%
  fill_panel(q2,column=2,row=1) 

figure6

#####
#TukeyHSD(ANOVA)

#Old vs. Young FC


#get the data in, the ones who play accurately
HealthyOld <- read_csv(here("OldAcc.csv"), col_names = FALSE)
HealthyOld <- t(HealthyOld)
HealthyOld<-as.data.frame(HealthyOld)
colnames(HealthyOld)<- c("ShortHighInfo","LongHighInfo","DirectedExploration","ShortlowMean","LonglowMean","RandomExploration","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast","RepeatProbabilityFirst","RepeatProbabilitySecond","RepeatProbabilityThird","RepeatProbabilityFourth","RepeatProbabilityFifth","RepeatProbabilityLast","RTimeFirst","RTimeSecond","RTimeThird","RTimeFourth","RTimeFifth","RTimeLast","RepeatProbabilityFirstHOne","RTimeFirstHOne","fcorFirstHOne")
HealthyOld$SN<-c(1,2,4:14,16,17,20,23,25:29,32,34,39:43,45,46,48:50,53,54,56,57,59:61,63,64)

HealthyOld<- HealthyOld %>% mutate(HighInfo=(HealthyOld$ShortHighInfo + HealthyOld$LongHighInfo)/2)
mean(HealthyOld$HighInfo)
sd(HealthyOld$HighInfo)
HealthyOld<- HealthyOld %>% mutate(LowMean=(HealthyOld$ShortlowMean + HealthyOld$LonglowMean)/2)
#mean(HealthyOld$LowMean)
#sd(HealthyOld$LowMean)


#mean((Young$DirectedExploration + HealthyOld$DirectedExploration)/2)
#sd((Young$DirectedExploration + HealthyOld$DirectedExploration)/2)


Young <-read_csv(here("YoungIndAcc.csv"), col_names = FALSE)
Young <- t(Young)
Young<-as.data.frame(Young)
colnames(Young)<- c("ShortHighInfo","LongHighInfo","DirectedExploration","ShortlowMean","LonglowMean","RandomExploration","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast","RepeatProbabilityFirst","RepeatProbabilitySecond","RepeatProbabilityThird","RepeatProbabilityFourth","RepeatProbabilityFifth","RepeatProbabilityLast","RTimeFirst","RTimeSecond","RTimeThird","RTimeFourth","RTimeFifth","RTimeLast","RepeatProbabilityFirstHOne","RTimeFirstHOne","fcorFirstHOne")
Young$SN<-c(102:106,108:115,116,117,119:125,127:129,131,132,134:140)

Young<- Young %>% mutate(HighInfo=(Young$ShortHighInfo + Young$LongHighInfo)/2)
#mean(Young$HighInfo)
#sd(Young$HighInfo)

Young<- Young %>% mutate(LowMean=(Young$ShortlowMean + Young$LonglowMean)/2)
#mean(Young$LowMean)
#sd(Young$LowMean)

#subset the old
AmbiguityAverse<-subset(HealthyOld,HealthyOld$ShortHighInfo <= .25 & HealthyOld$LongHighInfo <= .25)
HealthyOld<- subset(HealthyOld,HealthyOld$ShortHighInfo > .25 | HealthyOld$LongHighInfo > .25)
Young<- subset(Young,Young$ShortHighInfo > .25 | Young$LongHighInfo > .25)

#grouping variable
HealthyOld$Group<-"Old"
Young$Group<-"Young"
AmbiguityAverse$Group<-"AA"


Youngold<-rbind(Young,HealthyOld,AmbiguityAverse)
#Put Together
fcYoungold<-rbind(Young,HealthyOld,AmbiguityAverse)

fcYoungold_long<-melt(fcYoungold, id.vars= c("SN", "Group"),measure.vars=c("fcorFirstHOne","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast"))


#p <- ggplot(fcYoungold_long, aes(x=variable, y=value,fill=Group))
#p + geom_boxplot(size=1.5, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("lightblue","red"))+ ggtitle("         Fraction Correct by Age Group") + xlab("Free Choice")+ylab("Repeat Probability")+theme_classic()

Order<-sort(rep(c(1,1:6),nrow(fcYoungold)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(fcYoungold)))
fcYoungold_long<-cbind(fcYoungold_long,Order,Horizon)

fcerc <- summarySE(fcYoungold_long, measurevar="value", groupvars=c("Order","Group","Horizon"))

Line<-ggplot(fcerc, aes(x=Order, y=value, group=Group, color=Group))
p<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2)) +
  geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("           Percent Correct in Free Trials Across Groups") + xlab("Free Trials") + ylab("Percent Correct") + scale_y_continuous(breaks = c(.7,.8,.9,1)) +
  theme(plot.title = element_text(hjust = 0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=24),legend.title = element_text(face="bold",size=30),legend.position="right",legend.justification = c("right","top")) + 
  scale_color_manual(values=c('lightblue','darkblue','darkred'))+ scale_shape_manual(values=c(15, 17)) +
  scale_fill_manual(name=c("Group","Horizon"))+labs(color="Group",shape="Horizon") +
  scale_x_discrete(limits=c("1","2","3","4","5","6"))

fcer6c<-subset(fcerc,fcerc$Horizon==6)
fcer6doubc<-subset(fcer6c,fcer6c$Order==1)
fcer6c<-rbind(fcer6c,fcer6doubc)

Line6<-ggplot(fcer6c, aes(x=Order, y=value, group=Group, color=Group))

q3<- p + geom_line(aes(x=fcer6c$Order, y=fcer6c$value, group=fcer6c$Group, color=fcer6c$Group),size=1.5) 

Ac1<-q3+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))




###### Reaction Time #####
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h6_ 1','all_RT_h6_ 2','all_RT_h6_ 3','all_RT_h6_ 4','all_RT_h6_ 5','all_RT_h6_ 6','all_RT_h6_ 7','all_RT_h6_ 8','all_RT_h6_ 9','all_RT_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
# + Error(factor(subjectID))
#print(summary(ANOVA))
#TukeyHSD(ANOVA)

#OldRT1<-subset(RepeatYoungold_long,Condition=="old")
#YoungRT1<-subset(RepeatYoungold_long,Condition=="young")


#describe(OldRT1$value)
#describe(YoungRT1$value)





Order<-sort(rep(c(1:10),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
rt2<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.6) + geom_point(size=5, shape=17) +ggtitle("RT, horizon 6") + ylab("Response Time(in secs)") +
  theme_classic() + scale_color_manual(values=c('lightblue','darkblue','darkred')) + scale_x_discrete(name ="Trial", limits=c("1","2","3","4","5","6","7","8","9","10"))+
  theme(plot.title = element_text(hjust=0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24), legend.title = element_blank(),legend.position = "none") +ylim(0,3.5)+ylab(NULL)


###
###### Reaction Time ##### Horizon 1
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h1_ 1','all_RT_h1_ 2','all_RT_h1_ 3','all_RT_h1_ 4','all_RT_h1_ 5'))
ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
print(summary(ANOVA))
TukeyHSD(ANOVA)



#OldRT1<-subset(RepeatYoungold_long,Condition=="old")
#YoungRT1<-subset(RepeatYoungold_long,Condition=="young")


#describe(OldRT1$value)
#describe(YoungRT1$value)





Order<-sort(rep(c(1:5),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
rt1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.6) + geom_point(size=5,shape=15) + ggtitle("RT, horizon 1") + ylab("Response Time(in secs)") +
  theme_classic() + scale_color_manual(values=c('lightblue','darkblue','darkred')) + scale_x_discrete(name ="Trial", limits=c("1","2","3","4","5"))+
  theme(plot.title = element_text(hjust=0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.title = element_blank(),legend.position="none")+ylim(0,3.5)


library(multipanelfigure)
library(magrittr)

figure7 <- multi_panel_figure(width=270,height=360,columns = 5, rows = 2,figure_name="Figure 4. Overall Performance")

figure7 %<>% 
  fill_panel(Ac1,column=1:5,row=1) %>%
  fill_panel(rt1,column=1:2,row=2) %>%
  fill_panel(rt2, column = 3:5, row=2)
figure7


#16,12



###okay now do it all without exclusion of those with accuracy below .60###

#get the data in
data<- read_csv(here("AgingInconsistent.csv"), col_names = TRUE)

#old, remove for MoCA
data<-subset(data,data$subjectID!=3)
data<-subset(data,data$subjectID!=15)
data<-subset(data,data$subjectID!=18)
data<-subset(data,data$subjectID!=19)
data<-subset(data,data$subjectID!=21)
data<-subset(data,data$subjectID!=22)
data<-subset(data,data$subjectID!=24)
data<-subset(data,data$subjectID!=30)
data<-subset(data,data$subjectID!=31)

Old<-subset(data,data$Condition=="old")
Young<-subset(data,data$Condition=="young")















# For [3 1]
#RepeatYoungold_long<-melt(BehVar, id.vars= c("subjectID","Condition"),measure.vars=c("p_inconsistent_13_1","p_inconsistent_13_2"))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#p <- ggplot(RepeatYoungold_long, aes(x=variable, y=value,fill=Condition))
#p + geom_boxplot(size=1.2, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("darkblue","red","blue"))+ ggtitle("Behavioral Variability in [3 1] by Age Group") + xlab("Horizon")+ylab("p(inconsistent)")+scale_x_discrete(labels=c("1","6")) + ylim(0,.8)+
#theme(plot.title = element_text(color = "black", size = 16, face = "bold"),axis.title =element_text(color = "black", size = 14, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=12))


# For [2 2]
#RepeatYoungold_long<-melt(BehVar, id.vars= c("subjectID","Condition"),measure.vars=c("p_inconsistent_22_1","p_inconsistent_22_2"))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#p <- ggplot(RepeatYoungold_long, aes(x=variable, y=value,fill=Condition))
#p + geom_boxplot(size=1.2, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("darkblue","red","blue"))+ ggtitle("Behavioral Variability in [2 2] by Age Group") + xlab("Horizon")+ylab("p(inconsistent)")+scale_x_discrete(labels=c("1","6"))+ylim(0,.8)+
#theme(plot.title = element_text(color = "black", size = 16, face = "bold"),axis.title =element_text(color = "black", size = 14, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=12))

# Overall
#RepeatYoungold_long<-melt(BehVar, id.vars= c("subjectID","Condition"),measure.vars=c("p_inconsistent_1","p_inconsistent_2"))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#p <- ggplot(RepeatYoungold_long, aes(x=variable, y=value,fill=Condition))
#p + geom_boxplot(size=1.5, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("darkblue","red","blue"))+ ggtitle("         Behavioral Variability by Age Group") + xlab("Horizon")+ylab("p(inconsistent)") +scale_x_discrete(labels=c("1","6")) +
#theme(plot.title = element_text(color = "black", size = 16, face = "bold"),axis.title =element_text(color = "black", size = 14, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=12))
#panel.background = element_blank(), axis.line = element_line(colour = "black"))


# Within Age Group Comparison


###Pattern Repeat###
#PatternHor1<-data

#Change it from switch to Repeat
#PatternHor1$Pattern_p_rp_h1_1<-1-PatternHor1$Pattern_p_rp_h1_1
#PatternHor1$Pattern_p_rp_h1_2<-1-PatternHor1$Pattern_p_rp_h1_2
#PatternHor1$Pattern_p_rp_h1_3<-1-PatternHor1$Pattern_p_rp_h1_3
#PatternHor1$Pattern_p_rp_h1_4<-1-PatternHor1$Pattern_p_rp_h1_4
#PatternHor1$Pattern_p_rp_h1_5<-1-PatternHor1$Pattern_p_rp_h1_5
#PatternHor1$Pattern_p_rp_h1_6<-1-PatternHor1$Pattern_p_rp_h1_6







# [3 1] h1
#RepeatYoungold_long<-melt(PatternHor1, id.vars= c("subjectID","Condition"),measure.vars=c("Pattern_p_rp_h1_1","Pattern_p_rp_h1_4","Pattern_p_rp_h1_6","Pattern_p_rp_h1_7"))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#p <- ggplot(RepeatYoungold_long, aes(x=variable, y=value,fill=Condition))
#p + geom_boxplot(size=1.5, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("darkblue","red","blue"))+ ggtitle("p(repeat) by Trial Pattern [3 1] ") + xlab("Trial Pattern")+ylab("p(repeat)")+scale_x_discrete(labels=c("RLLL","RLRR","RRLR","RRRL"))+
#theme(plot.title = element_text(color = "black", size = 16, face = "bold"),axis.title =element_text(color = "black", size = 14, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=12))




# [2 2] h1
#RepeatYoungold_long<-melt(PatternHor1, id.vars= c("subjectID","Condition"),measure.vars=c("Pattern_p_rp_h1_2","Pattern_p_rp_h1_3","Pattern_p_rp_h1_5"))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#p <- ggplot(RepeatYoungold_long, aes(x=variable, y=value,fill=Condition))
#p + geom_boxplot(size=1.5, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("darkblue","red","blue"))+ ggtitle("p(repeat) by Trial Pattern [2 2]") + xlab("Trial Pattern")+ylab("p(repeat)")+scale_x_discrete(labels=c("RLLR","RLRL","RRLL"))+
# theme(plot.title = element_text(color = "black", size = 16, face = "bold"),axis.title =element_text(color = "black", size = 14, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=12))





###Pattern Repeat###
#PatternHor6<-data

#Change it from switch to Repeat
#PatternHor6$Pattern_p_rp_h6_1<-1-PatternHor6$Pattern_p_rp_h6_1
#PatternHor6$Pattern_p_rp_h6_2<-1-PatternHor6$Pattern_p_rp_h6_2
#PatternHor6$Pattern_p_rp_h6_3<-1-PatternHor6$Pattern_p_rp_h6_3
#PatternHor6$Pattern_p_rp_h6_4<-1-PatternHor6$Pattern_p_rp_h6_4
#PatternHor6$Pattern_p_rp_h6_5<-1-PatternHor6$Pattern_p_rp_h6_5
#PatternHor6$Pattern_p_rp_h6_6<-1-PatternHor6$Pattern_p_rp_h6_6


# [3 1] h6
#RepeatYoungold_long<-melt(PatternHor6, id.vars= c("subjectID","Condition"),measure.vars=c("Pattern_p_rp_h6_1","Pattern_p_rp_h6_4","Pattern_p_rp_h6_6","Pattern_p_rp_h6_7"))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#p <- ggplot(RepeatYoungold_long, aes(x=variable, y=value,fill=Condition))
#p + geom_boxplot(size=1.5, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("darkblue","red","blue"))+ ggtitle("p(repeat) by Trial Pattern [3 1] ") + xlab("Trial Pattern")+ylab("p(repeat)")+scale_x_discrete(labels=c("RLLL","RLRR","RRLR","RRRL"))+
#  theme(plot.title = element_text(color = "black", size = 16, face = "bold"),axis.title =element_text(color = "black", size = 14, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=12))


# [2 2] h6 
#RepeatYoungold_long<-melt(PatternHor6, id.vars= c("subjectID","Condition"),measure.vars=c("Pattern_p_rp_h6_2","Pattern_p_rp_h6_3","Pattern_p_rp_h6_5"))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#p <- ggplot(RepeatYoungold_long, aes(x=variable, y=value,fill=Condition))
#p + geom_boxplot(size=1.5, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("darkblue","red","blue"))+ ggtitle("p(repeat) by Trial Pattern [2 2]") + xlab("Trial Pattern")+ylab("p(repeat)")+scale_x_discrete(labels=c("RLLR","RLRL","RRLL"))+
#  theme(plot.title = element_text(color = "black", size = 16, face = "bold"),axis.title =element_text(color = "black", size = 14, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=12))



##################Figure 3#####################################################################
##### Get in Horizon 1 and 6 Comparison probability of picking high info ######
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_ 5','all_p_hi_h6_ 5'))
ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
print(summary(ANOVA))
#TukeyHSD(ANOVA)


Order<-sort(rep(c(1,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)



ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))






head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
d1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.5) + geom_point(size=5) +ggtitle("Directed") + xlab("Horizon") + ylab("p(high info)") +
  theme_classic() + scale_color_manual(values=c('darkblue','darkred')) +ylim(0.27,0.61) + scale_x_discrete(limits=c(1,6))+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none"))

Point<-ggplot(data, aes(x=p_hi_13_1,y=p_hi_13_2,fill=Condition,color=Condition))
d2<-Point+geom_point(size=5)+ ggtitle("Directed") + xlab("p(high info, horizon 1)") + ylab("p(high info, horizon 6)") +
  scale_color_manual(values=c('darkblue','darkred')) + scale_x_continuous(breaks= c(0,.2,.4,.6,.8,1),limits= c(0,1)) + scale_y_continuous(breaks= c(0,.2,.4,.6,.8,1),limits= c(.0,1.0)) +
  theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none")) +
  geom_abline(intercept = 0, slope = 1,linetype="dashed",size=1)



##### Get in Horizon 1 and 6 Comparison probability of picking low mean######
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h1_ 5','all_p_lm_h6_ 5'))
ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
print(summary(ANOVA))
TukeyHSD(ANOVA)


Order<-sort(rep(c(1,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition*Order)


ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
r1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.5) + geom_point(size=5) +ggtitle("Random") + xlab("Horizon") + ylab("p(low mean)") +
  theme_classic() + scale_color_manual(values=c('darkblue','darkred')) + ylim(.1,.44)+scale_x_discrete(limits=c(1,6))+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=30),legend.title = element_text(face="bold",size=30), legend.position=c(.99,.99),legend.justification = c("right","top"))+
  labs(fill="Age Group")

Point<-ggplot(data, aes(x=p_lm_13_1,y=p_lm_13_2,fill=Condition,color=Condition))
r2<-Point+geom_point(size=5)+ ggtitle("Random") + xlab("p(low mean,horizon 1)") + ylab("p(low mean,horizon 6)") +
  scale_color_manual(values=c('darkblue','darkred')) + scale_x_continuous(breaks= c(0,.2,.4,.6,.8,1),limits= c(0,1)) + scale_y_continuous(breaks= c(0,.2,.4,.6,.8,1.0),limits= c(0,1)) +
  theme(plot.title = element_text(hjust=.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"),axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position=c("none"))+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",size=1)


figure8 <- multi_panel_figure(width=360,height=360,columns = 2, rows = 2, row_spacing=10)


figure8 %<>% 
  fill_panel(d1,column=1,row=1) %>%
  fill_panel(r1,column=2,row=1) %>%
  fill_panel(d2,column=1,row=2) %>%
  fill_panel(r2,column=2,row=2) 
figure8

#16,16





#####Figure 4####

##### Get in Horizon 6 probability of picking more informative option
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_ 5','all_p_hi_h6_ 5','all_p_hi_h6_ 6','all_p_hi_h6_ 7','all_p_hi_h6_ 8','all_p_hi_h6_ 9','all_p_hi_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)


Order<-sort(rep(c(5,5:10),nrow(data)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order,Horizon)



##### Get in Horizon 6 probability of picking more informative option#### for ANOVA
#RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h1_5','all_p_hi_h6_ 5','all_p_hi_h6_ 6','all_p_hi_h6_ 7','all_p_hi_h6_ 8','all_p_hi_h6_ 9','all_p_hi_h6_10'))

RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_hi_h6_ 5','all_p_hi_h6_ 6','all_p_hi_h6_ 7','all_p_hi_h6_ 8','all_p_hi_h6_ 9','all_p_hi_h6_10'))
ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
print(summary(ANOVA))
TukeyHSD(ANOVA)


#AOrder<-sort(rep(c(5:10),nrow(data)))
#Horizon<-sort(rep(c(6,6,6,6,6,6),nrow(data)))
#ARepeatYoungold_long<-cbind(ARepeatYoungold_long,AOrder)
#oldA<-subset(ARepeatYoungold_long,Condition=="old")
#youngA<-subset(ARepeatYoungold_long,Condition=="young")

#ANOVA<-aov(data=ARepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#mean(oldA$value)
#sd(oldA$value)


#mean(youngA$value)
#sd(youngA$value)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition","Horizon"))
#head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
p1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("p(high info), Free Trials") + xlab("Free Trials") + ylab("p(high info)") +
  scale_color_manual(values=c('darkblue','darkred')) + ylim(0,.61)+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 36, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position = "none")+
  scale_shape_manual(values=c(15, 17))


fcer6<-subset(ReaPer,ReaPer$Horizon==6)
fcer1<-subset(ReaPer,ReaPer$Horizon==1)
fcer1doub<-subset(fcer6,fcer6$Order==5)
fcer6a<-rbind(fcer6,fcer1doub)

Line6<-ggplot(fcer6, aes(x=Order, y=value, group=Condition, color=Condition))

q1<- p1 + geom_line(aes(x=fcer6a$Order, y=fcer6a$value, group=fcer6a$Condition, color=fcer6a$Condition),size=1.5) 

q1
#hi1<-q1+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))




#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
##### Get in Horizon 6 probability of picking low mean
#RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h1_ 5','all_p_lm_h6_ 5','all_p_lm_h6_ 6','all_p_lm_h6_ 7','all_p_lm_h6_ 8','all_p_lm_h6_ 9','all_p_lm_h6_10'))
#ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))
#TukeyHSD(ANOVA)

#For ANOVA
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h6_ 5','all_p_lm_h6_ 6','all_p_lm_h6_ 7','all_p_lm_h6_ 8','all_p_lm_h6_ 9','all_p_lm_h6_10'))
ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
print(summary(ANOVA))



#BRepeatYoungold_long<-melt(PatternHor6, id.vars= c("subjectID", "Condition"),measure.vars=c('all_p_lm_h6_ 5','all_p_lm_h6_ 6','all_p_lm_h6_ 7','all_p_lm_h6_ 8','all_p_lm_h6_ 9','all_p_lm_h6_10'))

#BOrder<-sort(rep(c(5:10),nrow(data)))
#Horizon<-sort(rep(c(6,6,6,6,6,6),nrow(data)))
#BRepeatYoungold_long<-cbind(BRepeatYoungold_long,AOrder)
#oldB<-subset(BRepeatYoungold_long,Condition=="old")
#youngB<-subset(BRepeatYoungold_long,Condition=="young")

#ANOVA<-aov(data=BRepeatYoungold_long,value~variable*Condition)
#print(summary(ANOVA))

#mean(oldB$value)
#sd(oldB$value)


#mean(youngB$value)
#sd(youngB$value)

Order<-sort(rep(c(5,5:10),nrow(data)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(data)))

RepeatYoungold_long<-cbind(RepeatYoungold_long,Order,Horizon)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition","Horizon"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
p2<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("p(low mean), Free Trials") + xlab("Free Trials") + ylab("p(high info)") +
  scale_color_manual(values=c('darkblue','darkred')) + ylim(0,.61)+
  theme(plot.title = element_text(hjust=.5,color = "black", size = 36, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.position = "none")+
  scale_shape_manual(values=c(15, 17))






#p2<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
#geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("p(low mean), Free Trials") + xlab("Free Trials") + ylab("p(low mean)") +
#scale_color_manual(values=c('darkblue','darkred')) + ylim(0,.61)+
#theme(plot.title = element_text(hjust=.5,color = "black", size = 36, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=24),legend.title = element_text(face="bold",size=30), legend.position=c(.99,.99),legend.justification = c("right","top"))
#scale_shape_manual(values=c(15, 17))

p2

fcer6<-subset(ReaPer,ReaPer$Horizon==6)
fcer1<-subset(ReaPer,ReaPer$Horizon==1)
fcer1doub<-subset(fcer6,fcer6$Order==5)
fcer6b<-rbind(fcer6,fcer1doub)

#Line6b<-ggplot(fcer6, aes(x=Order, y=value, group=Condition, color=Condition))

q2<- p2 + geom_line(aes(x=fcer6b$Order, y=fcer6b$value, group=fcer6b$Condition, color=fcer6b$Condition), size=1.5) 

q2

#q2+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))


#q2


#grid.arrange(q1, q2, nrow = 1)


figure9 <- multi_panel_figure(width=300,height=180,columns = 2, rows = 1,figure_name="Figure 4. Overall Performance",column_spacing = 20)

figure9 %<>% 
  fill_panel(q1,column=1,row=1) %>%
  fill_panel(q2,column=2,row=1) 

figure9


##### Overall Accuracy and Reaction Time
#get the data in, the ones who play accurately
HealthyOld <- read_csv(here("OldNoAcc.csv"), col_names = FALSE)
HealthyOld <- t(HealthyOld)
HealthyOld<-as.data.frame(HealthyOld)
colnames(HealthyOld)<- c("ShortHighInfo","LongHighInfo","DirectedExploration","ShortlowMean","LonglowMean","RandomExploration","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast","RepeatProbabilityFirst","RepeatProbabilitySecond","RepeatProbabilityThird","RepeatProbabilityFourth","RepeatProbabilityFifth","RepeatProbabilityLast","RTimeFirst","RTimeSecond","RTimeThird","RTimeFourth","RTimeFifth","RTimeLast","RepeatProbabilityFirstHOne","RTimeFirstHOne","fcorFirstHOne")
HealthyOld$SN<-c(1,2,4:14,16,17,20,23,25:29,32:34,39:43,45:48,50,51,52,53,54,56,57,59:64)

HealthyOld<- HealthyOld %>% mutate(HighInfo=(HealthyOld$ShortHighInfo + HealthyOld$LongHighInfo)/2)
mean(HealthyOld$HighInfo)
sd(HealthyOld$HighInfo)
HealthyOld<- HealthyOld %>% mutate(LowMean=(HealthyOld$ShortlowMean + HealthyOld$LonglowMean)/2)
#mean(HealthyOld$LowMean)
#sd(HealthyOld$LowMean)


#mean((Young$DirectedExploration + HealthyOld$DirectedExploration)/2)
#sd((Young$DirectedExploration + HealthyOld$DirectedExploration)/2)


Young <-read_csv(here("YoungNoAcc.csv"), col_names = FALSE)
Young <- t(Young)
Young<-as.data.frame(Young)
colnames(Young)<- c("ShortHighInfo","LongHighInfo","DirectedExploration","ShortlowMean","LonglowMean","RandomExploration","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast","RepeatProbabilityFirst","RepeatProbabilitySecond","RepeatProbabilityThird","RepeatProbabilityFourth","RepeatProbabilityFifth","RepeatProbabilityLast","RTimeFirst","RTimeSecond","RTimeThird","RTimeFourth","RTimeFifth","RTimeLast","RepeatProbabilityFirstHOne","RTimeFirstHOne","fcorFirstHOne")
Young$SN<-c(102:140)

Young<- Young %>% mutate(HighInfo=(Young$ShortHighInfo + Young$LongHighInfo)/2)
#mean(Young$HighInfo)
#sd(Young$HighInfo)

Young<- Young %>% mutate(LowMean=(Young$ShortlowMean + Young$LonglowMean)/2)
#mean(Young$LowMean)
#sd(Young$LowMean)

HealthyOld$Group<-"Old"
Young$Group<-"Young"


#Put Together
fcYoungold<-rbind(HealthyOld,Young)

#for graphing
fcYoungold_long<-melt(fcYoungold, id.vars= c("SN", "Group"),measure.vars=c("fcorFirstHOne","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast"))

#Horizon 1 comparison
fcYoungold_long<-melt(fcYoungold, id.vars= c("SN", "Group"),measure.vars=c("fcorFirstHOne","fcFirst"))

ANOVA<-aov(data=fcYoungold_long,value~variable*Group)
# + Error(factor(subjectID))
print(summary(ANOVA))


#Horizon 6 comparison
fcYoungold_long<-melt(fcYoungold, id.vars= c("SN", "Group"),measure.vars=c("fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast"))
#fcYoungold_long<-melt(fcYoungold, id.vars= c("SN", "Group"),measure.vars=c("fcorFirstHOne","fcFirst","fcSecond","fcThird","fcFourth","fcFifth","fcLast"))

ANOVA<-aov(data=fcYoungold_long,value~Group)
# + Error(factor(subjectID))
print(summary(ANOVA))

#p <- ggplot(fcYoungold_long, aes(x=variable, y=value,fill=Group))
#p + geom_boxplot(size=1.5, alpha=1.0, outlier.shape=NA, notch=TRUE)+scale_fill_manual(values=c("lightblue","red"))+ ggtitle("         Fraction Correct by Age Group") + xlab("Free Choice")+ylab("Repeat Probability")+theme_classic()

Order<-sort(rep(c(1,1:6),nrow(fcYoungold)))
Horizon<-sort(rep(c(1,6,6,6,6,6,6),nrow(fcYoungold)))
fcYoungold_long<-cbind(fcYoungold_long,Order,Horizon)

fcerc <- summarySE(fcYoungold_long, measurevar="value", groupvars=c("Order","Group","Horizon"))

Line<-ggplot(fcerc, aes(x=Order, y=value, group=Group, color=Group))
p<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2)) +
  geom_point(aes(shape=factor(Horizon)),size=5) +ggtitle("           Percent Correct in Free Trials Across Age Groups") + xlab("Free Trials") + ylab("Percent Correct") + scale_y_continuous(breaks = c(.7,.8,.9,1)) +
  theme(plot.title = element_text(hjust = 0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=24),legend.title = element_text(face="bold",size=30), legend.position="right",legend.justification = c("right","top")) + 
  #for removing legend
  #theme(plot.title = element_text(hjust = 0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.key= element_rect(fill="white",colour="black"),legend.text=element_text(size=24),legend.title = element_text(face="bold",size=30), legend.position="none",legend.justification = c("right","top")) + 
  scale_color_manual(values=c('darkblue','darkred'))+ scale_shape_manual(values=c(15, 17)) +
  scale_fill_manual(name=c("Age Group","Horizon"))+labs(color="Age Group",shape="Horizon") +
  scale_x_discrete(limits=c("1","2","3","4","5","6"))

fcer6c<-subset(fcerc,fcerc$Horizon==6)
fcer6doubc<-subset(fcer6c,fcer6c$Order==1)
fcer6c<-rbind(fcer6c,fcer6doubc)

Line6<-ggplot(fcer6c, aes(x=Order, y=value, group=Group, color=Group))

q3<- p + geom_line(aes(x=fcer6c$Order, y=fcer6c$value, group=fcer6c$Group, color=fcer6c$Group),size=1.5) 

Ac1<-q3+guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))




###### Reaction Time #####
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h6_ 1','all_RT_h6_ 2','all_RT_h6_ 3','all_RT_h6_ 4','all_RT_h6_ 5','all_RT_h6_ 6','all_RT_h6_ 7','all_RT_h6_ 8','all_RT_h6_ 9','all_RT_h6_10'))
#RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h6_ 5','all_RT_h6_ 6','all_RT_h6_ 7','all_RT_h6_ 8','all_RT_h6_ 9','all_RT_h6_10'))
#RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h1_5','all_RT_h6_ 5','all_RT_h6_ 6','all_RT_h6_ 7','all_RT_h6_ 8','all_RT_h6_ 9','all_RT_h6_10'))


ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
# + Error(factor(subjectID))
print(summary(ANOVA))
#TukeyHSD(ANOVA)

RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h6_ 5','all_RT_h1_ 5'))

ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
# + Error(factor(subjectID))
print(summary(ANOVA))
#TukeyHSD(ANOVA)





#OldRT1<-subset(RepeatYoungold_long,Condition=="old")
#YoungRT1<-subset(RepeatYoungold_long,Condition=="young")


#describe(OldRT1$value)
#describe(YoungRT1$value)





Order<-sort(rep(c(1:10),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
rt2<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.6) + geom_point(size=5, shape=17) +ggtitle("RT, horizon 6") + ylab("Reaction Time(in secs)") +
  theme_classic() + scale_color_manual(values=c('darkblue','darkred')) + scale_x_discrete(name ="Trial", limits=c("1","2","3","4","5","6","7","8","9","10"))+
  theme(plot.title = element_text(hjust=0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24), legend.title = element_blank(),legend.position = "none") +ylim(0,3)+ylab(NULL)


###
###### Reaction Time ##### Horizon 1
RepeatYoungold_long<-melt(data, id.vars= c("subjectID", "Condition"),measure.vars=c('all_RT_h1_ 1','all_RT_h1_ 2','all_RT_h1_ 3','all_RT_h1_ 4','all_RT_h1_ 5'))
ANOVA<-aov(data=RepeatYoungold_long,value~variable*Condition)
print(summary(ANOVA))
TukeyHSD(ANOVA)



#OldRT1<-subset(RepeatYoungold_long,Condition=="old")
#YoungRT1<-subset(RepeatYoungold_long,Condition=="young")


#describe(OldRT1$value)
#describe(YoungRT1$value)





Order<-sort(rep(c(1:5),nrow(data)))
RepeatYoungold_long<-cbind(RepeatYoungold_long,Order)

ReaPer <- summarySE(RepeatYoungold_long, measurevar="value", groupvars=c("Order","Condition"))
head(ReaPer)

Line<-ggplot(ReaPer, aes(x=Order, y=value, group=Condition, color=Condition))
rt1<-Line + geom_errorbar(aes(ymin=value -se, ymax=value+se, width=.2))+
  geom_line(size=1.6) + geom_point(size=5,shape=15) + ggtitle("RT, horizon 1") + ylab("Reaction Time(in secs)") +
  theme_classic() + scale_color_manual(values=c('darkblue','darkred')) + scale_x_discrete(name ="Trial", limits=c("1","2","3","4","5"))+
  theme(plot.title = element_text(hjust=0.5,color = "black", size = 40, face = "bold"),axis.title =element_text(color = "black", size = 36, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x  = element_text(size=24),axis.text.y = element_text(size=24),legend.title = element_blank(),legend.position="none")+ylim(0,3)




figure2 <- multi_panel_figure(width=270,height=360,columns = 5, rows = 2,figure_name="Figure 4. Overall Performance")

figure2 %<>% 
  fill_panel(Ac1,column=1:5,row=1) %>%
  fill_panel(rt1,column=1:2,row=2) %>%
  fill_panel(rt2, column = 3:5, row=2)
figure2




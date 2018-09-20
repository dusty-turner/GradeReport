library(gridExtra) 
library(grid)
library(gridBase)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

df <- expand.grid(x = 1:3, y = 3:1)
df %>% rowwise() %>% do(i = seq(.$x, .$y))

getwd()
# setwd("T:/MA104 CD Stuff/AY18/18-02/Assessments/gradeReport_R")

# gradeBooktest<- read_csv("T:/MA104 CD Stuff/AY18/18-01/Assessments/GradeReport/gradeBookTest.csv")

gradeBooktest = read.csv("gradeBookCSV_WPR3.csv", stringsAsFactors = F)

class(gradeBooktest)
admindata = gradeBooktest[-c(1:3),c(1:10)]
names(admindata) = as.character(unlist(gradeBooktest[3,c(1:10)]))

data = gradeBooktest[-(1:3),-c(1:10)]
names(data) = c(as.character(unlist(gradeBooktest[2,c(11:34)])), 
                    as.character(unlist(gradeBooktest[1,-c(1:34)])))

datatots = cbind(admindata,data)

#string of max points for each assignment
maxpts=gradeBooktest[3,]
maxpts[,c(1:10)]=0
maxpts[,-c(1:10)]=as.numeric(as.character(maxpts[,-c(1:10)]))


#export organized data
write.csv(datatots, "cleaneddata104.csv") #consider making this user defined in app

#read in data so column names are updated
cleaned104 = read.csv("cleaneddata104.csv")
cleaned104=cleaned104[,-1] #removes leading index column
names(maxpts)=names(cleaned104)

#Add MA103 FCE1 Data
MA103=read.csv("MA103FCEscores.csv")
cleaned104$FCE=MA103$FCE
cleaned104$MA103Final=MA103$MA103Grade
cleaned104$MA103Type=MA103$TypeMA103

#Add Instructor Names Data
instructor=read.csv("MA104SectionInstructors.csv")
cleaned104$Instructor=instructor$Instructor

#define report settings
#cutline<-c(0,.5,.6,0.65, 0.70, 0.73, 0.77, 0.80, 0.83, 0.87, 0.90, 0.93, 0.97, 1.0) #letter grade break points
cutline<-c(0,.2,.4,.5,.6,0.65, 0.70, 0.73, 0.77, 0.80, 0.83, 0.87, 0.90, 0.93, 0.97, 1.0) #letter grade break points
#histogram colours
#histcols=c(rep("darkred",4), rep("darkorange3"),rep("darkorchid3",3),rep("dodgerblue3",3),rep("darkgreen",3))
histcols=c(rep("darkred",5), rep("darkorange3"),rep("darkorchid3",3),rep("dodgerblue3",3),rep("darkgreen",3))
#Question names
questNames=c("Question 1", "Question 2","Question 3","Question 4");

#Number of students in the course
maxStudents=length(cleaned104$Name)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#analyze WPR1
numquestions=4 #number of questions on WPR <user defined>
colstartWPR1= which(names(cleaned104)=="WPR1") #column number Q1 starts on
cleaned104$WPR1.Total=apply(cleaned104[,c(colstartWPR1:(colstartWPR1+numquestions-1))],1, sum) #collapse grades into WPR totals
maxpts$WPR1.Total=sum(maxpts[,c(colstartWPR1:(colstartWPR1+numquestions-1))]) #max points for assessment 


#Boxplot of performance by question
    #WPR1
hugedf.1.1 = data.frame(Value = cleaned104$WPR1/maxpts$WPR1, Variable = rep("Cumulative",maxStudents),WPR = rep("WPR1",maxStudents) )
hugedf.1.2 = data.frame(Value = cleaned104$WPR1.1/maxpts$WPR1.1, Variable = rep("Conceptual",maxStudents),WPR = rep("WPR1",maxStudents) )
hugedf.1.3 = data.frame(Value = cleaned104$WPR1.2/maxpts$WPR1.2, Variable = rep("Graphical",maxStudents),WPR = rep("WPR1",maxStudents) )
hugedf.1.4 = data.frame(Value = cleaned104$WPR1.3/maxpts$WPR1.3, Variable = rep("Application",maxStudents),WPR = rep("WPR1",maxStudents) )
hugedf.1.5 = data.frame(Value = cleaned104$WPR1.4/maxpts$WPR1.4, Variable = rep("Corrected",maxStudents),WPR = rep("WPR1",maxStudents) )
hugedf.1= rbind(hugedf.1.1, hugedf.1.2,hugedf.1.3,hugedf.1.4, hugedf.1.5) #Combine WPR data


summary(cleaned104$WPR1.Total)
sd(cleaned104$WPR1.Total[!is.na(cleaned104$WPR1.Total)]) #ignores NA fields

# develop summary table
frame()
summaryTable.WPR1=data.frame(Mean=mean(cleaned104$WPR1.Total/maxpts$WPR1.Total, na.rm=T), StndDev=sd(cleaned104$WPR1.Total, na.rm=T), 
                             Max= max(cleaned104$WPR1.Total/maxpts$WPR1.Total, na.rm=T), 
                             Min=min(cleaned104$WPR1.Total/maxpts$WPR1.Total, na.rm=T), 
                             Median=median((cleaned104$WPR1.Total)/(maxpts$WPR1.Total), na.rm=T),
                             Failures=sum((cleaned104$WPR1.Total/maxpts$WPR1.Total)<0.65, na.rm=T)
)
summaryTable.WPR1=round(summaryTable.WPR1,  digits=3)
frame()
grid.table(summaryTable.WPR1)

#Histogram of WPR 1 scores
Hist.WPR1= ggplot(data=cleaned104, aes((cleaned104$WPR1.Total)/(maxpts$WPR1.Total)))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR1: Distribution of Grades", x="Score") + ylim(c(0,150))+ xlim(c(0.5,1))
Hist.WPR1

#Histogram of WPR 1 scores +50 pts
Hist.WPR1.50= ggplot(data=cleaned104, aes((cleaned104$WPR1.Total+50)/(maxpts$WPR1.Total+50)))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR1: Projected Grades after 50pt Revisions", x="Score") + ylim(c(0,150))+ xlim(c(0.5,1))
Hist.WPR1.50

#Histogram of WPR 1 scores Revisions
Hist.WPR1.Final= ggplot(data=cleaned104, aes((cleaned104$WPR1.Total+cleaned104[,colstartWPR1+4])/(maxpts$WPR1.Total+maxpts[,colstartWPR1+4])))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR1: Grades after 50pt Revisions", x="Score") + ylim(c(0,150))+ xlim(c(0.5,1))
Hist.WPR1.Final

# develop summary table with 50 pt revision
frame()
summaryTable.WPR1.50=data.frame(Mean=mean((cleaned104$WPR1.Total+50)/(maxpts$WPR1.Total+50), na.rm=T), StndDev=sd(cleaned104$WPR1.Total+50, na.rm=T), 
                             Max= max((cleaned104$WPR1.Total+50)/(maxpts$WPR1.Total+50), na.rm=T), 
                             Min=min((cleaned104$WPR1.Total+50)/(maxpts$WPR1.Total+50), na.rm=T), 
                             Median=median((cleaned104$WPR1.Total+50)/(maxpts$WPR1.Total+50), na.rm=T),
                             Failures=sum((cleaned104$WPR1.Total+50)/(maxpts$WPR1.Total+50)<0.65, na.rm=T)
)
summaryTable.WPR1.50=round(summaryTable.WPR1.50,  digits=3)
frame()
grid.table(summaryTable.WPR1.50)

# develop summary table with actual revisions
frame()
summaryTable.WPR1.Final=data.frame(Mean=mean((cleaned104$WPR1.Total+cleaned104[,colstartWPR1+4])/(maxpts$WPR1.Total+maxpts[,colstartWPR1+4]), na.rm=T), 
                                StndDev=sd(cleaned104$WPR1.Total+cleaned104[,colstartWPR1+4], na.rm=T), 
                                Max= max((cleaned104$WPR1.Total+cleaned104[,colstartWPR1+4])/(maxpts$WPR1.Total+maxpts[,colstartWPR1+4]), na.rm=T), 
                                Min=min((cleaned104$WPR1.Total+cleaned104[,colstartWPR1+4])/(maxpts$WPR1.Total+maxpts[,colstartWPR1+4]), na.rm=T), 
                                Median=median((cleaned104$WPR1.Total+cleaned104[,colstartWPR1+4])/(maxpts$WPR1.Total+maxpts[,colstartWPR1+4]), na.rm=T),
                                Failures=sum((cleaned104$WPR1.Total+cleaned104[,colstartWPR1+4])/(maxpts$WPR1.Total+maxpts[,colstartWPR1+4])<0.65, na.rm=T)
)
summaryTable.WPR1.Final=round(summaryTable.WPR1.Final,  digits=3)
frame()
grid.table(summaryTable.WPR1.Final)


#Plot it
BP1.WPR1=ggplot(data=hugedf.1, aes(x=Variable, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR1: Distribution of Grades by Question", x="Question Type", y="Score(%)")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed")
BP1.WPR1

#Compare it to MA103 FCE Data and scatter plot
SP1.WPR1.FCE=ggplot(data=cleaned104, aes(y=cleaned104$FCE, x=(cleaned104$WPR1.Total)/(maxpts$WPR1.Total)))+
  geom_point(color="blue") +labs(title="Exam Performance on MA104 WPR1 Compared to MA103 FCE1", y="FCE1 Score", x="WPR1 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0.79, slope=0, linetype="dashed", color="red")
SP1.WPR1.FCE

max(cleaned104$WPR1.2,na.rm =T)

#------------------------------------------------------------------------------------------------------------------------

BP1.WPR1.Q3.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=cleaned104$WPR1.2/maxpts$WPR1.2)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR1: Q3 Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(cleaned104$WPR1.2/maxpts$WPR1.2, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR1.2/maxpts$WPR1.2,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR1.2/maxpts$WPR1.2,.75, na.rm=T), color="blue", linetype="dashed")
BP1.WPR1.Q3.Inst

BP1.WPR1.Q4.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=cleaned104$WPR1.3/maxpts$WPR1.3)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR1: Q4 Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(cleaned104$WPR1.3/maxpts$WPR1.3, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR1.3/maxpts$WPR1.3,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR1.3/maxpts$WPR1.3,.75, na.rm=T), color="blue", linetype="dashed")
BP1.WPR1.Q4.Inst



#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#PSL 1 Analysis
hugedf.PSL1 = data.frame(Value = (cleaned104$PSL1+cleaned104$PSL1.1)/(maxpts$PSL1+maxpts$PSL1.1), Variable = rep("PSL",maxStudents),WPR = rep("PSL1",maxStudents) )

Hist.PSL1= ggplot(data=cleaned104, aes(cleaned104$PSL1/maxpts$PSL1))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 PSL1: Distribution of Grades", x="Score") + ylim(c(0,165))+ xlim(c(0.5,1))
Hist.PSL1

# develop summary table
summaryTable.PSL1=data.frame(Mean=mean((hugedf.PSL1$Value), na.rm=T), StndDev=sd(hugedf.PSL1$Value, na.rm=T), 
                             Max= max(hugedf.PSL1$Value, na.rm=T), 
                             Min=min((hugedf.PSL1[hugedf.PSL1$Value>0,1]), na.rm=T), 
                             Median=median((hugedf.PSL1$Value), na.rm=T),
                             Failures=sum((hugedf.PSL1$Value)<0.65, na.rm=T)
)
summaryTable.PSL1=round(summaryTable.PSL1,  digits=3)
frame()
grid.table(summaryTable.PSL1)


#Grading by Instructor
BP1.PSL1.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=hugedf.PSL1$Value)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 PSL1: Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(hugedf.PSL1$Value, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(hugedf.PSL1$Value,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(hugedf.PSL1$Value,.75, na.rm=T), color="blue", linetype="dashed")
BP1.PSL1.Inst



#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#analyze WPR2
#Update both FCE names and Instructor-Section CSV files to capture students who dropped the course


numquestions=4 #number of questions on WPR <user defined>, DOn't include Revisions (they are added later)
colstartWPR2= which(names(cleaned104)=="WPR2") #column number Q1 starts on
cleaned104$WPR2.Total=apply(cleaned104[,c(colstartWPR2:(colstartWPR2+numquestions-1))],1, sum) #collapse grades into WPR totals
maxpts$WPR2.Total=sum(maxpts[,c(colstartWPR2:(colstartWPR2+numquestions-1))]) #max points for assessment 


#Boxplot of performance by question
#WPR2
hugedf.2.1 = data.frame(Value = cleaned104$WPR2/maxpts$WPR2, Variable = rep("Cumulative",maxStudents),WPR = rep("WPR2",maxStudents) )
hugedf.2.2 = data.frame(Value = cleaned104$WPR2.1/maxpts$WPR2.1, Variable = rep("Graphical",maxStudents),WPR = rep("WPR2",maxStudents) )
hugedf.2.3 = data.frame(Value = cleaned104$WPR2.2/maxpts$WPR2.2, Variable = rep("Conceptual",maxStudents),WPR = rep("WPR2",maxStudents) )
hugedf.2.4 = data.frame(Value = cleaned104$WPR2.3/maxpts$WPR2.3, Variable = rep("Application",maxStudents),WPR = rep("WPR2",maxStudents) )
hugedf.2.5 = data.frame(Value = cleaned104$WPR2.4/maxpts$WPR2.4, Variable = rep("Corrected",maxStudents),WPR = rep("WPR2",maxStudents) )
hugedf.2= rbind(hugedf.2.1, hugedf.2.2,hugedf.2.3,hugedf.2.4, hugedf.2.5) #Combine WPR data


summary(cleaned104$WPR2.Total)
sd(cleaned104$WPR2.Total[!is.na(cleaned104$WPR2.Total)]) #ignores NA fields

# develop summary table
frame()
summaryTable.WPR2=data.frame(Mean=mean(cleaned104$WPR2.Total/maxpts$WPR2.Total, na.rm=T), StndDev=sd(cleaned104$WPR2.Total, na.rm=T), 
                             Max= max(cleaned104$WPR2.Total/maxpts$WPR2.Total, na.rm=T), 
                             Min=min(cleaned104$WPR2.Total/maxpts$WPR2.Total, na.rm=T), 
                             Median=median((cleaned104$WPR2.Total)/(maxpts$WPR2.Total), na.rm=T),
                             Failures=sum((cleaned104$WPR2.Total/maxpts$WPR2.Total)<0.65, na.rm=T)
)
summaryTable.WPR2=round(summaryTable.WPR2,  digits=3)
frame()
grid.table(summaryTable.WPR2)

#Histogram of WPR2 scores
Hist.WPR2= ggplot(data=cleaned104, aes((cleaned104$WPR2.Total)/(maxpts$WPR2.Total)))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR2: Distribution of Grades", x="Score") + ylim(c(0,175))+ xlim(c(0.5,1))
Hist.WPR2

#Histogram of WPR2 scores +50 pts
Hist.WPR2.50= ggplot(data=cleaned104, aes((cleaned104$WPR2.Total+50)/(maxpts$WPR2.Total+50)))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR2: Projected Grades after 50pt Revisions", x="Score") + ylim(c(0,175))+ xlim(c(0.5,1))
Hist.WPR2.50

#Histogram of WPR2 scores Revisions
Hist.WPR2.Final= ggplot(data=cleaned104, aes((cleaned104$WPR2.Total+cleaned104[,colstartWPR2+4])/(maxpts$WPR2.Total+maxpts[,colstartWPR2+4])))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR2: Grades after 50pt Revisions", x="Score") + ylim(c(0,150))+ xlim(c(0.5,1))
Hist.WPR2.Final

# develop summary table with 50 pt revision
frame()
summaryTable.WPR2.50=data.frame(Mean=mean((cleaned104$WPR2.Total+50)/(maxpts$WPR2.Total+50), na.rm=T), StndDev=sd(cleaned104$WPR2.Total+50, na.rm=T), 
                                Max= max((cleaned104$WPR2.Total+50)/(maxpts$WPR2.Total+50), na.rm=T), 
                                Min=min((cleaned104$WPR2.Total+50)/(maxpts$WPR2.Total+50), na.rm=T), 
                                Median=median((cleaned104$WPR2.Total+50)/(maxpts$WPR2.Total+50), na.rm=T),
                                Failures=sum((cleaned104$WPR2.Total+50)/(maxpts$WPR2.Total+50)<0.65, na.rm=T)
)
summaryTable.WPR2.50=round(summaryTable.WPR2.50,  digits=3)
frame()
grid.table(summaryTable.WPR2.50)

# develop summary table with actual revisions
frame()
summaryTable.WPR2.Final=data.frame(Mean=mean((cleaned104$WPR2.Total+cleaned104[,colstartWPR2+4])/(maxpts$WPR2.Total+maxpts[,colstartWPR2+4]), na.rm=T), 
                                   StndDev=sd(cleaned104$WPR2.Total+cleaned104[,colstartWPR2+4], na.rm=T), 
                                   Max= max((cleaned104$WPR2.Total+cleaned104[,colstartWPR2+4])/(maxpts$WPR2.Total+maxpts[,colstartWPR2+4]), na.rm=T), 
                                   Min=min((cleaned104$WPR2.Total+cleaned104[,colstartWPR2+4])/(maxpts$WPR2.Total+maxpts[,colstartWPR2+4]), na.rm=T), 
                                   Median=median((cleaned104$WPR2.Total+cleaned104[,colstartWPR2+4])/(maxpts$WPR2.Total+maxpts[,colstartWPR2+4]), na.rm=T),
                                   Failures=sum((cleaned104$WPR2.Total+cleaned104[,colstartWPR2+4])/(maxpts$WPR2.Total+maxpts[,colstartWPR2+4])<0.65, na.rm=T)
)
summaryTable.WPR2.Final=round(summaryTable.WPR2.Final,  digits=3)
frame()
grid.table(summaryTable.WPR2.Final)


#Plot it
BP2.WPR2=ggplot(data=hugedf.2, aes(x=Variable, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR2: Distribution of Grades by Question", x="Question Type", y="Score(%)")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed")
BP2.WPR2

#Course Totals to this point
# develop summary table
frame()
summaryTable.WPR2.CourseAvg=data.frame(Mean=mean(cleaned104$X.Pts./cleaned104$X.MaxPts., na.rm=T), StndDev=sd(cleaned104$X.Pts., na.rm=T), 
                             Max= max(cleaned104$X.Pts./cleaned104$X.MaxPts., na.rm=T), 
                             Min=min((cleaned104$X.Pts./cleaned104$X.MaxPts.)>.2, na.rm=T), 
                             Median=median((cleaned104$X.Pts./cleaned104$X.MaxPts.), na.rm=T),
                             Failures=sum((cleaned104$X.Pts./cleaned104$X.MaxPts.)<0.65, na.rm=T)
)
summaryTable.WPR2.CourseAvg=round(summaryTable.WPR2.CourseAvg,  digits=3)
frame()
grid.table(summaryTable.WPR2.CourseAvg)


#Histogram of Course scores
Hist.WPR2.CourseAvg= ggplot(data=cleaned104, aes((cleaned104$X.Pts.)/(cleaned104$X.MaxPts.)))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 Distribution of Grades", x="Score") + ylim(c(0,180))+ xlim(c(0.5,1))
Hist.WPR2.CourseAvg

#BoxPlot of major graded events
WPR2.CourseEvents=rbind(hugedf.1, hugedf.PSL.1, hugedf.2)
BP2.WPR2.CourseEvents=ggplot(data=WPR2.CourseEvents, aes(x=WPR, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104: Distribution of Grades by Assessment", x="Assessment", y="Score(%)", fill="Assessment")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed")
BP2.WPR2.CourseEvents


#Compare it to MA103 FCE Data and scatter plot
SP.WPR2.FCE=ggplot(data=cleaned104, aes(y=cleaned104$FCE, x=(cleaned104$WPR2.Total)/(maxpts$WPR2.Total)))+
  geom_point(color="blue") +labs(title="Exam Performance on MA104 WPR2 Compared to MA103 FCE1", y="FCE1 Score", x="WPR2 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0.79, slope=0, linetype="dashed", color="red")+ geom_abline(intercept=0, slope=1, linetype="dashed", color="green")
SP.WPR2.FCE

#scatter plot comparing it to WPR1 
SP2.WPR2.LastWPR=ggplot(data=cleaned104, aes(x=(cleaned104$WPR1.Total)/(maxpts$WPR1.Total), y=(cleaned104$WPR2.Total)/(maxpts$WPR2.Total)))+
  geom_point(color="blue") +labs(title="Exam Performance on MA104 WPR2 Compared to WPR1", x="WPR1 Score", y="WPR2 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
SP2.WPR2.LastWPR


#------------------------------------------------------------------------------------------------------------------------

BP2.WPR2.Q3.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=cleaned104$WPR2.2/maxpts$WPR2.2)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR2: Q3 Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(cleaned104$WPR2.2/maxpts$WPR2.2, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR2.2/maxpts$WPR2.2,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR2.2/maxpts$WPR2.2,.75, na.rm=T), color="blue", linetype="dashed")
BP2.WPR2.Q3.Inst

BP2.WPR2.Q4.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=cleaned104$WPR2.3/maxpts$WPR2.3)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR2: Q4 Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(cleaned104$WPR2.3/maxpts$WPR2.3, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR2.3/maxpts$WPR2.3,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR2.3/maxpts$WPR2.3,.75, na.rm=T), color="blue", linetype="dashed")
BP2.WPR2.Q4.Inst

#------------------------------------------------------------------------------------------------------------------------
#MA103 Performance

#BoxPlot of current course grade by MA103 Section type

MA103.BP2.WPR2.CourseEvents=ggplot(data=cleaned104, aes(x=MA103Type, y=cleaned104$X.Pts./cleaned104$X.MaxPts., fill=MA103Type)) + 
  geom_violin() + #facet_wrap(~Variable) + 
  scale_x_discrete( limits = rev(levels(cleaned104$MA103Type))) +
  scale_fill_brewer(palette="Dark2") +labs(title="MA104: Distribution of Grades by MA103 Section Type", x="Section Type", y="Score(%)", fill="Section Type")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed")
MA103.BP2.WPR2.CourseEvents

#BoxPlot of PSL1 grade by MA103 Section type

MA103.BP2.PSL1=ggplot(data=cleaned104, aes(x=MA103Type, y=(PSL1+PSL1.1)/(maxpts$PSL1+maxpts$PSL1.1), fill=MA103Type)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_x_discrete( limits = rev(levels(cleaned104$MA103Type))) +
  scale_fill_brewer(palette="Dark2") +labs(title="MA104: Final PSL1 Grades by MA103 Section Type", x="Section Type", y="Score(%)", fill="Section Type")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed")
MA103.BP2.PSL1


#Compare WPR2 to MA103 Final Grade and scatter plot
MA103Final.SP.WPR2=ggplot(data=cleaned104, aes(x=cleaned104$MA103Final, y=(cleaned104$WPR2.Total)/(maxpts$WPR2.Total), color=MA103Type))+
  geom_point() + scale_color_brewer(palette = "Set1")+
  labs(title="Exam Performance on MA104 WPR2 Compared to MA103 Final Grade", x="MA103 Final Score", y="WPR2 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0.65, slope=1, linetype="dashed", color="red")+ geom_abline(intercept=0, slope=1, linetype="dashed", color="green")
MA103Final.SP.WPR2



#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#PSL 2 Analysis
hugedf.PSL2 = data.frame(Value = (cleaned104$PSL2+cleaned104$PSL2.1)/(maxpts$PSL2+maxpts$PSL2.1), Variable = rep("PSL",maxStudents),WPR = rep("PSL2",maxStudents) )

Hist.PSL2= ggplot(data=cleaned104, aes(cleaned104$PSL2/maxpts$PSL2))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 PSL2: Distribution of Grades", x="Score") + ylim(c(0,165))+ xlim(c(0.5,1))
Hist.PSL2

# develop summary table
summaryTable.PSL2=data.frame(Mean=mean((hugedf.PSL2$Value), na.rm=T), StndDev=sd(hugedf.PSL2$Value, na.rm=T), 
                             Max= max(hugedf.PSL2$Value, na.rm=T), 
                             Min=min((hugedf.PSL2[hugedf.PSL2$Value>0,1]), na.rm=T), 
                             Median=median((hugedf.PSL2$Value), na.rm=T),
                             Failures=sum((hugedf.PSL2$Value)<0.65, na.rm=T)
)
summaryTable.PSL2=round(summaryTable.PSL2,  digits=3)
frame()
grid.table(summaryTable.PSL2)


#Grading by Instructor
BP1.PSL2.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=hugedf.PSL2$Value)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 PSL2: Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(hugedf.PSL2$Value, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(hugedf.PSL2$Value,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(hugedf.PSL2$Value,.75, na.rm=T), color="blue", linetype="dashed")
BP1.PSL2.Inst

#------------------------------------------------------------------------------------------------------------------------
#PSL 3 Analysis
hugedf.PSL3 = data.frame(Value = (cleaned104$PSL3)/(maxpts$PSL3), Variable = rep("PSL",maxStudents),WPR = rep("PSL3",maxStudents) )

Hist.PSL3= ggplot(data=cleaned104, aes(cleaned104$PSL3/maxpts$PSL3))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 PSL3: Distribution of Grades", x="Score") + ylim(c(0,165))+ xlim(c(0.5,1))
Hist.PSL3

# develop summary table
summaryTable.PSL3=data.frame(Mean=mean((hugedf.PSL3$Value), na.rm=T), StndDev=sd(hugedf.PSL3$Value, na.rm=T), 
                             Max= max(hugedf.PSL3$Value, na.rm=T), 
                             Min=min((hugedf.PSL3[hugedf.PSL3$Value>0,1]), na.rm=T), 
                             Median=median((hugedf.PSL3$Value), na.rm=T),
                             Failures=sum((hugedf.PSL3$Value)<0.65, na.rm=T)
)
summaryTable.PSL3=round(summaryTable.PSL3,  digits=3)
frame()
grid.table(summaryTable.PSL3)


#Grading by Instructor
BP1.PSL3.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=hugedf.PSL3$Value)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 PSL3: Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(hugedf.PSL3$Value, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(hugedf.PSL3$Value,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(hugedf.PSL3$Value,.75, na.rm=T), color="blue", linetype="dashed")
BP1.PSL3.Inst


#------------------------------------------------------------------------------------------------------------------------

#analyze WPR3


#Update both FCE names and Instructor-Section CSV files to capture students who dropped the course


numquestions=4 #number of questions on WPR <user defined>
colstartWPR3= which(names(cleaned104)=="WPR3") #column number Q1 starts on
cleaned104$WPR3.Total=apply(cleaned104[,c(colstartWPR3:(colstartWPR3+numquestions-1))],1, sum) #collapse grades into WPR totals
maxpts$WPR3.Total=sum(maxpts[,c(colstartWPR3:(colstartWPR3+numquestions-1))]) #max points for assessment 


#Boxplot of performance by question
#WPR3
hugedf.3.1 = data.frame(Value = cleaned104$WPR3/maxpts$WPR3, Variable = rep("Conceptual",maxStudents),WPR = rep("WPR3",maxStudents) )
hugedf.3.2 = data.frame(Value = cleaned104$WPR3.1/maxpts$WPR3.1, Variable = rep("Graphical",maxStudents),WPR = rep("WPR3",maxStudents) )
hugedf.3.3 = data.frame(Value = cleaned104$WPR3.2/maxpts$WPR3.2, Variable = rep("Cumulative",maxStudents),WPR = rep("WPR3",maxStudents) )
hugedf.3.4 = data.frame(Value = cleaned104$WPR3.3/maxpts$WPR3.3, Variable = rep("Application",maxStudents),WPR = rep("WPR3",maxStudents) )
#hugedf.3.5 = data.frame(Value = cleaned104$WPR3.4/maxpts$WPR3.4, Variable = rep("Corrected",maxStudents),WPR = rep("WPR3",maxStudents) )
hugedf.3= rbind(hugedf.3.1, hugedf.3.2,hugedf.3.3,hugedf.3.4)#, hugedf.3.5) #Combine WPR data


summary(cleaned104$WPR3.Total)
sd(cleaned104$WPR3.Total[!is.na(cleaned104$WPR3.Total)]) #ignores NA fields

# develop summary table
frame()
summaryTable.WPR3=data.frame(Mean=mean(cleaned104$WPR3.Total/maxpts$WPR3.Total, na.rm=T), StndDev=sd(cleaned104$WPR3.Total, na.rm=T), 
                             Max= max(cleaned104$WPR3.Total/maxpts$WPR3.Total, na.rm=T), 
                             Min=min(cleaned104$WPR3.Total/maxpts$WPR3.Total, na.rm=T), 
                             Median=median((cleaned104$WPR3.Total)/(maxpts$WPR3.Total), na.rm=T),
                             Failures=sum((cleaned104$WPR3.Total/maxpts$WPR3.Total)<0.65, na.rm=T)
)
summaryTable.WPR3=round(summaryTable.WPR3,  digits=3)
frame()
grid.table(summaryTable.WPR3)

#Histogram of WPR3 scores
Hist.WPR3= ggplot(data=cleaned104, aes((cleaned104$WPR3.Total)/(maxpts$WPR3.Total)))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR3: Distribution of Grades", x="Score") + ylim(c(0,175))+ xlim(c(0.5,1))
Hist.WPR3

#Histogram of WPR3 scores +50 pts
Hist.WPR3.50= ggplot(data=cleaned104, aes((cleaned104$WPR3.Total+50)/(maxpts$WPR3.Total+50)))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR3: Projected Grades after 50pt Revisions", x="Score") + ylim(c(0,175))+ xlim(c(0.5,1))
Hist.WPR3.50

#Histogram of WPR3 scores Revisions
Hist.WPR3.Final= ggplot(data=cleaned104, aes((cleaned104$WPR3.Total+cleaned104[,colstartWPR3+4])/(maxpts$WPR3.Total+maxpts[,colstartWPR3+4])))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 WPR3: Grades after 50pt Revisions", x="Score") + ylim(c(0,150))+ xlim(c(0.5,1))
Hist.WPR3.Final

# develop summary table with 50 pt revision
frame()
summaryTable.WPR3.50=data.frame(Mean=mean((cleaned104$WPR3.Total+50)/(maxpts$WPR3.Total+50), na.rm=T), StndDev=sd(cleaned104$WPR3.Total+50, na.rm=T), 
                                Max= max((cleaned104$WPR3.Total+50)/(maxpts$WPR3.Total+50), na.rm=T), 
                                Min=min((cleaned104$WPR3.Total+50)/(maxpts$WPR3.Total+50), na.rm=T), 
                                Median=median((cleaned104$WPR3.Total+50)/(maxpts$WPR3.Total+50), na.rm=T),
                                Failures=sum((cleaned104$WPR3.Total+50)/(maxpts$WPR3.Total+50)<0.65, na.rm=T)
)
summaryTable.WPR3.50=round(summaryTable.WPR3.50,  digits=3)
frame()
grid.table(summaryTable.WPR3.50)

# develop summary table with actual revisions
frame()
summaryTable.WPR3.Final=data.frame(Mean=mean((cleaned104$WPR3.Total+cleaned104[,colstartWPR3+4])/(maxpts$WPR3.Total+maxpts[,colstartWPR3+4]), na.rm=T), 
                                   StndDev=sd(cleaned104$WPR3.Total+cleaned104[,colstartWPR3+4], na.rm=T), 
                                   Max= max((cleaned104$WPR3.Total+cleaned104[,colstartWPR3+4])/(maxpts$WPR3.Total+maxpts[,colstartWPR3+4]), na.rm=T), 
                                   Min=min((cleaned104$WPR3.Total+cleaned104[,colstartWPR3+4])/(maxpts$WPR3.Total+maxpts[,colstartWPR3+4]), na.rm=T), 
                                   Median=median((cleaned104$WPR3.Total+cleaned104[,colstartWPR3+4])/(maxpts$WPR3.Total+maxpts[,colstartWPR3+4]), na.rm=T),
                                   Failures=sum((cleaned104$WPR3.Total+cleaned104[,colstartWPR3+4])/(maxpts$WPR3.Total+maxpts[,colstartWPR3+4])<0.65, na.rm=T)
)
summaryTable.WPR3.Final=round(summaryTable.WPR3.Final,  digits=3)
frame()
grid.table(summaryTable.WPR3.Final)


#Plot it
BP.WPR3=ggplot(data=hugedf.3, aes(x=Variable, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR3: Distribution of Grades by Question", x="Question Type", y="Score(%)")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed")
BP.WPR3

#Course Totals to this point
# develop summary table
frame()
summaryTable.WPR3.CourseAvg=data.frame(Mean=mean(cleaned104$X.Pts./cleaned104$X.MaxPts., na.rm=T), StndDev=sd(cleaned104$X.Pts., na.rm=T), 
                                       Max= max(cleaned104$X.Pts./cleaned104$X.MaxPts., na.rm=T), 
                                       Min=min((cleaned104$X.Pts./cleaned104$X.MaxPts.)>.2, na.rm=T), 
                                       Median=median((cleaned104$X.Pts./cleaned104$X.MaxPts.), na.rm=T),
                                       Failures=sum((cleaned104$X.Pts./cleaned104$X.MaxPts.)<0.65, na.rm=T)
)
summaryTable.WPR3.CourseAvg=round(summaryTable.WPR3.CourseAvg,  digits=3)
frame()
grid.table(summaryTable.WPR3.CourseAvg)


#Histogram of Course scores
Hist.WPR3.CourseAvg= ggplot(data=cleaned104, aes((cleaned104$X.Pts.)/(cleaned104$X.MaxPts.)))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 Distribution of Grades", x="Score") + ylim(c(0,180))+ xlim(c(0.5,1))
Hist.WPR3.CourseAvg


#BoxPlot of WPRs by Question Type
WPR3.WPRbyQuestion=rbind(hugedf.1, hugedf.2, hugedf.3)
BP.WPR3.WPRbyQuestion=ggplot(data=WPR3.WPRbyQuestion, aes(x=Variable, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104: Distribution of Grades by Question Type", x="Assessment", y="Score(%)", fill="Assessment")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed") +
  geom_hline(yintercept=mean(WPR3.WPRbyQuestion$Value, na.rm = T), color="blue", linetype="dashed")
BP.WPR3.WPRbyQuestion


#BP of only inclass events (WPR totals, no revisions)
TotalWPRAvg.WPR1= data.frame(Value = cleaned104$WPR1.Total/maxpts$WPR1.Total, WPR= rep("WPR1", maxStudents))
TotalWPRAvg.WPR2= data.frame(Value = cleaned104$WPR2.Total/maxpts$WPR2.Total, WPR= rep("WPR2", maxStudents))
TotalWPRAvg.WPR3= data.frame(Value = cleaned104$WPR3.Total/maxpts$WPR3.Total, WPR= rep("WPR3", maxStudents))
TotalWPRAvg= rbind(TotalWPRAvg.WPR1,TotalWPRAvg.WPR2,TotalWPRAvg.WPR3)

BP.WPR3.TotalWPRAvg=ggplot(data=TotalWPRAvg, aes(x=WPR, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104: Distribution of WPR Grades (no revisions)", x="Assessment", y="Score(%)", fill="Assessment")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed") +
  geom_hline(yintercept=mean(WPR3.WPRbyQuestion$Value, na.rm = T), color="blue", linetype="dashed")
BP.WPR3.TotalWPRAvg


#Histogram of Only WPR scores (no revisions)
Hist.WPR3.TotalWPRAvg= ggplot(data=cleaned104, aes((TotalWPRAvg.WPR1$Value+TotalWPRAvg.WPR2$Value+TotalWPRAvg.WPR3$Value)/3))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 Distribution of Overall WPR Performance (no revisions)", x="Score") + ylim(c(0,150))+ xlim(c(0.4,1))
Hist.WPR3.TotalWPRAvg

#Combo of WPR only and course average (see how we float them)
Hist.WPR3.Overall= ggplot()+
  geom_density(data=cleaned104, aes(x=(cleaned104$X.Pts.)/(cleaned104$X.MaxPts.)), 
                 col="black", fill="blue",alpha=.3)+ 
  geom_histogram(data=cleaned104, aes(x=(TotalWPRAvg.WPR1$Value+TotalWPRAvg.WPR2$Value+TotalWPRAvg.WPR3$Value)/3, y=..density..), 
                 breaks=cutline, col="darkblue", fill=histcols, alpha=.8)+  
  labs(title="MA104 Distribution of In-Class Assessments Relative to Course Average", x="Score") + ylim(c(0,5))+ xlim(c(0.4,1))
Hist.WPR3.Overall


#Total wpr grades with revisions
TotalWPRAvg.WPR1plus= data.frame(Value = (cleaned104$WPR1.Total+cleaned104$WPR1.4)/(maxpts$WPR1.Total+maxpts$WPR1.4), WPR= rep("WPR1", maxStudents))
TotalWPRAvg.WPR2plus= data.frame(Value = (cleaned104$WPR2.Total+cleaned104$WPR2.4)/(maxpts$WPR2.Total+maxpts$WPR2.4), WPR= rep("WPR2", maxStudents))
TotalWPRAvg.WPR3plus= data.frame(Value = (cleaned104$WPR3.Total+cleaned104$WPR3.4)/(maxpts$WPR3.Total+maxpts$WPR3.4), WPR= rep("WPR3", maxStudents))
TotalWPRAvg.plus= rbind(TotalWPRAvg.WPR1plus,TotalWPRAvg.WPR2plus,TotalWPRAvg.WPR3plus)

#BoxPlot of major graded events
WPR3.CourseEvents=rbind(TotalWPRAvg.WPR1plus, hugedf.PSL1[,c(1,3)], hugedf.PSL2[,c(1,3)], TotalWPRAvg.WPR2plus, hugedf.PSL3[,c(1,3)], TotalWPRAvg.WPR3)
BP.WPR3.CourseEvents=ggplot(data=WPR3.CourseEvents, aes(x=WPR, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104: Distribution of Grades by Assessment", x="Assessment", y="Score(%)", fill="Assessment")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed") +
  geom_hline(yintercept=mean(cleaned104$X.Ave./100, na.rm = T), color="blue", linetype="dashed")
BP.WPR3.CourseEvents

#Compare it to MA103 FCE Data and scatter plot
SP.WPR3.FCE=ggplot(data=cleaned104, aes(y=cleaned104$FCE, x=(cleaned104$WPR3.Total)/(maxpts$WPR3.Total)))+
  geom_point(color="blue") +labs(title="Exam Performance on MA104 WPR3 Compared to MA103 FCE1", y="FCE1 Score", x="WPR3 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0.79, slope=0, linetype="dashed", color="red")+ geom_abline(intercept=0, slope=1, linetype="dashed", color="green")
SP.WPR3.FCE

#scatter plot comparing it to WPR2 (without revisions)
SP2.WPR3.LastWPR=ggplot(data=cleaned104, aes(x=(cleaned104$WPR2.Total)/(maxpts$WPR2.Total), y=(cleaned104$WPR3.Total)/(maxpts$WPR3.Total)))+
  geom_point(color="blue") +labs(title="Exam Performance on MA104 WPR3 Compared to WPR2", x="WPR2 Score", y="WPR3 Score") + ylim(c(.25,1))+ xlim(c(0.25,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
SP2.WPR3.LastWPR


#------------------------------------------------------------------------------------------------------------------------

BP2.WPR3.Q3.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=cleaned104$WPR3.2/maxpts$WPR3.2)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR3: Q3 Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(cleaned104$WPR3.2/maxpts$WPR3.2, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR3.2/maxpts$WPR3.2,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR3.2/maxpts$WPR3.2,.75, na.rm=T), color="blue", linetype="dashed")
BP2.WPR3.Q3.Inst

BP2.WPR3.Q4.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=cleaned104$WPR3.3/maxpts$WPR3.3)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR3: Q4 Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(cleaned104$WPR3.3/maxpts$WPR3.3, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR3.3/maxpts$WPR3.3,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(cleaned104$WPR3.3/maxpts$WPR3.3,.75, na.rm=T), color="blue", linetype="dashed")
BP2.WPR3.Q4.Inst

#------------------------------------------------------------------------------------------------------------------------
#MA103 Performance

#BoxPlot of current course grade by MA103 Section type

MA103.BP2.WPR3.CourseEvents=ggplot(data=cleaned104, aes(x=MA103Type, y=cleaned104$X.Pts./cleaned104$X.MaxPts., fill=MA103Type)) + 
  geom_violin() + #facet_wrap(~Variable) + 
  scale_x_discrete( limits = rev(levels(cleaned104$MA103Type))) +
  scale_fill_brewer(palette="Dark2") +labs(title="MA104: Current Course Grades by MA103 Section Type", x="Section Type", y="Score(%)", fill="Section Type")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed")
MA103.BP2.WPR3.CourseEvents

#BoxPlot of PSL1 grade by MA103 Section type

MA103.BP2.PSL1=ggplot(data=cleaned104, aes(x=MA103Type, y=(PSL1+PSL1.1)/(maxpts$PSL1+maxpts$PSL1.1), fill=MA103Type)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_x_discrete( limits = rev(levels(cleaned104$MA103Type))) +
  scale_fill_brewer(palette="Dark2") +labs(title="MA104: Final PSL1 Grades by MA103 Section Type", x="Section Type", y="Score(%)", fill="Section Type")+
  geom_hline(yintercept=0.65, color="red", linetype="dashed")
MA103.BP2.PSL1


#Compare WPR3 to MA103 Final Grade and scatter plot
MA103Final.SP.WPR3=ggplot(data=cleaned104, aes(x=cleaned104$MA103Final, y=(cleaned104$WPR3.Total)/(maxpts$WPR3.Total), color=MA103Type))+
  geom_point() + scale_color_brewer(palette = "Set1")+
  labs(title="Exam Performance on MA104 WPR3 Compared to MA103 Final Grade", x="MA103 Final Score", y="WPR3 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0.65, slope=1, linetype="dashed", color="red")+ geom_abline(intercept=0, slope=1, linetype="dashed", color="green")
MA103Final.SP.WPR3


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#PSL 4 Analysis
hugedf.PSL4 = data.frame(Value = (cleaned104$PSL4)/(maxpts$PSL4), Variable = rep("PSL",maxStudents),WPR = rep("PSL4",maxStudents) )

Hist.PSL4= ggplot(data=cleaned104, aes(cleaned104$PSL4/maxpts$PSL4))+
  geom_histogram(aes(y =..count..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ #geom_density(colour="darkred") +
  labs(title="MA104 PSL4: Distribution of Grades", x="Score") + ylim(c(0,165))+ xlim(c(0.5,1))
Hist.PSL4

# develop summary table
summaryTable.PSL4=data.frame(Mean=mean((hugedf.PSL4$Value), na.rm=T), StndDev=sd(hugedf.PSL4$Value, na.rm=T), 
                             Max= max(hugedf.PSL4$Value, na.rm=T), 
                             Min=min((hugedf.PSL4[hugedf.PSL4$Value>0,1]), na.rm=T), 
                             Median=median((hugedf.PSL4$Value), na.rm=T),
                             Failures=sum((hugedf.PSL4$Value)<0.65, na.rm=T)
)
summaryTable.PSL4=round(summaryTable.PSL4,  digits=3)
frame()
grid.table(summaryTable.PSL4)


#Grading by Instructor
BP1.PSL4.Inst=ggplot(data=cleaned104, aes(x=cleaned104$Instructor, y=hugedf.PSL4$Value)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 PSL4: Grades by Instructor", x="Instructor", y="Score(%)")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  geom_hline(yintercept=mean(hugedf.PSL4$Value, na.rm=T), color="red", linetype="dashed")+
  geom_hline(yintercept=quantile(hugedf.PSL4$Value,.25, na.rm=T), color="blue", linetype="dashed")+
  geom_hline(yintercept=quantile(hugedf.PSL4$Value,.75, na.rm=T), color="blue", linetype="dashed")
BP1.PSL4.Inst



#------------------------------------------------------------------------------------------------------------------------

#analyze WPR4
numquestions=4 #number of questions on WPR <user defined>
colstartWPR4= which(names(cleaned104)=="WPR4") #column number Q1 starts on
cleaned104$WPR4.Total=apply(cleaned104[,c(colstartWPR4:(colstartWPR4+numquestions-1))],1, sum) #collapse grades into WPR totals
maxpts$WPR4.Total=sum(maxpts[,c(colstartWPR4:(colstartWPR4+numquestions-1))]) #max points for assessment 

summary(cleaned104$WPR4.Total)
sd(cleaned104$WPR4.Total[!is.na(cleaned104$WPR4.Total)]) #ignores NA fields

Hist.WPR4= ggplot(data=cleaned104, aes(cleaned104$WPR4.Total/maxpts$WPR4.Total))+
  geom_histogram(aes(y =..density..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ geom_density(colour="darkred") +
  labs(title="MA104 WPR4: Distribution of Grades", x="Score") + ylim(c(0,8))+ xlim(c(0.5,1))
Hist.WPR4


# develop summary table
frame()
summaryTable.WPR4=data.frame(Mean=mean(cleaned104$WPR4.Total/maxpts$WPR4.Total, na.rm=T), StndDev=sd(cleaned104$WPR4.Total, na.rm=T), 
                        Max= max(cleaned104$WPR4.Total/maxpts$WPR4.Total, na.rm=T), 
                        Min=min(cleaned104$WPR4.Total/maxpts$WPR4.Total, na.rm=T), 
                        Failures=sum((cleaned104$WPR4.Total/maxpts$WPR4.Total)<0.65, na.rm=T)
)
summaryTable.WPR4=round(summaryTable.WPR4,  digits=3)
grid.table(summaryTable.WPR4)

#WPR4
hugedf.4.1 = data.frame(Value = cleaned104$WPR4/maxpts$WPR4, Variable = rep("Cumulative",maxStudents),WPR = rep("WPR4",maxStudents) )
hugedf.4.2 = data.frame(Value = cleaned104$WPR4.1/maxpts$WPR4.1, Variable = rep("Conceptual",maxStudents),WPR = rep("WPR4",maxStudents) )
hugedf.4.3 = data.frame(Value = cleaned104$WPR4.2/maxpts$WPR4.2, Variable = rep("Graphical",maxStudents),WPR = rep("WPR4",maxStudents) )
hugedf.4.4 = data.frame(Value = cleaned104$WPR4.3/maxpts$WPR4.3, Variable = rep("Application",maxStudents),WPR = rep("WPR4",maxStudents) )
hugedf.4.5 = data.frame(Value = cleaned104$WPR4.4/maxpts$WPR4.4, Variable = rep("Application",maxStudents),WPR = rep("WPR4",maxStudents) )
hugedf.4= rbind(hugedf.4.1, hugedf.4.2,hugedf.4.3,hugedf.4.4, hugedf.4.5) #Combine WPR data

#Plot it
BP1.WPR4=ggplot(data=hugedf.4, aes(x=Variable, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 WPR4: Distribution of Grades by Question", x="Question Type", y="Score(%)")

BP1.WPR4+geom_hline(yintercept=0.65, color="red", linetype="dashed")

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------


#analyze TEE
numquestions=12 #number of questions on WPR <user defined>
colstartTEE= which(names(cleaned104)=="TEE") #column number Q1 starts on
cleaned104$TEE.Total=apply(cleaned104[,c(colstartTEE:(colstartTEE+numquestions-1))],1, sum) #collapse grades into WPR totals
maxpts$TEE.Total=sum(maxpts[,c(colstartTEE:(colstartTEE+numquestions-1))]) #max points for assessment 

summary(cleaned104$TEE.Total)
sd(cleaned104$TEE.Total[!is.na(cleaned104$TEE.Total)]) #ignores NA fields

Hist.TEE= ggplot(data=cleaned104, aes(cleaned104$TEE.Total/maxpts$TEE.Total))+
  geom_histogram(aes(y =..density..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ geom_density(colour="darkred") +
  labs(title="MA104 TEE: Distribution of Grades", x="Score") + ylim(c(0,6))+ xlim(c(0.5,1))
Hist.TEE


# develop summary table
frame()
summaryTable=data.frame(Mean=mean(cleaned104$TEE.Total/maxpts$TEE.Total, na.rm=T), StndDev=sd(cleaned104$TEE.Total/maxpts$TEE.Total, na.rm=T), 
                        Max= max(cleaned104$TEE.Total/maxpts$TEE.Total, na.rm=T), 
                        Min=min(cleaned104$TEE.Total/maxpts$TEE.Total, na.rm=T), 
                        Failures=sum((cleaned104$TEE.Total/maxpts$TEE.Total)<0.65, na.rm=T)
)
summaryTable=round(summaryTable,  digits=3)
grid.table(summaryTable)

#TEE
hugedf.TEE.1 = data.frame(Value = cleaned104$TEE/maxpts$TEE, Variable = rep("Conceptual",60),WPR = rep("TEE",60) )
hugedf.TEE.2 = data.frame(Value = cleaned104$TEE.1/maxpts$TEE.1, Variable = rep("Conceptual",60),WPR = rep("TEE",60) )
hugedf.TEE.3 = data.frame(Value = cleaned104$TEE.2/maxpts$TEE.2, Variable = rep("Conceptual",60),WPR = rep("TEE",60) )
hugedf.TEE.4 = data.frame(Value = cleaned104$TEE.3/maxpts$TEE.3, Variable = rep("Conceptual",60),WPR = rep("TEE",60) )
hugedf.TEE.5 = data.frame(Value = cleaned104$TEE.4/maxpts$TEE.4, Variable = rep("Application",60),WPR = rep("TEE",60) )
hugedf.TEE.6 = data.frame(Value = cleaned104$TEE.5/maxpts$TEE.5, Variable = rep("Application",60),WPR = rep("TEE",60) )
hugedf.TEE.7 = data.frame(Value = cleaned104$TEE.6/maxpts$TEE.6, Variable = rep("Application",60),WPR = rep("TEE",60) )
hugedf.TEE.8 = data.frame(Value = cleaned104$TEE.7/maxpts$TEE.7, Variable = rep("Graphical",60),WPR = rep("TEE",60) )
hugedf.TEE.9 = data.frame(Value = cleaned104$TEE.8/maxpts$TEE.8, Variable = rep("Graphical",60),WPR = rep("TEE",60) )
hugedf.TEE.10 = data.frame(Value = cleaned104$TEE.9/maxpts$TEE.9, Variable = rep("Graphical",60),WPR = rep("TEE",60) )
hugedf.TEE.11 = data.frame(Value = cleaned104$TEE.10/maxpts$TEE.10, Variable = rep("Application",60),WPR = rep("TEE",60) )
hugedf.TEE.12 = data.frame(Value = cleaned104$TEE.11/maxpts$TEE.11, Variable = rep("Conceptual",60),WPR = rep("TEE",60) )
hugedf.TEE= rbind(hugedf.TEE.1, hugedf.TEE.2,hugedf.TEE.3,hugedf.TEE.4,hugedf.TEE.5,hugedf.TEE.6,hugedf.TEE.7,hugedf.TEE.8,
                  hugedf.TEE.9,hugedf.TEE.10,hugedf.TEE.11,hugedf.TEE.12) #Combine TEE data


#Plot it
BP1.TEE=ggplot(data=hugedf.TEE, aes(x=Variable, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="MA104 TEE: Distribution of Grades by Question", x="Question Type", y="Score(%)")

BP1.TEE+geom_hline(yintercept=0.65, color="red", linetype="dashed")


#plot of performance by math source
origin.TEE=data.frame(value=cleaned104$TEE.Total/maxpts$TEE.Total, source=origins$Background, WPR="TEE")
summary(origin.TEE)

BP.TEE=ggplot(data=origin.TEE, aes(x=source, y=value, fill=source)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="TEE Performance by Path to MA104", x="Math Path", y="Score(%)")

BP.TEE+geom_hline(yintercept=0.65, color="red", linetype="dashed")+guides(fill=FALSE)

# develop summary table by path to MA104
TEE.MA100=origin.TEE[which(origin.TEE[,2]=='MA100'),1];
TEE.Tbl.MA100=round(data.frame(Mean=mean(TEE.MA100, na.rm=T),StndDev=sd(TEE.MA100, na.rm=T), 
                                Max= max(TEE.MA100, na.rm=T), 
                                Min=min(TEE.MA100, na.rm=T), 
                                Failures=sum(TEE.MA100[!is.na(TEE.MA100)]<0.65)
),digits=3)                                

TEE.MA103=origin.TEE[which(origin.TEE[,2]=='MA103'),1];
TEE.Tbl.MA103=round(data.frame(Mean=mean(TEE.MA103, na.rm = T),StndDev=sd(TEE.MA103, na.rm=T), 
                                Max= max(TEE.MA103, na.rm=T), 
                                Min=min(TEE.MA103, na.rm=T), 
                                Failures=sum(TEE.MA103[!is.na(TEE.MA103)]<0.65)
),digits=3)

TEE.MA104=origin.TEE[which(origin.TEE[,2]=='MA104'),1];
TEE.Tbl.MA104=round(data.frame(Mean=mean(TEE.MA104, na.rm=T),StndDev=sd(TEE.MA104, na.rm=T), 
                                Max= max(TEE.MA104, na.rm=T), 
                                Min=min(TEE.MA104, na.rm=T), 
                                Failures=sum(TEE.MA104[!is.na(TEE.MA104)]<0.65)
),digits=3)

TEE.MEDLVE=origin.TEE[which(origin.TEE[,2]=='MED_LVE'),1];
TEE.Tbl.MEDLVE=round(data.frame(Mean=mean(TEE.MEDLVE, na.rm=T),StndDev=sd(TEE.MEDLVE, na.rm=T), 
                                 Max= max(TEE.MEDLVE, na.rm=T), 
                                 Min=min(TEE.MEDLVE, na.rm=T), 
                                 Failures=sum(TEE.MEDLVE[!is.na(TEE.MEDLVE)]<0.65)
),digits=3)


TEE.Tbl.Path=rbind(TEE.Tbl.MA100, TEE.Tbl.MA103, TEE.Tbl.MA104)
TEE.Tbl.Path$Total=c(sum(origins$Background=="MA100"), sum(origins$Background=="MA103"), sum(origins$Background=="MA104"))
TEE.Tbl.Path$Exam=c("MA100", "MA103", "MA104")
frame()
grid.table(TEE.Tbl.Path[,1:6],rows=TEE.Tbl.Path[,7]) 

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#Multiple Box Plots grouped by question
#Combine WPR data
hugedf=rbind(hugedf.1, hugedf.2,hugedf.3,hugedf.4, hugedf.5) #combines consolidated df's from each WPR

#Plot it with TEE
multBP=ggplot(data=hugedf, aes(x=Variable, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="Exam Performance by Question Type", x="Questions", y="Score(%)")

multBP+geom_hline(yintercept=0.65, color="red", linetype="dashed")

#Multi plot by path to MA104 
origin.total=rbind(origin.WPR1,origin.WPR2,origin.WPR3,origin.WPR4,origin.WPR5, origin.TEE);
BP.origins=ggplot(data=origin.total, aes(x=source, y=value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="WPR Performance by Path to MA104", x="Math Path", y="Score(%)")

BP.origins+geom_hline(yintercept=0.65, color="red", linetype="dashed")#+guides(fill=FALSE)

#Cumulative Course Averages (percentage)
cleaned104$average = cleaned104$X.Pts./cleaned104$X.MaxPts.
Hist.CourseAvg= ggplot(data=cleaned104, aes(cleaned104$average))+
  geom_histogram(aes(y =..density..), breaks=cutline, col="darkblue", fill=histcols, alpha=.7)+ geom_density(colour="darkred") +
  labs(title="MA104: Cumulative Distribution of Grades", x="Score") + ylim(c(0,10))+ xlim(c(0.5,1))
Hist.CourseAvg


# develop summary table
frame()
summaryTableAvg=data.frame(Mean=mean(cleaned104$average, na.rm=F), StndDev=sd(cleaned104$average, na.rm=F), 
                        Max= max(cleaned104$average, na.rm=F), 
                        Min=min(cleaned104$average, na.rm=F), 
                        Failures=sum((cleaned104$average)<0.65, na.rm=F)
)
summaryTableAvg=round(summaryTableAvg,  digits=3)
grid.table(summaryTableAvg)


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#Compare WPR's to eachother
#compare WPR2 to  WPR1
WPRCompare.1.2=ggplot(data=cleaned104, aes(x=cleaned104$WPR1.Total/maxpts$WPR1.Total, y=cleaned104$WPR2.Total/maxpts$WPR2.Total))+
  geom_point(color="blue") +labs(title="Exam Performance from WPR1 to WPR2", x="WPR1 Score", y="WPR2 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.1.2

#compare WPR3 to  WPR2
WPRCompare.2.3=ggplot(data=cleaned104, aes(x=cleaned104$WPR2.Total/maxpts$WPR2.Total, y=cleaned104$WPR3.Total/maxpts$WPR3.Total))+
  geom_point(color="blue") +labs(title="Exam Performance from WPR2 to WPR3", x="WPR2 Score", y="WPR3 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.2.3

#compare WPR4 to  WPR3
WPRCompare.3.4=ggplot(data=cleaned104, aes(x=cleaned104$WPR3.Total/maxpts$WPR3.Total, y=cleaned104$WPR4.Total/maxpts$WPR4.Total))+
  geom_point(color="blue") +labs(title="Exam Performance from WPR3 to WPR4", x="WPR3 Score", y="WPR4 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.3.4

#compare WPR5 to  WPR4
WPRCompare.4.5=ggplot(data=cleaned104, aes(x=cleaned104$WPR4.Total/maxpts$WPR4.Total, y=cleaned104$WPR5.Total/maxpts$WPR5.Total))+
  geom_point(color="blue") +labs(title="Exam Performance from WPR4 to WPR5", x="WPR4 Score", y="WPR5 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.4.5

#compare TEE to  WPR5
WPRCompare.5.TEE=ggplot(data=cleaned104, aes(x=cleaned104$WPR5.Total/maxpts$WPR5.Total, y=cleaned104$TEE.Total/maxpts$TEE.Total))+
  geom_point(color="blue") +labs(title="Exam Performance from WPR5 to TEE", x="WPR5 Score", y="TEE Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.5.TEE


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#Analyze HW grades
#HW check scores
hW.cols=grep("HW", names(cleaned104))  #column names for hwck
hw.col.avail=hW.cols[!is.na(cleaned104[1,hW.cols])] #<assumes there are no blanks for the first row>
hwdata=cleaned104[,hw.col.avail] 
hwdata[is.na(hwdata)]=0  #Error check that turns blank assignments to score of 0
cleaned104$HW.avg=rowSums(hwdata)/sum(maxpts[hw.col.avail]) #current HW average



#hw quiz scores
hW.cols=grep("HWQZ", names(cleaned104))  #column names for hwck
hw.col.avail=hW.cols[!is.na(cleaned104[1,hW.cols])] #<assumes there are no blanks for the first row>
hwquiz=cleaned104[,hw.col.avail] 
hwquiz[is.na(hwquiz)]=0  #Error check that turns blank assignments to score of 0
cleaned104$HWquiz.avg=rowSums(hwquiz)/sum(maxpts[hw.col.avail]) #current HW average

#overall hw score (check +quiz)
hW.cols=grep("HW", names(cleaned104))  #column names for hw
hw.col.avail=hW.cols[!is.na(cleaned104[1,hW.cols])] #<assumes there are no blanks for the first row>
hwtotal=cleaned104[,hw.col.avail] 
hwtotal[is.na(hwtotal)]=0  #Error check that turns blank assignments to score of 0
cleaned104$HWTotal.avg=rowSums(hwtotal)/sum(maxpts[hw.col.avail[1:(length(hw.col.avail)-2)]]) #current HW average <minus hwcheck avg, hwquiz avg columns>

#compare hw quiz score to hw check
HWquiz.check=ggplot(data=cleaned104, aes(x=cleaned104$HW.avg, y=cleaned104$HWquiz.avg))+
  geom_point(color="blue") +labs(title="HW Quiz Compared to HW Check Grade", x="HW Check Score", y="HW Quiz Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
HWquiz.check

#compare WPR1 to overall hw grade
WPRCompare.hw.1=ggplot(data=cleaned104, aes(x=cleaned104$HWTotal.avg, y=cleaned104$WPR1.Total/maxpts$WPR1.Total))+
  geom_point(color="blue") +labs(title="WPR1 Performance Compared to HW Grade", x="HW Score", y="WPR1 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.hw.1

#compare WPR2 to overall hw grade
WPRCompare.hw.2=ggplot(data=cleaned104, aes(x=cleaned104$HWTotal.avg, y=cleaned104$WPR2.Total/maxpts$WPR2.Total))+
  geom_point(color="blue") +labs(title="WPR2 Performance Compared to HW Grade", x="HW Score", y="WPR2 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.hw.2

#compare WPR3 to overall hw grade
WPRCompare.hw.3=ggplot(data=cleaned104, aes(x=cleaned104$HWTotal.avg, y=cleaned104$WPR3.Total/maxpts$WPR3.Total))+
  geom_point(color="blue") +labs(title="WPR3 Performance Compared to HW Grade", x="HW Score", y="WPR3 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.hw.3

#compare WPR4 to overall hw grade
WPRCompare.hw.4=ggplot(data=cleaned104, aes(x=cleaned104$HWTotal.avg, y=cleaned104$WPR4.Total/maxpts$WPR4.Total))+
  geom_point(color="blue") +labs(title="WPR4 Performance Compared to HW Grade", x="HW Score", y="WPR4 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.hw.4

#compare WPR5 to overall hw grade
WPRCompare.hw.5=ggplot(data=cleaned104, aes(x=cleaned104$HWTotal.avg, y=cleaned104$WPR5.Total/maxpts$WPR5.Total))+
  geom_point(color="blue") +labs(title="WPR5 Performance Compared to HW Grade", x="HW Score", y="WPR5 Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.hw.5

#compare TEE to overall hw grade
WPRCompare.hw.TEE=ggplot(data=cleaned104, aes(x=cleaned104$HWTotal.avg, y=cleaned104$TEE.Total/maxpts$TEE.Total))+
  geom_point(color="blue") +labs(title="TEE Performance Compared to HW Grade", x="HW Score", y="TEE Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.hw.TEE

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#Course end analysis

#PS Labs
cleaned104$PSLtotal.1=(cleaned104$PSL1+cleaned104$PSL1.1)/(maxpts$PSL1+maxpts$PSL1.1)
cleaned104$PSLtotal.2=(cleaned104$PSL2+cleaned104$PSL2.1)/(maxpts$PSL2+maxpts$PSL2.1)
cleaned104$PSLtotal.3=(cleaned104$PSL3+cleaned104$PSL3.1)/(maxpts$PSL3+maxpts$PSL3.1)
cleaned104$PSLtotal.4=(cleaned104$PSL4+cleaned104$PSL4.1)/(maxpts$PSL4+maxpts$PSL4.1)
cleaned104$PSLtotal.5=(cleaned104$PSL5+cleaned104$PSL5.1)/(maxpts$PSL5+maxpts$PSL5.1)
PSL.cols=grep("PSLtotal", names(cleaned104))

cleaned104$PSL.avg=rowSums(cleaned104[,PSL.cols])/5
PSL1.df.total=data.frame(value =cleaned104$PSLtotal.1,Variable = rep("PSL 1",60))
PSL2.df.total=data.frame(value =cleaned104$PSLtotal.2,Variable = rep("PSL 2",60))
PSL3.df.total=data.frame(value =cleaned104$PSLtotal.3,Variable = rep("PSL 3",60))
PSL4.df.total=data.frame(value =cleaned104$PSLtotal.4,Variable = rep("PSL 4",60))
PSL5.df.total=data.frame(value =cleaned104$PSLtotal.5,Variable = rep("PSL 5",60))
PSL.df.total=rbind(PSL1.df.total,PSL2.df.total,PSL3.df.total,PSL4.df.total,PSL5.df.total)

#Plot it
BP.PSL=ggplot(data=PSL.df.total, aes(x=Variable, y=value, fill=Variable)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="PSL Performance", x="PS Lab", y="Score(%)")

BP.PSL+geom_hline(yintercept=0.65, color="red", linetype="dashed")#+guides(fill=FALSE)

#compare TEE to PSL
WPRCompare.psl.TEE=ggplot(data=cleaned104, aes(x=cleaned104$PSL.avg, y=cleaned104$TEE.Total/maxpts$TEE.Total))+
  geom_point(color="blue") +labs(title="TEE Performance Compared to PSL Grade", x="PS Lab Score", y="TEE Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.psl.TEE


#Multiple Box Plots grouped by question
#Combine WPR data with TEE
hugedf.final=rbind(hugedf.1, hugedf.2,hugedf.3,hugedf.4, hugedf.5, hugedf.TEE) #combines consolidated df's from each WPR

#Plot it with TEE
multBP.final=ggplot(data=hugedf.final, aes(x=Variable, y=Value, fill=WPR)) + 
  geom_boxplot() + #facet_wrap(~Variable) + 
  scale_fill_brewer(palette="Dark2") +labs(title="Exam Performance by Question Type", x="Questions", y="Score(%)")

multBP.final+geom_hline(yintercept=0.65, color="red", linetype="dashed")

#compare Differentiation to  Integration
WPRCompare.diff.int=ggplot(data=cleaned104, aes(x=(cleaned104$WPR1.Total+cleaned104$WPR2.Total+cleaned104$WPR3.Total)/
                                                  (maxpts$WPR1.Total+maxpts$WPR2.Total+maxpts$WPR3.Total), 
                                                y=(cleaned104$WPR4.Total+cleaned104$WPR5.Total)/
                                                  (maxpts$WPR4.Total+maxpts$WPR5.Total)))+
  geom_point(color="blue") +labs(title="WPR Performance from Differentiation to Integration", x="Differentiation Average", y="Integration Average") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.diff.int


#compare TEE application questions to  PSL
WPRCompare.psl.TEEapp=ggplot(data=cleaned104, aes(x=cleaned104$PSL.avg, 
                                                  y=(cleaned104$TEE.4+cleaned104$TEE.5+cleaned104$TEE.6+cleaned104$TEE.9+cleaned104$TEE.10)/
                                                    (maxpts$TEE.4+maxpts$TEE.5+maxpts$TEE.6+maxpts$TEE.9+maxpts$TEE.10)))+
  geom_point(color="blue") +labs(title="TEE Application Questions Compared to PSL Grade", x="PS Lab Score", y="TEE Application Question Score") + ylim(c(.5,1))+ xlim(c(0.5,1))+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")
WPRCompare.psl.TEEapp

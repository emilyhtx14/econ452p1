setwd("/Users/shreyachalla/Downloads/gss.RDS")
library(haven)
library(dplyr)
library(sjlabelled)
gss<- readRDS("gss.RDS")

library(ggplot2)
library(ggthemes)
theme_set(theme_tufte())

# Sex
smalldatashreya<-gss[,c('year','fehome','sex')]
smalldatashreya<-droplevels(smalldatashreya)
smalldatashreya<-na.omit(smalldatashreya)
ggplot(smalldatashreya,aes(x=year,y=fehome,col=sex))+ylab("% disagree")+
  geom_smooth()+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

library(stargazer)
smalldatashreyab <-  smalldatashreya[smalldatashreya$year == 1974 | smalldatashreya$year == 1998, ]
smalldatashreyab$late <- as.numeric(smalldatashreyab$year == 1998)
smalldatashreyab$Female <- as.numeric(smalldatashreyab$sex == 2)
modelshreya <- lm(fehome~Female*late,smalldatashreyab)
stargazer(modelshreya,header=FALSE,title="",type="text")

# Party ID
smalldatashreya2<-gss[gss$partyid <=2 | gss$partyid %in%c(3:6),]
smalldatashreya2<-smalldatashreya2[,c('year','fehome','partyid')]
smalldatashreya2<-droplevels(smalldatashreya2)
smalldatashreya2<-na.omit(smalldatashreya2)
smalldatashreya2$Democrat <- smalldatashreya2$partyid <=2
ggplot(smalldatashreya2,aes(x=year,y=fehome,col=Democrat))+
  geom_smooth()+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

smalldatashreya2b <-  smalldatashreya2[smalldatashreya2$year == 1974 | smalldatashreya2$year == 1998, ]
smalldatashreya2b$late <- as.numeric(smalldatashreya2b$year == 1998)
modelshreya2 <- lm(fehome~Democrat*late,smalldatashreya2b)
stargazer(modelshreya2,header=FALSE,title="",type="text")

# Religious vs. Not
gss$none <- gss$relig==4
table(gss$none)
smalldatashreya3<-gss[,c('year','fehome','none')]
smalldatashreya3<-droplevels(smalldatashreya3)
smalldatashreya3<-na.omit(smalldatashreya3)
ggplot(smalldatashreya3,aes(x=year,y=fehome,col=none))+ylab("% disagree")+
  geom_smooth()+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

smalldatashreya3b <-  smalldatashreya3[smalldatashreya3$year == 1974 | smalldatashreya3$year == 1998, ]
smalldatashreya3b$late <- as.numeric(smalldatashreya3b$year == 1998)
modelshreya3 <- lm(fehome~none*late,smalldatashreya3b)
stargazer(modelshreya3,header=FALSE,title="",type="text")

#checking if they work for govt/military
gss$govtjob <- gss$indus10>=9370
table(gss$govtjob)
smalldatashreya4<-gss[,c('year','fehome','govtjob')]
smalldatashreya4<-droplevels(smalldatashreya4)

smalldatashreya4<-na.omit(smalldatashreya4)
ggplot(smalldatashreya4,aes(x=year,y=fehome,col=govtjob))+
  geom_smooth()+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

smalldatashreya4b <-  smalldatashreya4[smalldatashreya4$year == 1974 | smalldatashreya4$year == 1998, ]
smalldatashreya4b$late <- as.numeric(smalldatashreya4b$year == 1998)
modelshreya4 <- lm(fehome~govtjob*late,smalldatashreya4b)
stargazer(modelshreya4,header=FALSE,title="",type="text")

# Religion Dummy Variables
get_labels(gss$relig)
gss$protestant <- as.numeric(gss$relig==1)
gss$catholic <- as.numeric(gss$relig==2)
gss$jewish <- as.numeric(gss$relig==3)
gss$none <- gss$relig==4
# Fundamentalism
get_labels(gss$fund)
gss$fundam <- as.numeric(gss$fund == 1)
# Attendance
gss <- gss %>% mutate(attendnum = case_match(attend,
                                             0~0,1~0.5,2~1.5,3~6,4~12,5~30,6~40,7~52,8~100))
table(gss$attendnum)
# Time
gss$late <- as.numeric(gss$year >= 1992)

# Respondent Income in Thousands Constant Dollars
get_label(gss$conrinc)
summary(gss$conrinc)
gss$conrincth <- gss$conrinc/1000
summary(gss$conrincth)

# Political Party
library(stringr)
gss$partyid <- ifelse(gss$partyid == 7, NA, gss$partyid)
gss$Democrat <- gss$partyid<=2

# Race
get_labels(gss$race)
gss$black <- gss$race==2

model4<-lm(fehome~late+educ+conrincth+age+sex+black+protestant+
             catholic+jewish+fundam+attendnum+partyid,gss)
model5<-lm(fehome~late+educ+conrincth+age+sex+black+protestant+
             catholic+jewish+none,gss)
model6<-lm(fehome~late+protestant+catholic+jewish+none+
             fundam+attendnum,gss)
model7<-lm(fehome~late+educ+conrincth+age+sex+black+none*late,gss)

stargazer(model4,model5,model6,model7,header=FALSE,title="",single.row = TRUE,type="text")


# regressing on work status 
gss$female_job <- as.numeric(gss$sex == 2 & gss$wrkstat <= 3)
gss$female_house <- as.numeric(gss$sex == 2 & gss$wrkstat == 7)
gss$male_job <- as.numeric(gss$sex == 1 & gss$wrkstat <= 3)
gss$male_house <- as.numeric(gss$sex == 1 & gss$wrkstat == 7)

job_intent<- lm(fehome ~ female_job+female_house+male_job+male_house, gss)
stargazer(job_intent, type="text")


setwd("~/Desktop/REE")
library(haven)
library(dplyr)
library(sjlabelled)
gss<- readRDS("gss.RDS")

# Turn fepol (1 = agree, 2 = disagree) into percentage
gss$fepolperc <- (gss$fepol-1)*100

# Plot
library(ggplot2)
library(ggthemes)
theme_set(theme_tufte())
ggplot(gss,aes(x=year,y=fepolperc,))+ylab("% disagree")+
  ggtitle('"Most men are better suited emotionally\nfor politics than are most women."')+
  geom_smooth()+scale_colour_brewer(palette = "Set1")

# Gendered Plot
sexes<-get_labels(gss$sex)
gss$sexname <- case_match(gss$sex,1~sexes[1],2~sexes[2])
smalldata<-gss[,c('year','fepolperc','sexname')]
smalldata<-droplevels(smalldata)
smalldata<-na.omit(smalldata)
ggplot(smalldata,aes(x=year,y=fepolperc,col=sexname))+ylab("% disagree")+
  geom_smooth()+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

# Difference in Difference
library(stargazer)
smalldatab <-  smalldata[smalldata$year == 1974 | smalldata$year == 2022, ]
smalldatab$late <- as.numeric(smalldatab$year == 2022)
smalldatab$Female <- as.numeric(smalldatab$sexname == 'female')
model1 <- lm(fepolperc~Female*late,smalldatab)
stargazer(model1,header=FALSE,title="",type="text")

# Political Party
smalldata3<-gss[gss$partyid <=2 | gss$partyid %in%c(3:6),]
smalldata3<-smalldata3[,c('year','fepolperc','partyid')]
smalldata3<-droplevels(smalldata3)
smalldata3<-na.omit(smalldata3)
smalldata3$Democrat <- smalldata3$partyid <=2
ggplot(smalldata3[smalldata3$year %in% c(1988:2018),],aes(x=year,y=fepolperc,col=Democrat))+ylab("% disagree")+
  geom_smooth()+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

# Difference in Difference
smalldata3b <-  smalldata3[smalldata3$year == 1988 | smalldata3$year == 2018, ]
smalldata3b$late <- as.numeric(smalldata3b$year == 2018)
model3 <- lm(fepolperc~Democrat*late,smalldata3b)
stargazer(model3,header=FALSE,title="",type="text")

# Religious vs. Not
get_labels(gss$relig)
gss$none <- gss$relig==4
table(gss$none)
smalldata2<-gss[,c('year','fepolperc','none')]
smalldata2<-droplevels(smalldata2)
smalldata2<-na.omit(smalldata2)
ggplot(smalldata2,aes(x=year,y=fepolperc,col=none))+ylab("% disagree")+
  geom_smooth()+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

ggplot(smalldata2,aes(x=year,y=fepolperc,col=none))+ylab("% disagree")+
  geom_smooth(method="lm",se=FALSE)+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

ggplot(smalldata2,aes(x=year,y=fepolperc,group=interaction(none, year>1995),col=none))+
  geom_smooth(method="lm")+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

# Difference in Difference
smalldata2b <-  smalldata2[smalldata2$year == 1974 | smalldata2$year == 2022, ]
smalldata2b$late <- as.numeric(smalldata2b$year == 2022)
model2 <- lm(fepolperc~none*late,smalldata2b)
stargazer(model2,header=FALSE,title="",type="text")

# Map
library(readxl) 
library(usmap)
library(grid)
library(gridExtra) 
regions<-get_labels(gss$region) 
regions[6]<-"east south central"
gss <- gss %>% mutate(regionname = case_match(region,1~regions[1],
                                              2~regions[2],3~regions[3],4~regions[4],5~regions[5],
                                              6~regions[6],7~regions[7],8~regions[8],9~regions[9]))
fepol74 <- gss[gss$year ==1974,] %>% group_by(regionname) %>% 
  summarize(Fepol=mean(fepolperc,na.rm=TRUE))
fepol22 <- gss[gss$year ==2022,] %>% group_by(regionname) %>% 
  summarize(Fepol=mean(fepolperc,na.rm=TRUE))
statetoregion <- read_excel("statetoregion.xlsx")
fepol74<-merge(fepol74,statetoregion)
fepol22<-merge(fepol22,statetoregion)
g74<-plot_usmap(data = fepol74, values = "Fepol")+ggtitle("1974")
g22<-plot_usmap(data = fepol22, values = "Fepol")+ggtitle("2022")
grid.arrange(g74,g22,ncol=2,top=textGrob("% Disagree"))

# Multivariate Regression
# Religion Dummy Variables
get_labels(gss$relig)
gss$protestant <- as.numeric(gss$relig==1)
gss$catholic <- as.numeric(gss$relig==2)
gss$jewish <- as.numeric(gss$relig==3)
# Fundamentalism
get_labels(gss$fund)
gss$fundam <- as.numeric(gss$fund == 1)
# Attendance
gss <- gss %>% mutate(attendnum = case_match(attend,
                                             0~0,1~0.5,2~1.5,3~6,4~12,5~30,6~40,7~52,8~100))
table(gss$attendnum)
# Time
gss$after1995 <- as.numeric(gss$year >= 1995)
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

model4<-lm(fepol~after1995+educ+conrincth+age+sexname+black+protestant+
             catholic+jewish+none+fundam+attendnum+partyid,gss)
model5<-lm(fepol~after1995+educ+conrincth+age+sexname+black+protestant+
             catholic+jewish+none,gss)
model6<-lm(fepol~after1995+protestant+catholic+jewish+none+
                     fundam+attendnum,gss)
model7<-lm(fepol~after1995+educ+conrincth+age+sexname+black+none*after1995,gss)

stargazer(model4,model5,model6,model7,header=FALSE,title="",single.row = TRUE,type="text")

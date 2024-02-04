library(haven) 
library(dplyr) 
install.packages("sjlabelled")
library(sjlabelled) # for extracting labels
library(stargazer)

# Change this to the path for the folder in which you saved the data
#setwd("/Users/shreyachalla/Downloads/gss.RDS")
file_path <- "/Users/shreyachalla/Downloads/gss.RDS"

# Extract the directory
directory <- dirname(file_path)

# Set the working directory to the extracted directory
setwd(directory)

# Read the data from RDS file
gss<- readRDS("gss.RDS")

attr(gss$fehome,"labels")
table(gss$fehome)
gss2 <- gss[complete.cases(gss$fehome), ]



library(ggplot2) # Load ggplot2 library for plotting
library(ggthemes) # Load ggthemes library for themes
theme_set(theme_tufte()) # This is my favorite theme, but you can use any other

result <- gss2 %>%
  group_by(year) %>%
  summarize(fehomeper = sum(fehome == 2) / n() * 100)

# Merge the result back to the original data frame
gss2 <- merge(gss2, result, by = "year", all.x = TRUE)
#gss$fehomeper
ggplot(gss2,aes(x=year,y=fehomeper))+
  geom_smooth()+
  labs(y = "% disagree")

table(gss$fehome,gss$year)
table(gss2$fehome,gss2$year)
validgss<- gss2[gss2$year %in% c(1974, 1998), ]

gss_small <-  gss2[gss2$year <= 1980  | gss2$year >= 1992, ]
gss_small$late <- as.numeric(gss_small$year==1998)
gss_small$Female <- as.numeric(gss_small$sex==2)

model1 <- lm(fehomeper~Female*late, gss_small)
print(model1)
stargazer(model1,header=FALSE,title="",type="text")

#subset_data <- gss2[gss2$year %in% c(1978, 1998), ]
#subset_data$year <- as.factor(subset_data$year)

#ggplot(subset_data, aes(x = year, y = fehomeper, col = as.factor(sex))) +
  #geom_smooth() +
  #labs(y = "% disagree")

ggplot(gss2[gss_small2$year==1978 | gss_small2$year==1998,],aes(x=year,y=fehomeper,col=as.factor(sex)))+ geom_smooth()+labs(y = "% disagree")

#checking if they work for govt/military
gss_small$govtjob <- as.numeric(gss_small$indus10>=9370)
gss_small2 <- gss_small[complete.cases(gss_small$govtjob), ]

ggplot(gss_small2[gss_small2$year==1978 | gss_small2$year==1998,], aes(x = year, y = fehomeper, col = as.factor(govtjob))) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 5)) +
  theme_minimal()+labs(y = "% disagree")

gss_smaller2 <-  gss_small2[gss_small2$year <= 1980  | gss_small2$year >= 1992, ]
gss_smaller2$late <- as.numeric(gss_smaller2$year>=1992)

model2 <- lm(fehomeper~govtjob*late, gss_smaller2)
print(model2)
stargazer(model2,header=FALSE,title="",type="text")

gss_small3<-gss2[gss2$partyid <=2 | gss2$partyid %in%c(3:6),]
gss_small3<-gss_small3[,c('year','fehomeper','partyid')]
gss_small3<-droplevels(gss_small3)
gss_small3<-na.omit(gss_small3)
gss_small3$Democrat <- gss_small3$partyid <=2
ggplot(gss_small3,aes(x=year,y=fehomeper,col=Democrat))+ylab("% disagree")+
  geom_smooth()+scale_colour_brewer(palette = "Set1")+
  theme(legend.position="bottom")

# Difference in Difference
gss_small3diff <-  gss_small3[gss_small3$year <=1980 | gss_small3$year >= 1992, ]
gss_small3diff$late <- as.numeric(gss_small3diff$year >=1992)
model3 <- lm(fehomeper~Democrat*late,gss_small3diff)
stargazer(model3,header=FALSE,title="",type="text")


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

# Respondent Income 
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


library(haven) # to deal with haven labeled variables
library(dplyr) # For mutating variables
library(sjlabelled) # for extracting labels
library(ggplot2)
library(stargazer)

# Change this to the path for the folder in which you saved the data
setwd("/Users/emilyhuang/econ452/econ452p1")

# Read the data from RDS file
gss<- readRDS("gss.RDS")

table(gss$fepresch,gss$year)
validYears<-gss[gss$year %in% c(1988:2022),]
validYears<-validYears[,c("fepresch","year","sex", "madeg", "relig", "marital", "wrkstat", "padeg", "conrinc", "income", "rincome")]
validYears$Female<-as.numeric(validYears$sex==2)
validYears$fepresch<-4-validYears$fepresch

validYears2<-validYears[validYears$year %in% c(1988,2022),]
validYears2$late<-as.numeric(validYears2$year==2022)
validYears2<-na.omit(validYears2)

# women no degree - women with degree 
earlydiff<-mean(validYears2[validYears2$year==1988 & validYears2$madeg==0,]$fepresch,na.rm = TRUE)-
  mean(validYears2[validYears2$year==1988 & validYears2$madeg > 0,]$fepresch,na.rm = TRUE)

latediff<-mean(validYears2[validYears2$year==2022 & validYears2$madeg==0,]$fepresch,na.rm = TRUE)-
  mean(validYears2[validYears2$year==2022 & validYears2$madeg > 0,]$fepresch,na.rm = TRUE)

print(latediff)
print(earlydiff)
diffindiff<-latediff-earlydiff
print(diffindiff)

validYears$mom_no_degree<-as.numeric(validYears$madeg == 0)

validYears2$mom_no_degree<-as.numeric(validYears2$madeg == 0)
model1 <- lm(fepresch~mom_no_degree*late,data=validYears2)
summary(model1)
stargazer(model1, type = "text", single.row = TRUE)

validYears<-na.omit(validYears)
ggplot(validYears,aes(x=year,y=fepresch,color=as.factor(mom_no_degree)))+
  geom_smooth()+
  labs(title="",
       x="Year",
       y="is the mother working harmful to children?",
       color="hasNoDegree")+
  theme_minimal()

# men minus women in 1988
earlydiff<-mean(validYears2[validYears2$year==1988 & validYears2$Female==0,]$fepresch,na.rm = TRUE)-
  mean(validYears2[validYears2$year==1988 & validYears2$Female==1,]$fepresch,na.rm = TRUE)

# men minus women in 2022
latediff<-mean(validYears2[validYears2$year==2022 & validYears2$Female==0,]$fepresch,na.rm = TRUE)-
  mean(validYears2[validYears2$year==2022 & validYears2$Female==1,]$fepresch,na.rm = TRUE)

diffindiff<-latediff-earlydiff
print(diffindiff)

model2 <- lm(fepresch~Female*late,data=validYears2)
summary(model2)
stargazer(model2, type = "text", single.row = TRUE)

validYears<-na.omit(validYears)
ggplot(validYears,aes(x=year,y=fepresch,color=as.factor(Female)))+
  geom_smooth()+
  labs(title="",
       x="Year",
       y="is the mother working harmful to children?",
       color="isFemale")+
  theme_minimal()

# filters out null values for simpler mapping 
gss_filtered <- validYears %>%
  filter(!is.na(year) & !is.na(fepresch) & !is.na(sex))

gss_filtered <- gss_filtered %>% mutate(madeg_label 
               = case_match(madeg, 0~"no degree", 1~"<high school",
               2~"jc/associates", 3~"bachelor's", 4~"grad"))
# Filter out NA values in madeg_label
gss_filtered_madeg <- gss_filtered[!is.na(gss_filtered$madeg_label),]
# Plot based on madeg_label
ggplot(gss_filtered_madeg, aes(x = year, y = fepresch, 
                               col = factor(madeg_label))) + geom_smooth()

validYears$late<-as.numeric(validYears$year==2022)

# other regressors included
# validYears$noincome<-as.numeric(is.na(validYears$realinc))
validYears$realincome<-validYears$conrinc/10000 # Measure in tens of thousands
# create variables for difference in income
validYears$other_income <- validYears$income - validYears$rincome
# income diff between respondent income and remaining familial income
validYears$income_diff <- abs(validYears$other_income - validYears$rincome)

validYears$christian <- as.numeric(validYears$relig == 1 | validYears$relig == 2 | validYears$relig == 10 | validYears$relig == 13)
validYears$no_relig <- as.numeric(validYears$relig == 4)
validYears$married <- as.numeric(validYears$marital == 1)
validYears$working <- as.numeric(validYears$wrkstat <= 2)
validYears$no_padeg<- as.numeric(validYears$padeg == 0)
validYears$madeg_post_hs<- as.numeric(validYears$madeg > 1)
validYears$madeg_post_college<- as.numeric(validYears$madeg >= 3)
validYears$padeg_post_hs<- as.numeric(validYears$padeg > 1)

all_vars<- lm(fepresch ~ no_padeg + madeg_post_college + married + christian 
              + no_relig + wrkstat + Female * late + mom_no_degree * late + realincome + income_diff, validYears)

summary(all_vars)
stargazer(all_vars, type = "text", single.row = TRUE)


# female and male job intentions
validYears$female_job <- as.numeric(validYears$sex == 2 & validYears$wrkstat <= 3)
validYears$female_house <- as.numeric(validYears$sex == 2 & validYears$wrkstat == 7)
validYears$male_job <- as.numeric(validYears$sex == 1 & validYears$wrkstat <= 3)
validYears$male_house <- as.numeric(validYears$sex == 1 & validYears$wrkstat == 7)

job_intent<- lm(fepresch ~ female_job + female_house + male_job + male_house, validYears)
summary(job_intent)
stargazer(job_intent, type = "text")

attr(gss$wrkstat, "labels")
attr(gss$childs, "labels")


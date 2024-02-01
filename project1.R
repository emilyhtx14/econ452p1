# Load required libraries
install.packages("rmarkdown")

library(haven) # to deal with haven labeled variables
library(dplyr) # For mutating variables
library(sjlabelled) # for extracting labels
library(ggplot2)
library(stargazer)

# Change this to the path for the folder in which you saved the data
setwd("/Users/emilyhuang/econ452/econ452p1")

# Read the data from RDS file
gss<- readRDS("gss.RDS")

# preschool kids suffer if mother works, higher the number is the more likely
# the person thinks that preschoolers will suffer

# 1 is strongly agree, 4 is strongly disagree
gss <- gss %>% mutate(fepreschnum = case_match(fepresch, 1~100,2~75,3~50,4~25))
attr(gss$degree, "labels")

# filters out null values for simpler mapping 
gss_filtered <- gss %>%
  filter(!is.na(year) & !is.na(fepresch) & !is.na(sex))

gss_filtered <- gss_filtered %>% mutate(madeg_label 
                = case_match(madeg, 0~"no degree", 1~"<high school",
                2~"jc/associates", 3~"bachelor's", 4~"grad"))
gss_filtered <- gss_filtered %>% mutate(sex_label = case_match(madeg, 1~"Male", 2~"Female"))


# Filter out NA values in sex_label
gss_filtered_sex <- gss_filtered[!is.na(gss_filtered$sex_label),]

# Plot based on sex_label
ggplot(gss_filtered_sex, aes(x = year, y = fepreschnum, col = factor(sex_label))) + geom_smooth()

# less than high school  0, high school 1, associate/junior college 2, bachelor 3, grad 4 
# discovery that mother with no degree i

# Filter out NA values in madeg_label
gss_filtered_madeg <- gss_filtered[!is.na(gss_filtered$madeg_label),]

# Plot based on madeg_label
ggplot(gss_filtered_madeg, aes(x = year, y = fepreschnum, col = factor(madeg_label))) + geom_smooth()

# year is less than 2006 or greater than 2015, subset early years and late years
gss3 <- gss[gss$year <= 2006| gss$year > 2015, ]
gss3$late <- as.numeric(gss3$year > 2015)

# analysis for female * late
gss3$Female<- as.numeric(gss3$sex == 2)
female_late <- lm(fepreschnum~Female*late, gss3)
summary(female_late)
stargazer(female_late, type = "text")

# analysis for mother's degree * late

# create variable for mother's degree
gss3$mom_no_degree <- as.numeric(gss3$madeg == 0)
gss3$mom_hs_degree <- as.numeric(gss3$madeg == 1)
gss3$mom_jc_degree <- as.numeric(gss3$madeg == 2)
gss3$mom_bc_degree <- as.numeric(gss3$madeg == 3)


mother_deg_late<- lm(fepreschnum~ mom_no_degree*late + mom_hs_degree * late + mom_jc_degree * late 
              + late * mom_bc_degree, gss3)

summary(mother_deg_late)
stargazer(mother_deg_late, type = "text")


# female job intentions + children in the home
gss3$female_job_c <- as.numeric(gss3$sex == 2 & gss3$wrkstat <= 3 & gss3$childs > 0)
gss3$female_house_c <- as.numeric(gss3$sex == 2 & gss3$wrkstat == 7 & gss3$childs > 0)
gss3$female_job_nc <- as.numeric(gss3$sex == 2 & gss3$wrkstat <= 3 & gss3$childs == 0)
gss3$female_house_nc <- as.numeric(gss3$sex == 2 & gss3$wrkstat == 7 & gss3$childs == 0)

job_intent<- lm(fepreschnum ~female_job_c + female_house_c + female_job_nc + female_house_nc, gss3)
summary(job_intent)
stargazer(job_intent, type = "text")


attr(gss$wrkstat, "labels")
attr(gss$childs, "labels")

##########################

# create variable for income 

gss3$noincome<-as.numeric(is.na(gss3$realinc))
gss3$realincome<-gss3$conrinc/10000 # Measure in tens of thousands
gss3$realincome[is.na(gss3$realincome)]<-0

attr(gss$madeg,"labels")

# less than high school  0, high school 1,   associate/junior college 2, bachelor 3, grad 4 
# discovery that mother with no degree i

# create variable for father's degree
gss3$dad_no_degree <- as.numeric(gss3$padeg == 0)
gss3$dad_hs_degree <- as.numeric(gss3$padeg == 1)
gss3$dad_jc_degree <- as.numeric(gss3$padeg == 2)
gss3$dad_bc_degree <- as.numeric(gss3$padeg == 3)

# create variables for difference in income
gss3$other_income <- gss3$income - gss3$rincome
# income diff between respondent income and remaining familial income
gss3$income_diff <- abs(gss3$other_income - gss3$rincome)
  
#income_normal<- lm(fepreschnum ~ realincome + noincome + income_diff + rincome, gss3)
#summary(income_normal)
#stargazer(income_normal, type = "text")

all_normal<- lm(fepreschnum~realincome + noincome + Female + 
                mom_no_degree + mom_hs_degree + mom_jc_degree 
              +  mom_bc_degree + dad_no_degree + dad_hs_degree + dad_jc_degree
              +  dad_bc_degree + income_diff, gss3)

all_normal<- lm(fepreschnum~realincome + noincome + Female + 
                  padeg + madeg, gss3)
summary(all_normal)
stargazer(all_normal, type = "text")

all_late<- lm(fepreschnum~realincome*late + noincome*late + Female*late + 
                mom_no_degree*late + mom_hs_degree * late + mom_jc_degree * late 
              + late * mom_bc_degree + dad_no_degree*late + dad_hs_degree * late + dad_jc_degree * late 
              + late * dad_bc_degree + late*income_diff, gss3)

summary(all_late)
stargazer(all_late, type = "text")

degree_late<- lm(fepreschnum~ mom_no_degree*late + mom_hs_degree * late + mom_jc_degree * late 
              + late * mom_bc_degree + dad_no_degree*late + dad_hs_degree * late + dad_jc_degree * late 
              + late * dad_bc_degree, gss3)

summary(degree_late)
stargazer(degree_late, type = "text")

degree<- lm(fepreschnum~ mom_no_degree + mom_hs_degree  + mom_jc_degree 
                 +  mom_bc_degree + dad_no_degree + dad_hs_degree + dad_jc_degree
                 + dad_bc_degree, gss3)

summary(degree)
stargazer(degree, type = "text")


incomediff <- lm(fepreschnum~late*income_diff, gss3)
summary(incomediff)
stargazer(incomediff, type = "text")

degree<- lm(fepreschnum~mom_no_degree*late + mom_hs_degree * late + mom_jc_degree * late 
              + late * mom_bc_degree + dad_no_degree*late + dad_hs_degree * late + dad_jc_degree * late 
            + late * dad_bc_degree, gss3)
summary(degree)
stargazer(degree, type = "text")





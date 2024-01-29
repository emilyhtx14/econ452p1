# Load required libraries
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

gss_1988 <- gss[gss$year > 1988, ]
table(gss$fepreschnum)
ggplot(gss_1988,aes(x=year,y=fepreschnum))+ geom_smooth()

# filters out null values for simpler mapping 
gss_filtered <- gss %>%
  filter(!is.na(year) & !is.na(fepresch) & !is.na(sex))

gss_filtered <- gss_filtered %>% mutate(madeg_label = case_match(madeg, 0~"no degree" ,1~"<high school",2~"jc/associates",3~"bachelor's", 4~"grad"))
gss_filtered <- gss_filtered %>% mutate(sex_label = case_match(madeg, 1~"Male", 2~"Female"))
# Remove NA values from fepreschnum column
# gss_filtered <- na.omit(gss_filtered)

# year is less than 2015 or greater than 2015, subset early years and late years
gss3 <- gss[gss$year <= 2006| gss$year > 2006, ]
gss3$late <- as.numeric(gss3$year > 2006)

# 2 specific years as endpoints
gss3 <- gss[gss$year == 1988 | gss$year == 2022, ]
gss3$late <- as.numeric(gss3$year == 2022)

gss3$Female<- as.numeric(gss3$sex == 2)

# interact female with late dummy variable 
female_late <- lm(fepreschnum~Female*late, gss3)
summary(female_late)
stargazer(female_late, type = "text")

# interact late with other variables

# create variable for income 

gss3$noincome<-as.numeric(is.na(gss$realinc))
gss3$realincome<-gss$conrinc/10000 # Measure in tens of thousands
gss3$realincome[is.na(gss$realincome)]<-0

attr(gss$madeg,"labels")

# less than high school  0, high school 1,   associate/junior college 2, bachelor 3, grad 4 
# discovery that mother with no degree i

# create variable for mother's degree
gss3$mom_no_degree <- as.numeric(gss3$madeg == 0)
gss3$mom_hs_degree <- as.numeric(gss3$madeg == 1)
gss3$mom_jc_degree <- as.numeric(gss3$madeg == 2)
gss3$mom_bc_degree <- as.numeric(gss3$madeg == 3)

# create variable for father's degree
gss3$dad_no_degree <- as.numeric(gss3$padeg == 0)
gss3$dad_hs_degree <- as.numeric(gss3$padeg == 1)
gss3$dad_jc_degree <- as.numeric(gss3$padeg == 2)
gss3$dad_bc_degree <- as.numeric(gss3$padeg == 3)

# create variables for difference in income
gss3$other_income <- gss3$income - gss3$rincome
# income diff between respondent income and remaining familial income
gss3$income_diff <- abs(gss3$other_income - gss3$rincome)
  
inc_late<- lm(fepreschnum~realincome*late + noincome*late + Female*late + 
                mom_no_degree*late + mom_hs_degree * late + mom_jc_degree * late 
              + late * mom_bc_degree + dad_no_degree*late + dad_hs_degree * late + dad_jc_degree * late 
              + late * dad_bc_degree + late*income_diff, gss3)

summary(inc_late)
stargazer(inc_late, type = "text")

incomediff <- lm(fepreschnum~late*income_diff, gss3)
summary(incomediff)
stargazer(incomediff, type = "text")

degree<- lm(fepreschnum~mom_no_degree*late + mom_hs_degree * late + mom_jc_degree * late 
              + late * mom_bc_degree + dad_no_degree*late + dad_hs_degree * late + dad_jc_degree * late 
            + late * dad_bc_degree, gss3)
summary(degree)
stargazer(degree, type = "text")

# plot men vs women fepreschnum
ggplot(gss_filtered, aes(x = year, y = fepreschnum, col = factor(sex), linetype = factor(sex))) +
  geom_smooth() +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Male", "Female")) +
  labs(col = "Sex") +  # Optionally, customize the color legend title
  theme(legend.position = "bottom") +  # Optionally, move the legend to the bottom
  guides(col = FALSE)

ggplot(gss_filtered, aes(x = year, y = fepreschnum, col = factor(sex_label))) + geom_smooth()

# ggplot(gss_filtered, aes(x = year, y = fepreschnum, col = factor(race))) + geom_smooth()

# less than high school  0, high school 1, associate/junior college 2, bachelor 3, grad 4 
# discovery that mother with no degree i

ggplot(gss_filtered, aes(x = year, y = fepreschnum, col = factor(madeg))) + geom_smooth()

ggplot(gss_filtered, aes(x = year, y = fepreschnum, col = factor(madeg_label))) + geom_smooth()

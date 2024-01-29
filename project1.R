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
# the person thinks that preschoolers

# 1 is strongly agree, 4 is strongly disagree
gss <- gss %>% mutate(fepreschnum = case_match(fepresch, 1~100,2~75,3~50,4~25))

attr(gss$degree,"labels")
attr(gss$madeg,"labels")



gss_1988 <- gss[gss$year > 1988, ]
table(gss$fepreschnum)
ggplot(gss_1988,aes(x=year,y=fepreschnum))+ geom_smooth()

# filters out null values for simpler mapping 
gss_filtered <- gss %>%
  filter(!is.na(year) & !is.na(fepresch) & !is.na(sex))

# year is less than 2015 or greater than 2015, subset early years and late years
gss3 <- gss[gss$year <= 2006| gss$year > 2006, ]
gss3$late <- as.numeric(gss3$year > 2006)

# 2 specific years as endpoints
gss3 <- gss[gss$year == 1988 | gss$year == 2022, ]
gss3$late <- as.numeric(gss3$year = 2022)

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

# create variable for mother's degree
gss3$mom_degree<- as.numeric(gss3$madeg == 0)

inc_late<- lm(fepreschnum~realincome*late + noincome*late + Female*late + mom_degree*late, gss3)
summary(inc_late)
stargazer(inc_late, type = "text")


# plot men vs women fepreschnum
ggplot(gss_filtered, aes(x = year, y = fepreschnum, col = factor(sex), linetype = factor(sex))) +
  geom_smooth() +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Male", "Female")) +
  labs(col = "Sex") +  # Optionally, customize the color legend title
  theme(legend.position = "bottom") +  # Optionally, move the legend to the bottom
  guides(col = FALSE)



ggplot(gss_filtered, aes(x = year, y = fepreschnum, col = factor(race)))


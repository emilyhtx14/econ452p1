# For any libraries that you do not have installed, 
# you can install them by running 
# install.packages("libraryname") in the console

# Load required libraries
library(haven) # to deal with haven labeled variables
library(dplyr) # For mutating variables
library(sjlabelled) # for extracting labels

# Change this to the path for the folder in which you saved the data
setwd("/Users/emilyhuang/econ452/econ452p1")

# Read the data from RDS file
gss<- readRDS("gss.RDS")

# Check the values and labels of the 'fepresch' variable
head(gss$fepresch)
head(gss$fechId)


# Create a new variable 'attendnum' with numeric values based on 'fepresch' variable
#gss <- gss %>% mutate(attendnum = case_match(attend, 0~0,1~0.5,2~1.5,3~6,4~12,5~30,6~40,7~52,8~100))

# preschool kids suffer if mother works

# 1 is strongly agree, 4 is strongly disagree
gss <- gss %>% mutate(fepreschnum = case_match(fepresch, 1~100,2~75,3~50,4~25))
# gss <- gss %>% mutate(fepreschnum = case_match(fepresch, 1~4,2~3,3~2,4~1))

gss_1988 <- gss[gss$year > 1988, ]
table(gss$fepreschnum)
ggplot(gss_1988,aes(x=year,y=fepreschnum))+ geom_smooth()



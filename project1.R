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

gss_1988 <- gss[gss$year > 1988, ]
table(gss$fepreschnum)
ggplot(gss_1988,aes(x=year,y=fepreschnum))+ geom_smooth()


# year is less than 2015 or greater than 2015, subset early years and late years
gss3 <- gss[gss$year <= 2015  | gss$year > 2015, ]
gss3$late <- as.numeric(gss3$year > 2015)

# 2 specific years as endpoints
# gss3 <- gss[gss$year = 1988  | gss$year = 2022, ]
# gss3$late <- as.numeric(gss3$year = 2022)


gss3$Female<- as.numeric(gss3$sex == 2)

# filters out null values for simpler mapping 
gss_filtered <- gss %>%
  filter(!is.na(year) & !is.na(fepresch) & !is.na(sex))

# interact female with late dummy variable 
female_late <- lm(fepresch~Female*late, gss3)
summary(female_late)
stargazer(female_late, type = "text")

# plot men vs women fepreschnum
ggplot(gss_filtered, aes(x = year, y = fepreschnum, col = factor(sex), linetype = factor(sex))) +
  geom_smooth() +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Male", "Female")) +
  labs(col = "Sex") +  # Optionally, customize the color legend title
  theme(legend.position = "bottom") +  # Optionally, move the legend to the bottom
  guides(col = FALSE)





###############################
# ECON 43 Final Project
# Sophie Harpt, Lauren Houge, Chris Olson, Mina Tawfik, Larry Yang
# 6/6/2019
###############################

# The goal of our project is to analyze and visualize data on job-to-job
# flows in the U.S. by different groupings of individuals and firms.
# The data is from the Longitudinal Employer-Household Dynamics survey from the U.S. Census Bureau


# Preliminary steps:
rm(list = ls())
options(scipen = 9)
setwd("/Users/sophieharpt/Desktop/ECON")

library(doBy)
library(dplyr)
library(foreign)
library(gdata)
library(ggplot2)
library(readstata13)
library(sandwich)
library(stargazer)
library(ISwR)
library(psych)
library(car)
library(rgl)
library(gridExtra)
library(maps)
library(maptools)
library(rgdal)
library(tibble)
library(corrplot)
library(igraph)
library(gcookbook)
library(tm)
library(wordcloud)
library(htmltab)
library(rvest)
library(RColorBrewer)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(stringr)

# Data is read into R, subset is created for seasonally adjusted values in 2017
j2j1=read.csv("j2j_us_all.csv")
j2j=subset(j2j1, year==2017 & seasonadj=="S")


###############################
# Question 1: Visualize the turnover rates for each race during 2017. Which race experiences the
# highest turnover rates? Which has the lowest?

# A subset is created to capture observations at the national level that are aggregated by race
# and exclude observations for all races
j2jrace=subset(j2j, geo_level=="N" & race!="A0" & agg_level==5)
j2jrace$EEHire=log(j2jrace$EEHire)

# A new variable is created to change the race codes into words
j2jrace$Race=ifelse(j2jrace$race=="A1", "White", ifelse(j2jrace$race=="A2", "African American", ifelse(j2jrace$race=="A3", "American Indian", ifelse(j2jrace$race=="A4", "Asian", ifelse(j2jrace$race=="A5", "Pacific Islander", "Two or More")))))


# The plot is created
ggplot(j2jrace, aes(x=quarter, y=EEHire, colour=Race)) + geom_line(size=1.5) +# generates line plot 
  xlab("Quarter") + ylab("Log of hires") + ggtitle("Job to Job Hires by Race") +
  theme(axis.title.x=element_text(face="bold", colour="darkred", size=14), axis.title.y = element_text(face="bold.italic", size=16))+
  geom_point(size=2, colour="darkred") + guides(fill=guide_legend(title="Race"))

# Explanation: This plot shows how the job flows changed for each race throughout the year in 2017.
# The graph shows that for each race, job flows remained relatively constant each quarter of the year.
# More informative, however, are the levels of each line, which show that whites have
# The highest annual turnover, while Pacific Islanders have the lowest.

########################################
# Question 2: How do the job-to-job hire rates differ between age groups in 2017?

# A subset is made to isolate data which varies by age and the data is aggregated to sum
# toatl job-to-job hires in 2017
j2jmina=subset(j2j, agg_level==3)
j2jmina1=aggregate(j2jmina[c("EEHire")], by=list(j2jmina$agegrp), sum, na.rm=TRUE)
colnames(j2jmina1)[c(1)] <- c("age")


j2jmina1$number=ifelse(j2jmina1$age=="A01", 1, ifelse(j2jmina1$age=="A02", 2, ifelse(j2jmina1$age=="A03", 3, ifelse(j2jmina1$age=="A04", 4, ifelse(j2jmina1$age=="A05", 5, ifelse(j2jmina1$age== "A06", 6, ifelse(j2jmina1== "A07", 7, 8)))))))


ggplot(j2jmina1, aes(x=number, y=EEHire)) +
  geom_bar(stat = "identity") + xlab("Age Group") + ylab("Job to Job Flows") + ggtitle("Job to Job flows by Age Group") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, size=8, colour= "blue")) + theme(axis.text.y=element_text(hjust=1, size=8, colour ="blue")) + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8), labels=c("14-18","19-21","22-24","25-34","35-44","45-54","55-64","65-99"))

# Explanation: The graph shows a strong trend in job-to-job hires among age. The hire rate increases
# until age 25-34, and steadily decreases until the age of retirement.

##########################################
# Question 3: Is there correlation between the size of the firm and the job-to-job hire rate it experiences?

# putting the data into subsets based on firm size
j2j2=subset(j2j1, seasonadj=="S")
j2jfirm=subset(j2j2, agg_level==129)
j2jfirm1=aggregate(j2jfirm[c("EEHire")],by=list(j2jfirm$firmsize, j2jfirm$year), sum, na.rm=TRUE) 
colnames(j2jfirm1)[c(1,2)] <- c("firmsize","year")
j2jfirm2=subset(j2jfirm1, firmsize!="N")

#plotting the data
ggplot(j2jfirm2, aes(x=firmsize, y=log(EEHire), fill=firmsize)) + geom_boxplot() + 
  xlab("Firm Size") + ylab("Job-to-Job Hires") +
  ggtitle("Firm Size and Job-to-Job Hires") +
  theme(plot.title=element_text(hjust=0.5, size=16, colour="brown", face="bold")) +
  theme(axis.title.x=element_text(face="bold", size=14), axis.title.y = element_text(face="bold", size=14), axis.line=element_line(colour="black", size=1.5)) +
  theme(legend.position = c(0.89,0.32)) +
  scale_fill_discrete(name = "Firm Size", labels = c("1-19", "20-49", "50-249", "250-499", "500+")) +
  theme(legend.title=element_text(face="bold", family="Times", colour="brown", size=13))

# Explanation: We pooled data from multiple years to have a larger sample size. The graph does not show
# a strong correlation between firm size and job-to-job turnover rates, though very large firms have the
# highest numbers by far. 

############################################
# Question 4: Visualize the turnover rate by industry 

# Subset by industry specific measurements and aggregate to sum the quarters
rate <- c("EEHire", "industry")
newdata <- j2j[rate]
newdata1=subset(newdata, industry!="00") #remove all 0s in industry column
df1=aggregate(newdata1[c("EEHire")],by=list(newdata1$industry), sum, na.rm=TRUE)
colnames(df1)[c(1)] = c("industry")

# Table with description for industry codes is read and merged with industry data
url <- "https://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart=2017"
table <- htmltab(doc = url, which = 1)

df2=merge(df1, table, by.y=c("Sector"), by.x=c("industry"))

# Plot is created
ggplot(df2, aes(x = Description, y = EEHire)) +
  geom_bar(stat = "identity") + xlab("Industry") + ylab("Log of Job to Job Hires") + ggtitle("Job to Job Hires by Industries")+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8, colour="red"))
+theme(axis.text.y  =element_text(angle=90, hjust=1, size=8, colour="red"))

# Explanation: The industries with the highest job-to-job hire rates are food services, waste management, healthcare
# and retail. Utilities has the lowest rate.

###########################################
# Question 5: How have the turnover rates in the the manufacturing industry changed over time? 

# Subset is made for manufacturing
j2jm=subset(j2j1, industry=="31-33")
manufacture=aggregate(j2jm[c("EEHire")],by=list(j2jm$industry, j2jm$year), sum, na.rm=TRUE)
colnames(manufacture)[c(1,2)] <- c("industry","year")

# Line plot is created to show trend over time
ggplot(manufacture, aes(x=year, y=EEHire)) + geom_line(size=1.5) + 
  xlab("Year") + ylab("Job to Job Hires") + ggtitle("Job to Job Hire Rate Over Time") +
  theme(axis.title.x=element_text(face="bold", colour="black", size=14), axis.title.y = element_text(face="bold", size=16))+
  geom_point(size=2, colour="purple")

# Explanation: The most notable feature of this plot is the major drop in job-to-job hires that occurred
# following the great recession. During this time, most people who left their jobs were most likely terminated
# and faced an unemployment spell after.

#############################################
# Question 6: How does education correlate with turnover rates?

# A subset is created for data which varies by education level
j2jedu=subset(j2j1, education=="E1" | education=="E2" | education=="E3" | education=="E4")

# Data is aggregated by year and education level to sum annual j2j hires
education=aggregate(j2jedu[c("EEHire")],by=list(j2jedu$education, j2jedu$year), sum, na.rm=TRUE)
colnames(education)[c(1,2)] <- c("education","year")

# Plot is created
ggplot(education, aes(x=education, y=log(EEHire), fill=education)) + geom_boxplot() +
  xlab("Education Level") + ylab("Log of hires") + ggtitle("Job to Job Hires and Education") +
  scale_fill_brewer(palette = 1) + theme(legend.position="none") + scale_x_discrete(labels=c("Less than high school", "High school/equivalent", "Some college", "Bachelor's/advanced degree")) 

# Explanation: This plot shows that the number of job to job hires increases with education attainment
# until the attainment of a college degree, which decreases the number of job to job hires.


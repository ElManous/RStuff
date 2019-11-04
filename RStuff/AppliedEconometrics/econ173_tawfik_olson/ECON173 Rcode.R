### ECON 173 Project R code ###

#clear the working space
rm(list = ls())

#setting the working directory
setwd("/Users/chrisolson/Desktop/Econ43")

# Load the packages
#install.packages("doBy")
#install.packages("dplyr")
#install.packages("foreign")
#install.packages("gdata")
#install.packages("ggplot2")
#install.packages("readstata13")
#install.packages("sandwich")
#install.packages("stargazer")
#install.packages("ISwR")
#install.packages("rgl")
#install.packages("gridExtra")

#Load the packages (must have been installed)
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
# turn off scientific notation except for big numbers. 
options(scipen = 9)

#read in data
cardata = read.csv("Car industry data.csv", header=TRUE)

#scatterplot of data
ggplot(cardata, aes(x=time, y=Cars_Japan)) + geom_point(size=2.5, colour="blue") + geom_vline(xintercept=12.5, linetype="dashed") + xlab("Month (1=March 2010)") + ylab("Cars Produced in Japan (in millions)")

#simple OLS regression
reg1 = lm(Cars_Japan ~ time, data=cardata)

#linear regression with the treatment and interaction term allowing for different slopes before and after
reg2 = lm(Cars_Japan ~ time + Treatment*time, data=cardata)

#quadratic regression, also with interaction term
reg3 = lm(Cars_Japan ~ time + Treatment*time + Treatment*I(time^2), data=cardata)

#last regression with controls included (design shown in Research Design section)
reg4 = lm(Cars_Japan ~ time + Treatment*time + Treatment*I(time^2) + Cars_world + Ind_growth, data=cardata)

#showing all regressions on a table
stargazer(reg1, reg2, reg3, reg4, title="Regression Results", type="text", column.labels=c("Simple OLS", "Linear RDD", "Quadratic RDD", "Quadratic RDD w/ Controls"), df=FALSE, digits=3)
          
          
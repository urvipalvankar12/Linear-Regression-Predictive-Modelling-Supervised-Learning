library(readxl)
library(tidyverse)
library(mice)
library(dplyr)
library(ggplot2)
library(fastDummies)
library(car)

#reading excel file
dog_walk<-read_excel("C:\\Users\\urvipalvankar\\Urvi\\Master of Management Analytics\\860 - Acquisition and management of data\\Final Assignment\\MMA860_2021W_FinalAssignmentDataB.xlsx",sheet=1)
str(dog_walk)
summary(dog_walk)

#Dummy Variables created to convert the character variables
dog_walk$mailman_yes<-ifelse(dog_walk$Mailman=="Yes",1,0)
dog_walk_final<-dummy_columns(dog_walk, select_columns = "Time_of_Day",remove_first_dummy = FALSE,remove_most_frequent_dummy = FALSE,ignore_na = FALSE,split = NULL,remove_selected_columns = FALSE)

#Creating dummy variables for someone else walks
dog_walk_final$Number_Incidents_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$Number_Incidents,0)
dog_walk_final$Walk_Length_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$Walk_Length,0)
dog_walk_final$Other_Dogs_in_Area_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$Other_Dogs_in_Area,0)
dog_walk_final$mailman_yes_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$mailman_yes,0)
dog_walk_final$Number_of_Squirrels_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$Number_of_Squirrels,0)
dog_walk_final$Fire_Hydrants_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$Fire_Hydrants,0)
dog_walk_final$Time_of_Day_AM_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$Time_of_Day_AM,0)
dog_walk_final$Time_of_Day_PM_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$Time_of_Day_PM,0)
dog_walk_final$Time_of_Day_Noon_dummy<-ifelse(dog_walk_final$Walker==1,dog_walk_final$Time_of_Day_Noon,0)


#Creating dummy variables for  walks with Alex and another person - last 30 obs
dog_walk_final$Number_Incidents_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$Number_Incidents,0)
dog_walk_final$Walk_Length_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$Walk_Length,0)
dog_walk_final$Other_Dogs_in_Area_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$Other_Dogs_in_Area,0)
dog_walk_final$mailman_yes_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$mailman_yes,0)
dog_walk_final$Number_of_Squirrels_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$Number_of_Squirrels,0)
dog_walk_final$Fire_Hydrants_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$Fire_Hydrants,0)
dog_walk_final$Time_of_Day_AM_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$Time_of_Day_AM,0)
dog_walk_final$Time_of_Day_PM_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$Time_of_Day_PM,0)
dog_walk_final$Time_of_Day_Noon_dummy2<-ifelse(dog_walk_final$Obs>270,dog_walk_final$Time_of_Day_Noon,0)

dog_walk_final$last_obs<-ifelse(dog_walk_final$Obs>270,1,0)

#Building intial model 
dog_walk_reg<-lm(Number_Incidents~Walker+Walk_Length+Other_Dogs_in_Area+mailman_yes+Number_of_Squirrels+Fire_Hydrants+Time_of_Day_AM+Time_of_Day_Noon+Time_of_Day_PM,dog_walk_final)
summary(dog_walk_reg)
#plot(dog_walk_reg)

#Removing insignificant variables that are not required for further analysis
dog_walk_reg_2<-lm(Number_Incidents~Walker+Walk_Length+Other_Dogs_in_Area+Number_of_Squirrels+Time_of_Day_AM+Time_of_Day_Noon,dog_walk_final)
summary(dog_walk_reg_2)
#plot(dog_walk_reg_2)


#Q1 a Final Regression Model - Including dummy vairables for the latter half of the tests
dog_walk_reg_final<-lm(Number_Incidents~Walker+Walk_Length+Walk_Length_dummy+Walk_Length_dummy2+Other_Dogs_in_Area+Other_Dogs_in_Area_dummy+Other_Dogs_in_Area_dummy2+Number_of_Squirrels+Number_of_Squirrels_dummy+Number_of_Squirrels_dummy2+Time_of_Day_AM+Time_of_Day_AM_dummy+Time_of_Day_AM_dummy2+Time_of_Day_Noon+Time_of_Day_Noon_dummy+Time_of_Day_Noon_dummy2,dog_walk_final)
summary(dog_walk_reg_final)
plot(dog_walk_reg_final)

#Q1 b Banksy is better behaved (less number of icidents) when alex walks him
#     vs anyone else walks him
#Hypothesis test H0 - Bansy is not better behaved when Alex walks him v/s someone else
#Hypothesis test H1 - Bansy is better behaved when alex walks him v/s someone else
summary(dog_walk_reg_final)

#Q1 c. conducting a chow test to dtermine if there is any difference at 
#      all when someone else walks Bansky
linearHypothesis(dog_walk_reg_final,c("Walk_Length_dummy=0","Other_Dogs_in_Area_dummy=0","Number_of_Squirrels_dummy=0","Time_of_Day_AM_dummy=0","Time_of_Day_Noon_dummy=0"))

#Q1 d. conducting a chow test to determine if there is any difference when
#      i walk Bansky with another person (last 30 obs)
linearHypothesis(dog_walk_reg_final,c("Walk_Length_dummy=0","Other_Dogs_in_Area_dummy=0","Number_of_Squirrels_dummy=0","Time_of_Day_AM_dummy=0","Time_of_Day_Noon_dummy=0","Walk_Length_dummy2=0","Other_Dogs_in_Area_dummy2=0","Number_of_Squirrels_dummy2=0","Time_of_Day_AM_dummy2=0","Time_of_Day_Noon_dummy2=0"))

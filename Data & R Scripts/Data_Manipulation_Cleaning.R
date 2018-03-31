##################################
### Project #2
##################################

#Load the dataset

setwd("C:/Users/Clare/Documents/Spring 2018 Supervised and Unsupervised Learning")

library(tidyverse)
library(readxl)

ela <- read_excel("ELA13-17.xlsx", sheet = 2)


ELA <- as.data.frame(ela)

#To get rid of s for NAs
for(i in 1:23988){
  if(ela$One[i] == "s")
    ela$One[i] <- NA
}
for(i in 1:23988){
  if(ela$Two[i] == "s")
    ela$Two[i] <- NA
}
for(i in 1:23988){
  if(ela$Three[i] == "s")
    ela$Three[i] <- NA
}
for(i in 1:23988){
  if(ela$Four[i] == "s")
    ela$Four[i] <- NA
}
ela$Three <- as.numeric(ela$Three)
ela$Two <- as.numeric(ela$Two)
ela$One <- as.numeric(ela$One)
ela$Four <- as.numeric(ela$Four)



#To get the 2013 grades
ela.13<- ela %>%
  filter(Year == 2013)

ela.13 <- ela.13 %>%
  mutate(Year1 = Year)
ela.13 <- ela.13 %>%
  mutate(Year = NULL)
ela.13 <- ela.13 %>%
  mutate("One(%)"= NULL)
ela.13 <- ela.13 %>%
  mutate("Two(%)"= NULL)
ela.13 <- ela.13 %>%
  mutate("Three(%)"= NULL)
ela.13 <- ela.13 %>%
  mutate("Four(%)"= NULL)
ela.13 <- ela.13 %>%
  mutate("Three.Four(%)"= NULL)
names.2013 <- c("X__1","DBN","School Name","Grade","Category","Number Tested 2013","Mean Scale Score 2013","One 2013","Two 2013","Three 2013","Four 2013","Three.Four 2013","Year1")
colnames(ela.13) <- names.2013

#To get the 2014 grades
ela.14 <- ela %>%
  filter(Year == 2014)
ela.14 <- ela.14 %>%
  mutate(Year2 = Year)
ela.14 <- ela.14 %>%
  mutate(Year = NULL)
ela.14 <- ela.14 %>%
  mutate("One(%)"= NULL)
ela.14 <- ela.14 %>%
  mutate("Two(%)"= NULL)
ela.14 <- ela.14 %>%
  mutate("Three(%)"= NULL)
ela.14 <- ela.14 %>%
  mutate("Four(%)"= NULL)
ela.14 <- ela.14 %>%
  mutate("Three.Four(%)"= NULL)

names.2014 <- c("X__1","DBN","School Name","Grade","Category","Number Tested 2014","Mean Scale Score 2014","One 2014","Two 2014","Three 2014","Four 2014","Three.Four 2014","Year2")
colnames(ela.14) <- names.2014

#To get the 2015 grades
ela.15 <- ela %>%
  filter(Year == 2015)
ela.15 <- ela.15 %>%
  mutate(Year3 = Year)
ela.15 <- ela.15 %>%
  mutate(Year = NULL)
ela.15 <- ela.15 %>%
  mutate("One(%)"= NULL)
ela.15 <- ela.15 %>%
  mutate("Two(%)"= NULL)
ela.15 <- ela.15 %>%
  mutate("Three(%)"= NULL)
ela.15 <- ela.15 %>%
  mutate("Four(%)"= NULL)
ela.15 <- ela.15 %>%
  mutate("Three.Four(%)"= NULL)
names.2015 <- c("X__1","DBN","School Name","Grade","Category","Number Tested 2015","Mean Scale Score 2015","One 2015","Two 2015","Three 2015","Four 2015","Three.Four 2015","Year3")
colnames(ela.15) <- names.2015

#To get the 2016 grades
ela.16 <- ela %>%
  filter(Year == 2016)
ela.16 <- ela.16 %>%
  mutate(Year4 = Year)
ela.16 <- ela.16 %>%
  mutate(Year = NULL)
ela.16 <- ela.16 %>%
  mutate("One(%)"= NULL)
ela.16 <- ela.16 %>%
  mutate("Two(%)"= NULL)
ela.16 <- ela.16 %>%
  mutate("Three(%)"= NULL)
ela.16 <- ela.16 %>%
  mutate("Four(%)"= NULL)
ela.16 <- ela.16 %>%
  mutate("Three.Four(%)"= NULL)
names.2016 <- c("X__1","DBN","School Name","Grade","Category","Number Tested 2016","Mean Scale Score 2016","One 2016","Two 2016","Three 2016","Four 2016","Three.Four 2016","Year4")
colnames(ela.16) <- names.2016

#To get the 2017 grades
ela.17 <- ela %>%
  filter(Year == 2017)
ela.17 <- ela.17 %>%
  mutate(Year5 = Year)
ela.17 <- ela.17 %>%
  mutate(Year = NULL)
ela.17 <- ela.17 %>%
  mutate("One(%)"= NULL)
ela.17 <- ela.17 %>%
  mutate("Two(%)"= NULL)
ela.17 <- ela.17 %>%
  mutate("Three(%)"= NULL)
ela.17 <- ela.17 %>%
  mutate("Four(%)"= NULL)
ela.17 <- ela.17 %>%
  mutate("Three.Four(%)"= NULL)
names.2017 <- c("X__1","DBN","School Name","Grade","Category","Number Tested 2017","Mean Scale Score 2017","One 2017","Two 2017","Three 2017","Four 2017","Three.Four 2017","Year5")
colnames(ela.17) <- names.2017

#Now to join the datasets
ela.main <- full_join(ela.13, ela.14, by = c("DBN","School Name", "Grade","Category"))
ela.main <- full_join(ela.main, ela.15, by = c("DBN","School Name", "Grade","Category"))
ela.main <- full_join(ela.main, ela.16, by = c("DBN","School Name", "Grade","Category"))
ela.main <- full_join(ela.main, ela.17, by = c("DBN","School Name", "Grade","Category"))

#now to just get rid of the X__1's which are specific to each school for each year for each grade
ela.main <- ela.main %>%
  mutate(X__1 = NULL, X__1.x = NULL, X__1.y = NULL, X__1.x.x = NULL, X__1.y.y = NULL)
#EXPORT AND SAVE

write_excel_csv(ela.main, "C:/Users/Clare/Documents/Spring 2018 Supervised and Unsupervised Learning/elaYEAR.csv")



readstata13
#In stata
#insheet using in stata "csv" , comma
#library(readstata13)
#save.dta13(ela.main, file="ela.dta")


#For 5th grade only
ela.5th <- ela.main %>%
  filter(Grade == 5)



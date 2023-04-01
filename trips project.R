# today I will be analyzing an uber dataset to come up with meaningful analysis
# the first step is to import the libraries that we will be using for the analysis


# install packages
install.packages('tidyverse')
install.packages('lubridate')
install.packages('scales')
install.packages('ggthemes')
install.packages('DT')
install.packages('tidyr')
install.packages('ggplot2')
install.packages('tidyr')

# load the packages
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(DT)
library(tidyr)
library(ggplot2)
library(dplyr)


# let us import datasets in R
Apr_2014<-read.csv("C:/Users/Dell E5470/Desktop/uber dataset/uber-raw-data-apr14.csv")
May_2014<-read.csv("C:/Users/Dell E5470/Desktop/uber dataset/uber-raw-data-may14.csv")
Jun_2014<-read.csv("C:/Users/Dell E5470/Desktop/uber dataset/uber-raw-data-jun14.csv")
Jul_2014<-read.csv("C:/Users/Dell E5470/Desktop/uber dataset/uber-raw-data-jul14.csv")
Aug_2014<-read.csv("C:/Users/Dell E5470/Desktop/uber dataset/uber-raw-data-aug14.csv")
Sep_2014<- read.csv("C:/Users/Dell E5470/Desktop/uber dataset/uber-raw-data-sep14.csv") 
  

# let us confirm that the data is imported
view(Apr_2014)
view(May_2014)
head
view(Jun_2014)
view(Jul_2014)
view(Aug_2014)
view(Sep_2014)


# let us combine all the data into one table/dataframe

data_2014<- rbind(Apr_2014, May_2014, Jun_2014, Jul_2014, Aug_2014, Sep_2014)

# let us check the data
view(data_2014)
head(data_2014)
str(data_2014)
summary(data_2014)

# let us convert the datetime character to datetime object
data_2014$Date.Time<-as.POSIXct(data_2014$Date.Time, format="%m/%d/%Y %H:%M:%S")

summary(data_2014)

#extracting time

data_2014$Time<- format(as.POSIXct(data_2014$Date.Time, format="%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")

data_2014$Time

# lets us extract day
data_2014$day<- format(day(data_2014$Date.Time))

data_2014$month<-format(month(data_2014$Date.Time, label = TRUE)) #month
data_2014$year<- format(year(data_2014$Date.Time)) #year
data_2014$dayofweek<-format(wday(data_2014$Date.Time, label = TRUE)) #day of week

# let us get the hour minute and seconds
data_2014$hour<- factor(hour(hms(data_2014$Time)))
data_2014$minute<- factor(minute(hms(data_2014$Time)))
data_2014$second<- factor(second(hms(data_2014$Time)))

head(data_2014)

# let us plot the trip by hours in a day
hour_data<- data_2014 %>%
  group_by(hour) %>%
  summarise(Total=n())

#visualize the data in a table
datatable(hour_data)

# let us graph it
ggplot(hour_data, aes(hour, Total))+
  geom_bar(stat="identity" ,fill="black", color="red")+
  ggtitle("Trips By Hour")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

# let us plot the trip by hours and month
month_hour_data<- data_2014 %>%
  group_by(hour, month) %>%
  summarise(Total=n())

datatable(month_hour_data)

ggplot(month_hour_data, aes(hour, Total, fill=month))+
  geom_bar(stat="identity" )+
  ggtitle("Trips By Month and  Hour")+
  scale_y_continuous(labels = comma)


# let us look into the month of september
sept_data<- data_2014 %>%
  group_by(hour, month) %>%
  filter(month=="Sep") %>%
  summarise(Total=n())

ggplot(sept_data, aes(hour, Total, fill=hour))+
  geom_bar(stat="identity" )+
  ggtitle("Trips By September and  Hours")+
  scale_y_continuous(labels = comma)


# let us group the data by day
day_data<- data_2014 %>%
  group_by(day) %>%
  summarise(Total=n())

#visualize the data in a table
datatable(day_data)

ggplot(day_data, aes(day, Total))+
  geom_bar(stat="identity" ,fill="black", color="red")+
  ggtitle("Trips By Day")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)


# let us group by month and day
month_day_data<- data_2014 %>%
  group_by(month, day) %>%
  summarise(Total=n())

ggplot(month_day_data, aes(day, Total, fill=month))+
  geom_bar(stat="identity" )+
  ggtitle("Trips By Month and Day")+
  scale_y_continuous(labels = comma)


# let us find which day in september has the highest data
sept_day<- data_2014 %>%
  group_by(day, month) %>%
  filter(month=="Sep") %>%
  summarise(Total=n())

ggplot(sept_day, aes(day, Total, fill=day))+
  geom_bar(stat="identity" )+
  ggtitle("Trips By September-days")+
  scale_y_continuous(labels = comma)


# month trend

month_data<- data_2014 %>%
  group_by( month) %>%
  summarise(Total=n())

datatable(month_data)

ggplot(month_data, aes(month, Total, fill=month))+
  geom_bar(stat="identity" )+
  ggtitle("Trips By Months")+
  scale_y_continuous(labels = comma)


# month_weekday_data
month_weekday_data<- data_2014 %>%
  group_by( month, dayofweek) %>%
  summarise(Total=n())

datatable(month_weekday_data)


ggplot(month_weekday_data, aes(month, Total, fill=dayofweek))+
  geom_bar(stat="identity" )+
  ggtitle("Trips By Months and Weekday")+
  scale_y_continuous(labels = comma)


# weekday
weekday_data<- data_2014 %>%
  group_by(dayofweek) %>%
  summarise(Total=n())

datatable(weekday_data)


ggplot(weekday_data, aes(dayofweek, Total, fill=dayofweek))+
  geom_bar(stat="identity" )+
  ggtitle("Trips By Weekday")+
  scale_y_continuous(labels = comma)

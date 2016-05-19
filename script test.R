setwd("C:\\Users\\Christian\\Desktop\\Reproducible Research\\Project 1 week 2")


library(ggplot2)
library(lattice)
library(dplyr)
data<-read.csv("activity.csv" )      #Reading the data file
data[,2]<-as.Date(data[,2])          # Change the to Dates
data$Day<-as.factor(weekdays(data[,2]))  # Add the day of the Week 
data$Day<-factor(data$Day,              #Change the order of the factor
                 levels=c("Sunday" , "Monday","Tuesday",   
                          "Wednesday","Thursday", "Friday", "Saturday"))
weekend<-subset(data, (Day== "Saturday"| Day=="SUnday"))   #Filter by Weekend 
weekday<-subset(data, (Day!= "Saturday" & Day!="SUnday"))  #Filter by Weekdays 
weekend[,5]<-as.factor(rep( "Weekend", time=length(weekend[,4])))
weekday[,5]<-as.factor(rep( "Weekday", time=length(weekday[,4])))
data<-rbind(weekday, weekend)                             #Add the Filter 
names(data)<-c("Steps","Date","Interval","Day","Weekday/Weekend")
summary(data)      

histogram(Day~Steps, data)
with(data, hist(Steps, col="green", main="Histogram of steps per 5min interval"))

StepsT<-as.matrix(summarise(group_by(data,Date), sum(Steps)))
Total_Steps<-na.omit(as.numeric(StepsT[,2]))

hist(Total_Steps,col= "green",
     xlab= "Total Number of Steps per Day", 
     main= "Histogram of total number of Steps per day")
rug(Total_Steps)




StepsMeanMedian<-na.omit(as.data.frame(summarise(group_by(data,Date), mean(Steps), median(Steps))))
colnames(StepsMeanMedian)<-c("Days", "Mean", "Median") 
StepsMeanMedian


level<-levels(data$Day)         
Summary_Table <-matrix(0, 7,2)            #Create Table
rownames( Summary_Table)<-level          #Name Row
colnames( Summary_Table)<-c("Mean", "Median") #Name Col
for(i in 1:7){       #Calculate Mean and Median of Steps pet each Day
  Summary_Table[i,1]<-with(subset(data, Day== level[i]), mean(Steps, na.rm = TRUE))
  Summary_Table[i,2]<-with(subset(data, Day== level[i]),median(Steps, na.rm = TRUE))}
Summary_Table
 


StepsAverage<-as.data.frame(summarise(group_by(data,Interval), mean(Steps, na.rm = TRUE)))
colnames(StepsAverage)<-c("Interval", "Average") 
xyplot(Average~Interval, StepsAverage, type="l", lwd=4,
       ylab= "Average number of steps",
       main= " Time Sereies plot of the average number of steps per 5min interval")

Max5<-subset(StepsAverage, StepsAverage[,2]==max(StepsAverage[,2]))

sum(is.na(data))


head(data)



NAAverage<-as.data.frame(summarise            #Posible values to replace NA's
           (group_by(data,Interval,Day ),      #Subset by Intervals and Day of the Week
            mean(Steps, na.rm = TRUE)))       #Calcualte mean

dataNoNA<-subset(data, is.na(Steps) == FALSE)  #Subset data without NA values
dataNA<-subset(data, is.na(Steps) != FALSE)    #Subset data with NA values

for(i in 1:(length(dataNA[,1]))){
  dataNA[i,1]<- subset(NAAverage,     #Replace NA values by average steps acrros
                       Interval== dataNA[i,3] &  #Interval
                         Day== dataNA[i,4])[,3]   # And days of the week
  
}

dataImputed<-rbind(dataNoNA,dataNA)



StepsT2<-as.matrix(summarise
                   (group_by(dataImputed,Date),    #Subset the data by Date
                   sum(Steps)))             #Calcualte Total number of steps
Total_Steps2<-na.omit(as.numeric(StepsT[,2])) #Eliminate NA values

par(mfrow=c(1,2))
hist(Total_Steps2,col= "green",               #plot Histogram
     xlab= "Total Number of Steps per Day", 
     main= "Imputed Data")
rug(Total_Steps2)
abline(v= (mean(Total_Steps2)), lwd=4, col="blue")



hist(Total_Steps,col= "green",               #plot Histogram
     xlab= "Total Number of Steps per Day", 
     main= "Original Data")
rug(Total_Steps)




StepsMeanMedian2<-na.omit(as.data.frame(summarise                 
                                        (group_by(dataImputed,Date), #Subset the data by Date
                                        mean(Steps),          #Calcualte mean
                                        median(Steps))))     #Calculate Median
colnames(StepsMeanMedian2)<-c("Days", "Mean", "Median")      #Name the colums
StepsMeanMedian2





StepsAverage2<-as.data.frame(summarise
                            (group_by(data,Interval, data[, 5]),      #Subset by Intervals
                            mean(Steps, na.rm = TRUE))) #Calcualte the Mean without NA
colnames(StepsAverage2)<-c("Interval", "Weekday_Weekend", "Average") 
xyplot(Average~Interval| Weekday_Weekend, StepsAverage2, type="l", lwd=4,    #Make PLot
       ylab= "Average number of steps",
       main= " Time Sereies plot of the average number of steps per 5min interval")
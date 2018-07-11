airquality
complete.cases(airquality)
airquality[complete.cases(airquality),]
complete_airquality<-airquality[complete.cases(airquality),]
complete_airquality
length(complete_airquality$Ozone)
summary(airquality)
str(airquality)



#imputation of mean


mean_airquality<-mean(airquality$Ozone,na.rm=TRUE)
mean_airquality
airquality$ozone[is.na(airquality$Ozone)]<-mean_airquality
airquality$Ozone

a<-airquality$Solar.R
b<-mean(a,na.rm=TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)]<-b
airquality$Solar.R                                 



#repreasent dates and time

(x<-Sys.time())
class(x)
(y<-Sys.Date())
class(y)
weekdays(x)
months(x)
quarters(x)

d<-"2010-03-15"
class(d)

weekdays(d) #d is a character hence we are not able to find weekdays

#hence we convert d into date 

d<-as.Date("2010-03-15")
d
weekdays(d)
months(d)
class(d)
mode(d)


as.integer(d)  #it shows the day since 1jan1970(origin of unix)
julian(d)

d<-as.Date("1970-01-02")
d
as.integer(d)
julian(d)


d<-as.Date("12/31/2010",format = "%m/%d/%Y") #convert of formate
d
unclass(d) # it shows date in numeric format"


x<-Sys.time()
x
class(x)
unclass(x) #numbervof seconds that is passed from 1jan1970 


x<-as.Date.POSIXlt(x) #wrong
x

y<-as.POSIXlt(x)
y
class(y)
unclass(y)

names(unclass(y))
y$wday
y$zone
y$hour
x$wday #it shows error becoz it is in the date fotmat not in the list fotmat 
class(x)
mode(y)


1.
setwd("C:\\Documents")
flt<-read.csv("Fd.csv",stringsAsFactors = FALSE)
str(flt)
flt$FlightDate<-as.Date(flt$FlightDate,"%d-%b-%y")
head(flt$FlightDate)
class(flt$FlightDate)

unique(weekdays(flt$FlightDate))
unique(months(flt$FlightDate))
difftime(flt$FlightDate[1000],flt$FlightDate[1],units = "days")
difftime(flt$FlightDate[1000],flt$FlightDate[1],units = "weeks")
difftime(flt$FlightDate[1000],flt$FlightDate[1],units = "hours")

#auto is a day
difftime(flt$FlightDate[1000],flt$FlightDate[1],units = "auto")

#find all the airline id which operate on sundays
unique(flt$AirlineID[weekdays(flt$FlightDate)=="Sunday"])
or
unique(flt[weekdays(flt$FlightDate)=="Sunday","AirlineID"])
or
library(dplyr)
unique(select(filter(flt,weekdays(flt$FlightDate)=="Sunday"),AirlineID))
or
flt%>%filter(weekdays(FlightDate)=="Sunday")%>%select(AirlineID)%>%unique()

#find no of flight operating on sunday
flt%>%filter(weekdays(FlightDate)=="Sunday")%>%nrow()

x<-as.Date("2016-02-28")
x<-as.Date("2016-02-29")
x<-as.Date("2016-02-30")

#find out the number of flights by a carrier
flt%>%group_by(Carrier)%>%select(AirlineID)%>%count()
or
flt%>%group_by(Carrier)%>%count(AirlineID)

#find out the no of flights by a carrier fot every weekdays

flt%>% filter(weekdays(flt$FlightDate)!="Saturday"& weekdays(flt$FlightDate)!="Sunday")%>% group_by(Carrier)%>%count()
 

#find the number of flights on sundays for destination atlanta
flt %>% filter(weekdays(flt$FlightDate)=="Sunday" & Dest=="ATL") %>% count()


2.
setwd("C:\\Users\\Administrator\\Desktop\\New folder")
flt1<-read.csv("FlightDelays-1.csv",stringsAsFactors = FALSE)
flt1
unique(flt1$date)

#1. Find out the number of delayed flights for all weekdays
flt1$date <- as.Date(flt1$date)
flt1 %>% filter(weekdays(date)!=c("Saturday","Sunday"))%>%filter(delay=="delayed")%>%count()
colnames(flt1)                
head(flt1)

#2. Find the average distance, total distance and count for all delayed flights on Friday.
flt1 %>% filter(weekdays(date)=="Friday" & delay=="delayed")%>%summarise(mean(distance),sum(distance),n())

#3. Find out how many flights were on time on Week days and Weekends (Consider Saturday and Sunday as weekends)
flt1%>%filter(weekdays(date)& delay=="ontime")%>%count()

#4. Find out the number of flights for each destination across all weekdays
flt1%>%group_by("dest")%>%filter(weekdays(date)!=c("Saturday","Sunday"))%>%count()
              
                               
#5. Find out the number of times weather was bad across all weekdays. (1 indicates bad weather)
flt1%>%filter(weekdays(date)!=c("Saturday","Sunday"))%>%filter(weather=="1")%>%count()

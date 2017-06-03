###Setup###
install.packages("rmarkdown")
install.packages("devtools")
install.packages("htmltools")
install.packages("httpuv")
install.packages("xtable")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install_github("StatsWithR/statsr")

library(rmarkdown)
library(devtools)
library(dplyr)
library(ggplot2)
library(statsr)
library(lubridate)
###################################################

###Download Data###
data(nycflights)
#Description of each var: https://rpubs.com/maulikpatel/212127
#Other possible helpful dataset: #Link: https://cran.r-project.org/web/packages/nycflights13/nycflights13.pdf

###View the dataset###
#view all varirables. 16 var in total
names(nycflights)
# [1] "year"      "month"     "day"       "dep_time"  "dep_delay" "arr_time"  "arr_delay" "carrier"  
# [9] "tailnum"   "flight"    "origin"    "dest"      "air_time"  "distance"  "hour"      "minute"   

#dplyr approach to display the data in table structure (rows, columns).
mydata <- tbl_df(nycflights)

#convert tp a normal dataframe to see all of the columns
data.frame(head(mydata))

###Summary###
summary(nycflights)
#Note: Month, day, dep_time,and arr_time were equally distributed.

### Distribution of each var###
hist(nycflights$year,freq = TRUE)
hist(nycflights$month,freq = TRUE)
hist(nycflights$day, freq = TRUE)
hist(nycflights$dep_time, freq = TRUE)
hist(nycflights$arr_time, freq = TRUE)

# dep_delay,arr_delay Departure and arrival delays, in minutes. Negative times represent early
# departures/arrivals.

# dep_delay
hist(nycflights$dep_delay, freq = TRUE, xlim= c(-100,1400))
# Note: min=-21 Max=1301

hist(nycflights$arr_delay, freq = TRUE)
# Note: min=-73 max=1272

#If the data is character, use the following code to make hist of it
counts <- table(nycflights$carrier)
barplot(counts)
tbl_df(sort(counts))
# OO[SkyWest Airlines] has least record
# UA[United AirLines]has the largest flights

hist(nycflights$air_time, freq = TRUE)
hist(nycflights$distance,freq = TRUE)
plot(nycflights$air_time, nycflights$distance)

### Analyze the days of week: The goal is to add the col of MTWThF in the data set
mydata<- mutate(mydata, weekday= paste(year,month,day))
data.frame(head(mydata))

as.Date(mydata$weekday, format= "%Y %m %d")
format(mydata$weekday, format= "%A")

weekday2 <- as.Date(mydata$weekday, format= "%Y %m %d")
format(weekday2, format= "%A")

### Try The lubridate package is great for this sort of stuff.
# https://stackoverflow.com/questions/11985799/converting-date-to-a-day-of-week-in-r

#histgram of weekday
wkd<- wday(weekday2) # wkd = weekday
hist(wkd,las=1)
#Result_Note:  Saturday[7] has least flight counts

# Creat a new dataset = "mydata_wkday" which includes weekday data 
mydata_wkday<- cbind(mydata,wkd)
data.frame(head(mydata_wkday))


###Selection based on the matching criteria###
mydata_wkday %>% 
  select(carrier, dep_delay) %>% 
  filter( carrier=="UA",  dep_delay > 240) %>%
  arrange(dep_delay) 
  
    
# Select UA and select "dep_delay" column    
mydata_wkday %>% 
  filter(carrier=="UA", dep_delay > 240) %>% 
   select(dep_delay) %>% 
    hist()
#???  hist.default(.) : 'x' must be numeric      

mydata_wkday %>% 
  filter(carrier=="UA") %>% 
   select(dep_delay, dest)

mydata_wkday %>% 
  filter(carrier=="UA", dest=="SFO") %>% 
    select(dep_delay, distance, origin, dest)

### summarise: Reduce variables to values
# group_by creates groups that will be operated on
# summarise uses the provided aggregation function to summarise each group

# Average delay time of each airline
mydata_wkday %>%
  group_by(carrier) %>%
  summarise(avg_delay = mean(arr_delay, na.rm= TRUE)) %>%
  arrange (avg_delay)

#  Remove the early arrival data, and then find the average of the delay time
mydata_wkday %>%
  group_by(carrier) %>%
  filter(arr_delay >= 0) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm= TRUE)) %>% 
  arrange (avg_delay)

#  Remove the early arrival data, and then find the average of the delay time for each destination
mydata_wkday %>%
  group_by(dest) %>%
  filter(arr_delay >= 0) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm= TRUE)) %>% 
  arrange (avg_delay) %>%
  print(,n=101)

mydata_wkday %>% 
  select(origin, dest, distance ,arr_delay) %>% 
  filter(dest=="ANC"|dest=="BHM"|dest=="EGE") %>% 
  arrange(dest)
  #Note: Distance seems not to be the main factor causing arrival delay

# flight counts for each airline
mydata_wkday %>% 
  group_by(carrier) %>% 
  summarise(flight_count= n()) %>% 
  arrange(desc(carrier))  
  

#Delay counts for each airline
mydata_wkday %>% 
  group_by(carrier) %>% 
  filter(arr_delay >= 0) %>% 
  summarise(arr_delay_count= n()) %>% 
  arrange(desc(carrier)) 
  
### I want to cal the percentage of the (arr_deply) / (flight count) for each airline ????

### summarise_each: operate mutiple functions on mutiple columns
mydata_wkday %>% 
  group_by(carrier) %>% 
  summarise_each(funs(min(.,na.rm= TRUE), max(., na.rm=TRUE)), matches("delay"),distance)
   

1234

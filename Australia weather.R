install.packages("tidyverse")
install.packages("lubridate")
install.packages("broom", type="binary")
library(lubridate)
library(tidyverse)
library(readxl)
library(readr)
library(ggthemes)
urlfile <- "https://raw.githubusercontent.com/AbiVavilala/Australian-Weather-Report/main/weatherAUS.csv"

weather <- read.csv(url(urlfile))
View(weather)
#maximum temperature in Australia
max(!is.na(weather$MaxTemp))
is.na(weather$MaxTemp)
str(weather)
max(weather$MaxTemp)
?is.na()
c <-  na.omit(weather$MaxTemp)
weather$Date <- as.Date(weather$Date)
weather$year <- year(weather$Date)
weather$month <- months(weather$Date)
weather$month <- factor(weather$month, levels = month.name)
weather$temp_interval <- cut(weather$MaxTemp, breaks = seq(0,50, by = 3))
sydney_weather$temp_interval <- cut(sydney_weather$MaxTemp, breaks = seq(0,50, by = 3))
unique(weather$Location)

!is.na()
c <- omit
View(c)
max(weather$Date)
max(c)
min(weather$year)
max(weather$year)
weather %>% filter(Location == "Albury") %>% ggplot(aes(MaxTemp, MinTemp)) + geom_point()

mean(c)
str(weather)
weather %>% ggplot(aes(Location, color = Location, fill = Location)) + geom_bar()

unique(weather$Location)

sydney_weather <- weather %>% filter(Location == "Sydney") 
View(sydney_weather)
sydney_weather

#maximum temperature in Sydney from 2008 to 2017
max(sydney_weather$MaxTemp, na.rm = T)


sydney_weather %>% filter(MaxTemp == 45.8)

# we will look at the relationship between Mintemp and Maxtemp
sydney_weather %>% ggplot(aes(MaxTemp, MinTemp)) + geom_point() 

min(sydney_weather$Date)
max(sydney_weather$Date)
str(sydney_weather)

sydney_weather %>% filter(Date < "2009-01-01") %>% ggplot(aes(MaxTemp, MinTemp)) + geom_point() + 
  ggtitle("Temperature in Sydney for year 2008")

sydney_weather %>% filter(Date < "2010-01-01") %>% ggplot(aes(MaxTemp, MinTemp)) + geom_point() + 
  ggtitle("Temperature in Sydney for year 2009")


sydney_weather %>% filter(Date < "2011-01-01") %>% ggplot(aes(MaxTemp, MinTemp)) + geom_point() + 
  ggtitle("Temperature in Sydney for year 2010")
 


sydney_weather$Date <- as.Date(sydney_weather$Date)
str(sydney_weather)
library(lubridate)
sydney_weather$year <- year(sydney_weather$Date)
View(sydney_weather)

sydney_weather  %>% ggplot(aes(MaxTemp, MinTemp)) + geom_point() + facet_grid(.~year)

sydney_weather %>% ggplot(aes(MaxTemp, color = year, fill = year)) + geom_bar()

sydney_weather %>% filter(month == "December") %>% ggplot(aes(MaxTemp, MinTemp)) + geom_point() + facet_grid(.~year) + 
  ggtitle("Min and Maximum temperature in the month of December from 2008 to 2016 in Sydney")
install.packages("ggthemes")
library(ggthemes)
max(na.omit(weather$MaxTemp))


weather %>% filter(MaxTemp == 48.1)



s_2010 <- weather %>% filter(Location == "Woomera" & month == "January" & year == "2009") %>% mean(MaxTemp, na.rm =T)

str(weather)

mean(weather$MaxTemp, na.rm =T)

max(weather$MaxTemp, na.rm = T)

fun <- lapply(filter mean)

syd_2008 <- sydney_weather %>% filter(sydney_weather$year == "2008") 

syd_2009 <- sydney_weather %>% filter(sydney_weather$year == "2009") 

syd_2010 <- sydney_weather %>% filter(sydney_weather$year == "2010") 

syd_2011 <- sydney_weather %>% filter(sydney_weather$year == "2011")


syd_2012 <- sydney_weather %>% filter(sydney_weather$year == "2012")

syd_2013 <- sydney_weather %>% filter(sydney_weather$year == "2013")

syd_2014 <- sydney_weather %>% filter(sydney_weather$year == "2014")

syd_2015 <- sydney_weather %>% filter(sydney_weather$year == "2015")

syd_2016 <- sydney_weather %>% filter(sydney_weather$year == "2016")


syd_2017 <- sydney_weather %>% filter(sydney_weather$year == "2017")

mean(syd_2008$MaxTemp, na.rm = T)
mean(syd_2009$MaxTemp, na.rm = T)
mean(syd_2010$MaxTemp, na.rm = T)
mean(syd_2011$MaxTemp, na.rm = T)
mean(syd_2012$MaxTemp, na.rm = T)
mean(syd_2013$MaxTemp, na.rm = T)
mean(syd_2014$MaxTemp, na.rm = T)
mean(syd_2015$MaxTemp, na.rm = T)
mean(syd_2017$MaxTemp, na.rm = T)
mean(syd_2016$MaxTemp, na.rm = T)


melbourne_weather <- weather %>% filter(Location == "Melbourne") 
View(melbourne_weather)
melbourne_weather %>% ggplot(aes(MaxTemp, MinTemp, color = year)) + geom_point() + facet_grid(.~year) + ggtitle("weather for melbourne every year")
storage <- numeric(5)
for(i in 1:5){
  storage[i]<- i^2
}




sydney_weather %>% filter(year == "2016") %>% ggplot(aes(MaxTemp, fill = month)) + geom_histogram()


max(weather$MaxTemp, na.rm = T)



max(sydney_weather$MaxTemp, na.rm = T)

sydney_weather %>% ggplot(aes(Temp9am, Temp3pm)) + geom_point() + facet_grid(.~year)
min(sydney_weather$MinTemp, na.rm = T)

# we will see what's the most common temperature repeated in sydney
sort(table(sydney_weather$MaxTemp), decreasing = T)[1:3]
sydney_weather$month <- factor(sydney_weather$month, levels = month.name)
 

getwd()


str(sydney_weather$MaxTemp)



sydney_weather$MaxTemp <- as.factor(sydney_weather$MaxTemp)

as.numeric(sydney_weather$MaxTemp)
lappl

sydney_weather
sappl

?apply




syd_2010 %>% ggplot(aes(temp_in, fill = month)) + geom_histogram(binwidth = 0.5, stat = "count") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



syd_2010$month <- as.factor(syd_2010$month, levels = month.name)

#let's divide temperature into interval. this will give us good idea about temp pattern

sydney_weather$temp_interval <- cut(sydney_weather$MaxTemp, breaks = seq(0,50, by = 3))


View(sydney_weather)


sydney_weather$MaxTemp <- as.numeric(sydney_weather$MaxTemp)

#let's make a data table with temp from 2013 to 2017
sydney_weather_2013_2017 <- sydney_weather %>% filter(year %in% c("2013", "2014", "2015", "2016", "2017"))

View(sydney_weather_2008_2012)

sydney_weather_2013_2017 %>% ggplot(aes(temp_interval, fill = temp_interval)) + geom_bar()

unique(sydney_weather$year)

sydne


s <- c(24, 27]


typeof(sydney_weather$year)


#sydney temp from year 2008 to 2012
sydney_weather_2008_2012 <- sydney_weather %>% filter(year %in% c("2008", "2009", "2010", "2011", "2012"))
sydney_weather_2008_2012
View(sydney_weather_2008_2012)
sydney_weather_2008_2012 %>% ggplot(aes(temp_interval, fill = temp_interval)) + geom_bar() + ggtitle("temp recorded in Sydney from 2008 to 2012")


sydney_weather_2013_2017 %>% ggplot(aes(temp_interval, fill = temp_interval)) + geom_bar() + ggtitle("temp recorded in sydney from 2013 to 2017")

is.recursive(sydney_weather)
sydney



View(weather)
str(weather$month)

# lets convert month into a factor with levels

weather$month <- factor(weather$month, levels = month.name)


# lets see max temp in Australia for every month

weather %>%  ggplot(aes(MaxTemp, MinTemp, color = month)) + geom_point() + facet_grid(.~month) + ggtitle("Australian Temperature fromm 2008 to 2017")

#lets see tempearure in sydney for every month

sydney_weather$month <- factor(sydney_weather$month, levels = month.name)

sydney_weather %>% ggplot(aes(MaxTemp, MinTemp, color = month)) + geom_point() + facet_grid(.~month) + ggtitle("Sydney Temperature fromm 2008 to 2017 for each month")

mean(syd_2008$Rainfall)




#lets look at weather report for 5 Major cities in Australia 

weather_cities <- weather %>% filter(weather$Location %in% c( "Adelaide", "Sydney", "Melbourne", "Perth", "Brisbane"))

View(weather_cities)
 

weather_cities %>% ggplot(aes(MaxTemp, MinTemp, color = Location)) + geom_point() + facet_grid(.~Location)

rain_in_australia <- weather %>% filter(Rainfall > 0)
View(rain_in_australia)


rain_in_syd <- rain_in_australia %>% filter(Location == "Sydney")
View(rain_in_syd)

max(rain_in_syd$Rainfall)

rain_in_syd %>% filter(Rainfall == 119.4)


rain_in_syd %>% ggplot(aes(Rainfall)) + geom_histogram()

summary(sydney_weather)

sydney_weather %>% ggplot(aes(MaxTemp, MinTemp, color = year)) + geom_point()


weather %>% filter(Location == "Sydney" & year == 2009) %>% ggplot(aes(MaxTemp, MinTemp)) + geom_point() 



str(unique(weather$year))
str(weather$year)

library(tidyverse)
library(plotly)
library(lubridate)
library(pracma)

rm(list=ls())
mydata <- read_csv("../db/S_CEP.csv") %>% 
  select(JD,StarName,Band,Magnitude,ValidationFlag) %>%
  mutate( JD = as.numeric(JD)) %>%
  # mutate(Julian = round(JD)) %>%
  mutate(Ymd =insol::JD(JD,inverse = TRUE)) %>%
  mutate(Ymd = as.Date(Ymd))  %>%
  mutate(Julian = as.integer(JD)) %>%
  mutate(Julian = as_factor(Julian)) %>%
  mutate(Magnitude = as.numeric( gsub("<","", Magnitude) ) + 0.99) %>%
    filter(Band=="Vis." ) 

mydata$StarName <- "S_CEP"
mydata$ValidationFlag <-ifelse(mydata$ValidationFlag=="U","Not VAlidated",mydata$ValidationFlag)
mydata$ValidationFlag <-ifelse(mydata$ValidationFlag=="Z","pre-validation",mydata$ValidationFlag)
mydata$ValidationFlag <-ifelse(mydata$ValidationFlag=="V","Validated",mydata$ValidationFlag)
mydata$ValidationFlag <-ifelse(mydata$ValidationFlag=="T","Failed",mydata$ValidationFlag)


mydata %>% group_by(StarName) %>% summarise(n())
mydata %>% group_by(Band) %>% summarise(n())
mydata %>% group_by(ValidationFlag) %>% summarise(n())

## PLot of Validation Flags
##
plot_ly(mydata,x=~Ymd,y=~Magnitude,type="scatter",mode="markers",symbol=~ValidationFlag,colors="Set1") %>%
  layout(yaxis = list(autorange = "reversed")) %>%
  layout(title = "Plot of Observation by Validation Flags ")

## By Julian Date

plot_ly(mydata,x=~Julian,y=~Magnitude,type="scatter",mode="markers",symbol=~ValidationFlag,colors="Set1") %>%
  layout(yaxis = list(autorange = "reversed")) %>%
  layout(title = "Plot of Observation by Validation Flags ")


starDust <- mydata %>%
    group_by(Ymd) %>%
    summarize(Mean = round(mean(Magnitude),digits=1),
              Brighter = round(Mean -1,digits = 1),
              Fainter = round(Mean +1,digits= 1),
              Obs = n()) %>% 
    ungroup() %>% 
    mutate(Verify = case_when(
      (Brighter <  Mean -1) | (Fainter > Mean + 1) ~ "Yes",TRUE ~ "No")) %>%
      filter(Obs >=3 & Verify=="Yes") 

starDust %>% View()
#
## Table of questionable Observations by Julian Date.
stardust.jd <- mydata %>%
  group_by(Julian) %>%
  summarize(Mean = round(mean(Magnitude),digits=1),
            Brighter = round(Mean -1,digits = 1),
            Fainter = round(Mean +1,digits= 1),
            Obs = n()) %>% 
  ungroup() %>% 
  mutate(Verify = case_when(
    (Brighter <  Mean -1) | (Fainter > Mean + 1) ~ "Yes",TRUE ~ "No")) %>%
  filter(Obs >=3 & Verify=="Yes") 

stardust.jd %>% View()

     
##
## Plot of Daily Mean
##
mydata %>% plot_ly(x=~Ymd,y=~Magnitude,name="Magnitude",type="scatter",mode="markers") %>% 
  add_lines(data=starDust,x=~Ymd,y=~Fainter,name="Mean +1") %>% 
  add_lines(data=starDust,x=~Ymd,y=~Brighter,name="Mean -1") %>%
   layout(yaxis = list(autorange = "reversed")) %>%
  layout(title = "Daily Means +/- 1")
  
## Plotly plot of 3 day moving average
## Fun with Moving Averages
mydata$MA <- forecast::ma(mydata$Magnitude,order=3)
mydata$MA <- round(mydata$MA,digits = 1)
mydata$Plus <- round(mydata$MA +1,digits = 1)
mydata$Minus <- round(mydata$MA -1,digits= 1)
mydata %>% plot_ly(x=~Ymd,y=~Magnitude,name="Magnitude",type="scatter",mode="markers") %>% 
  add_lines(x=~Ymd,y=~Plus,name="Mean +1") %>% 
  add_lines(x=~Ymd,y=~Minus,name="Mean -1") %>%
   layout(yaxis = list(autorange = "reversed")) %>%
    layout(title = "3 Day Moving Average +/- 1")

## Plotly plot of 7 day moving average
## Fun with Moving Averages
mydata$MA7 <- forecast::ma(mydata$Magnitude,order=7)
mydata$MA7 <- round(mydata$MA,digits = 1)
mydata$Plus <- round(mydata$MA7 +1,digits = 1)
mydata$Minus <- round(mydata$MA7 -1,digits= 1)
mydata %>% plot_ly(x=~Ymd,y=~Magnitude,name="Magnitude",type="scatter",mode="markers") %>% 
  add_lines(x=~Ymd,y=~Plus,name="Mean +1") %>% 
  add_lines(x=~Ymd,y=~Minus,name="Mean -1") %>%
  layout(yaxis = list(autorange = "reversed")) %>%
  layout(title = "7 Day Moving Average +/- 1")

# Count number of Observations by Month

monthly_counts <- mydata %>% group_by(Monthly = floor_date(Ymd,"month")) %>% 
        summarise(Count =n()) 
monthly_counts %>% plot_ly() %>% add_bars(x=~Monthly,y=~Count) %>% layout(title = "Number of Observations by Month")
##
## Scatter Plot of daily mean
##
mydata %>% plot_ly(x=~Ymd,y=~Magnitude,name="Magnitude",type="scatter",mode="markers") %>% 
  add_trace(data=starDust,x=~Ymd,y=~Fainter,name="Mean +1",type="scatter",mode="markers") %>% 
  add_trace(data=starDust,x=~Ymd,y=~Brighter,name="Mean -1",type="scatter",mode="markers") %>%
  layout(yaxis = list(autorange = "reversed")) %>%
  layout(title = "Daily Means +/- 1")


## 30 Day Mean (simple and Moving Average)
## Plotly plot of 7 day moving average
monthly_magnitude <- mydata %>% group_by(Monthly = floor_date(Ymd,"month")) %>% 
  summarise(Thirty = mean(Magnitude))


mydata$MA7 <- forecast::ma(mydata$Magnitude,order=30)
mydata$MA7 <- round(mydata$MA,digits = 1)
mydata$Plus <- round(mydata$MA7 +1,digits = 1)
mydata$Minus <- round(mydata$MA7 -1,digits= 1)

## EMA 
mydata$EMA <- movavg(mydata$Magnitude,30,"e")
mydata$EMA <- round(mydata$EMA -1,digits= 1)
##
mydata %>% plot_ly(x=~Ymd,y=~MA7,name="Moving Average"  ) %>% add_lines()
  add_lines(data=monthly_magnitude,x=~Monthly,y=~Thirty,name="Simple Average") %>% 
    layout(yaxis = list(autorange = "reversed")) %>%
  layout(title = "30 Day Moving Average +/- 1")
  
##
  ## GGPLOt
  mydata %>% filter(Ymd >="2000-01-01") %>%
  ggplot() +geom_line(aes(x=Ymd,y=MA7,col="Moving Average")) +
    geom_line(aes(x=Ymd,y=EMA,col="EMA")) +
    scale_y_reverse() + geom_smooth(aes(x=Ymd,y=MA7)) +
    labs(title="R_LEO: 30 Day Averages(Moving vs EMA)",x="Date of Observation",y="Mean Magnitude")
  
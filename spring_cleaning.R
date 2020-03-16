library(tidyverse)
library(openxlsx)
library(plotly)
library(lubridate)

rm(list=ls())
mydata <-data.table::fread("../db/R_Hya.csv") %>% 
  select(JD,Star_Name,Band,Magnitude,Validation_Flag) %>%
  mutate( JD = as.numeric(JD)) %>%
  mutate(Ymd =insol::JD(JD,inverse = TRUE)) %>%
  mutate(Ymd = as.Date(Ymd))  %>%
  mutate(Magnitude = as.numeric( gsub("<","", Magnitude) ) + 0.99) %>%
    filter(Band=="Vis." )

mydata$Star_Name <- "R_HYA"
mydata$Validation_Flag <-ifelse(mydata$Validation_Flag=="U","Not VAlidated",mydata$Validation_Flag)
mydata$Validation_Flag <-ifelse(mydata$Validation_Flag=="Z","pre-validation",mydata$Validation_Flag)
mydata$Validation_Flag <-ifelse(mydata$Validation_Flag=="V","Validated",mydata$Validation_Flag)
mydata$Validation_Flag <-ifelse(mydata$Validation_Flag=="T","Failed",mydata$Validation_Flag)


mydata %>% group_by(Star_Name) %>% summarise(n())
mydata %>% group_by(Band) %>% summarise(n())
mydata %>% group_by(Validation_Flag) %>% summarise(n())

## PLot of Validation Flags
##
plot_ly(mydata,x=~Ymd,y=~Magnitude,type="scatter",mode="markers",symbol=~Validation_Flag,colors="Set1") %>%
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

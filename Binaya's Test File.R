library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggrepel)
library(maps)
library(mapproj)

#---Get And Clean Data---

  #---Lyme Disease---
    LDFfile <- "D:\\Personal Files\\School Work 2020 Fall\\Ds 202\\DS-202-Final-Project\\Data sets\\LymeDisease_9211_county (1).csv"
    LDdata <- read.csv(LDFfile)
    LDdata[is.na(LDdata)] <- 0
    LDdata$StateCode <- as.numeric(LDdata$StateCode)
    LDdata$CountyCode <- as.numeric(LDdata$CountyCode)
    LDdata$StateName <- tolower(LDdata$StateName)
    LDdata$CountyName <- tolower(LDdata$CountyName)
    LDdata$CountyName <- str_remove(LDdata$CountyName, " county")
    
    LDdataStates <- LDdata %>% group_by(StateName) %>% summarise(
      ConfirmedCount_1992_1996 = sum(ConfirmedCount_1992_1996),
      ConfirmedCount_1997_2001 = sum(ConfirmedCount_1997_2001),
      ConfirmedCount_2002_2006 = sum(ConfirmedCount_2002_2006),
      ConfirmedCount_2007_2011 = sum(ConfirmedCount_2007_2011))
  
#---Get State Data For Maps
  states <- map_data("state")
  county <- map_data("county")

#---Map Data For Lyme Disease---
  LDdataM <- left_join(LDdataStates, states, by=c("StateName" = "region"))
  
  LDdataM %>% ggplot(aes(x = long, y = lat, group=group, fill = ConfirmedCount_1992_1996)) + geom_polygon(color = NA) + coord_map()
  LDdataM %>% ggplot(aes(x = long, y = lat, group=group, fill = ConfirmedCount_1997_2001)) + geom_polygon(color = NA) + coord_map()
  LDdataM %>% ggplot(aes(x = long, y = lat, group=group, fill = ConfirmedCount_2002_2006)) + geom_polygon(color = NA) + coord_map()
  LDdataM %>% ggplot(aes(x = long, y = lat, group=group, fill = ConfirmedCount_2007_2011)) + geom_polygon(color = NA) + coord_map()
  
#---Graph Data For Lyme Disease---
  LDdataG <- LDdataStates
  LDdataG <- LDdataG %>% pivot_longer(names_to = "ConfirmedCount", values_to = "Count", cols = c(
    ConfirmedCount_1992_1996, 
    ConfirmedCount_1997_2001, 
    ConfirmedCount_2002_2006, 
    ConfirmedCount_2007_2011))
  
  #---Confirmed Count Total For Each Period---
  LDdataG %>% ggplot(aes(x = ConfirmedCount, weight = Count)) + geom_bar() + theme(axis.text.x=element_text(angle = -90, vjust = 0.5))
  
  #---Confirmed Count TOtal For Each State---
  
    #---1992 to 1996---
    LDdataG %>% filter(ConfirmedCount == "ConfirmedCount_1992_1996") %>% ggplot(aes(x = StateName, weight = Count)) + geom_bar() + theme(axis.text.x=element_text(angle = -90, vjust = 0.5))
    
    #---1997 to 2001---
    LDdataG %>% filter(ConfirmedCount == "ConfirmedCount_1997_2001") %>% ggplot(aes(x = StateName, weight = Count)) + geom_bar() + theme(axis.text.x=element_text(angle = -90, vjust = 0.5))
    
    #---2002 to 2006---
    LDdataG %>% filter(ConfirmedCount == "ConfirmedCount_2002_2006") %>% ggplot(aes(x = StateName, weight = Count)) + geom_bar() + theme(axis.text.x=element_text(angle = -90, vjust = 0.5))
    
    #---2007 to 2011---
    LDdataG %>% filter(ConfirmedCount == "ConfirmedCount_2007_2011") %>% ggplot(aes(x = StateName, weight = Count)) + geom_bar() + theme(axis.text.x=element_text(angle = -90, vjust = 0.5))

    #---All Periods Together---
    LDdataG %>% ggplot(aes(x = StateName, y = Count, fill = ConfirmedCount)) + geom_bar(stat = "identity", position = 'dodge') + theme(axis.text.x=element_text(angle = -90, vjust = 0.5))
    LDdataG %>% ggplot(aes(x = StateName, weight = Count)) + geom_bar() + theme(axis.text.x=element_text(angle = -90, vjust = 0.5)) + facet_wrap(~ConfirmedCount)
  
  
  
  
  
  
  
  
                     
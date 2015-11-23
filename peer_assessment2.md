Title:Impact of Severe Weather Events on Public Health and Economy in the United States

• Introduction

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.
---
• Assignment

The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. 


```r
setwd("/Users/lizard0827/Desktop/class/Reproducible research/")
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
library(R.utils) # for bunzip2
```

```
## Warning: package 'R.utils' was built under R version 3.1.3
```

```
## Loading required package: R.oo
```

```
## Warning: package 'R.oo' was built under R version 3.1.3
```

```
## Loading required package: R.methodsS3
## R.methodsS3 v1.7.0 (2015-02-19) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.19.0 (2015-02-27) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v2.1.0 (2015-05-27) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r
library(gridExtra) # for advanced plots
```

```
## Warning: package 'gridExtra' was built under R version 3.1.3
```
Loading the data

```r
if(!file.exists("./stormData.csv.bz2")){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="stormData.csv.bz2", method="curl")
bunzip2("./stormData.csv.bz2", overwrite=T, remove=F)
}
```


```r
Data<-read.csv("stormData.csv")
head(Data, n=5)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
```

Select data and explore data

```r
StormData<-select(Data, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
head(StormData)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15    25.0          K       0           
## 2 TORNADO          0        0     2.5          K       0           
## 3 TORNADO          0        2    25.0          K       0           
## 4 TORNADO          0        2     2.5          K       0           
## 5 TORNADO          0        2     2.5          K       0           
## 6 TORNADO          0        6     2.5          K       0
```

```r
str(StormData)
```

```
## 'data.frame':	902297 obs. of  7 variables:
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
unique(StormData$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(StormData$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

We need to setup a cross-reference table to convert the PROPDMGEXP and CROPDMGEXP values to numerical multipliers.
We use the following conversion:
## blanks: 1 (10^0)
## + or - or ?: 0
## H or h : 100 (hundred)
## K or k: 1000 (thousand)
## M or m: 1000000 (1e+06, million)
## B or b: 1000000000 (1e+09, billion)

```r
StormData$PROPDMGEXP<- as.character(StormData$PROPDMGEXP)
StormData$PROPDMGEXP[StormData$PROPDMGEXP ==""]<- 1
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="?"]<- 0
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="+"]<- 0
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="-"]<- 0
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="0"]<- 1
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="1"]<- 10
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="2"]<- 100
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="3"]<- 1000
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="4"]<- 10000
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="5"]<- 1e+05
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="6"]<- 1e+06
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="7"]<- 1e+07
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="8"]<- 1e+08
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="B"]<- 1e+09
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="h"]<- 100
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="H"]<- 100
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="K"]<- 1000
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="m"]<- 1e+06
StormData$PROPDMGEXP[StormData$PROPDMGEXP =="M"]<- 1e+06
```
Compute the property damage value

```r
head(StormData)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15    25.0       1000       0           
## 2 TORNADO          0        0     2.5       1000       0           
## 3 TORNADO          0        2    25.0       1000       0           
## 4 TORNADO          0        2     2.5       1000       0           
## 5 TORNADO          0        2     2.5       1000       0           
## 6 TORNADO          0        6     2.5       1000       0
```

```r
StormData$PROPDMGEXP<-as.numeric(StormData$PROPDMGEXP)
StormData$PROPDMGVAL<-StormData$PROPDMG* StormData$PROPDMGEXP
head(StormData)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15    25.0       1000       0           
## 2 TORNADO          0        0     2.5       1000       0           
## 3 TORNADO          0        2    25.0       1000       0           
## 4 TORNADO          0        2     2.5       1000       0           
## 5 TORNADO          0        2     2.5       1000       0           
## 6 TORNADO          0        6     2.5       1000       0           
##   PROPDMGVAL
## 1      25000
## 2       2500
## 3      25000
## 4       2500
## 5       2500
## 6       2500
```
Exploring the crop exponent data

```r
StormData$CROPDMGEXP<- as.character(StormData$CROPDMGEXP)
StormData$CROPDMGEXP[StormData$CROPDMGEXP ==""]<- 1
StormData$CROPDMGEXP[StormData$CROPDMGEXP =="?"]<- 0
StormData$CROPDMGEXP[StormData$CROPDMGEXP =="0"]<- 1
StormData$CROPDMGEXP[StormData$CROPDMGEXP =="2"]<- 100
StormData$CROPDMGEXP[StormData$CROPDMGEXP =="B"]<- 1e+09
StormData$CROPDMGEXP[StormData$CROPDMGEXP =="k"]<- 100
StormData$CROPDMGEXP[StormData$CROPDMGEXP =="K"]<- 1000
StormData$CROPDMGEXP[StormData$CROPDMGEXP =="m"]<- 1e+06
StormData$CROPDMGEXP[StormData$CROPDMGEXP =="M"]<- 1e+06
head(StormData)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15    25.0       1000       0          1
## 2 TORNADO          0        0     2.5       1000       0          1
## 3 TORNADO          0        2    25.0       1000       0          1
## 4 TORNADO          0        2     2.5       1000       0          1
## 5 TORNADO          0        2     2.5       1000       0          1
## 6 TORNADO          0        6     2.5       1000       0          1
##   PROPDMGVAL
## 1      25000
## 2       2500
## 3      25000
## 4       2500
## 5       2500
## 6       2500
```

Compute the property damage value

```r
StormData$CROPDMGEXP<-as.numeric(StormData$CROPDMGEXP)
StormData$CROPDMGVAL<-StormData$CROPDMG* StormData$CROPDMGEXP
head(StormData)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15    25.0       1000       0          1
## 2 TORNADO          0        0     2.5       1000       0          1
## 3 TORNADO          0        2    25.0       1000       0          1
## 4 TORNADO          0        2     2.5       1000       0          1
## 5 TORNADO          0        2     2.5       1000       0          1
## 6 TORNADO          0        6     2.5       1000       0          1
##   PROPDMGVAL CROPDMGVAL
## 1      25000          0
## 2       2500          0
## 3      25000          0
## 4       2500          0
## 5       2500          0
## 6       2500          0
```
Plot top 10 event types by number of fatalities and by number of Injuries

```r
Fatal<-aggregate(x=list(FATALITIES=StormData$FATALITIES), by=list(EVTYPE =StormData$EVTYPE), FUN= sum)
head(Fatal)
```

```
##                  EVTYPE FATALITIES
## 1    HIGH SURF ADVISORY          0
## 2         COASTAL FLOOD          0
## 3           FLASH FLOOD          0
## 4             LIGHTNING          0
## 5             TSTM WIND          0
## 6       TSTM WIND (G45)          0
```

```r
Fatal<-arrange(Fatal, desc(FATALITIES))
Fatal10<-Fatal[1:10, ]
head(Fatal10)
```

```
##           EVTYPE FATALITIES
## 1        TORNADO       5633
## 2 EXCESSIVE HEAT       1903
## 3    FLASH FLOOD        978
## 4           HEAT        937
## 5      LIGHTNING        816
## 6      TSTM WIND        504
```

```r
Fatalplot<- ggplot(Fatal10, aes(reorder(EVTYPE, -FATALITIES), FATALITIES))+ ggtitle("Fatalitites by Event Type")+
  geom_bar(stat="identity", fill="green")+
  xlab("Event Type")+
  theme(axis.text.x = element_text(angle = 45, size=8, hjust = 1, vjust = 1))+
  ylab("Total Fatalities")
##plot our 
head(StormData)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15    25.0       1000       0          1
## 2 TORNADO          0        0     2.5       1000       0          1
## 3 TORNADO          0        2    25.0       1000       0          1
## 4 TORNADO          0        2     2.5       1000       0          1
## 5 TORNADO          0        2     2.5       1000       0          1
## 6 TORNADO          0        6     2.5       1000       0          1
##   PROPDMGVAL CROPDMGVAL
## 1      25000          0
## 2       2500          0
## 3      25000          0
## 4       2500          0
## 5       2500          0
## 6       2500          0
```

```r
Injuries<-aggregate(x=list(INJURIES=StormData$INJURIES), by=list(EVTYPE =StormData$EVTYPE), FUN= sum)
head(Fatal)
```

```
##           EVTYPE FATALITIES
## 1        TORNADO       5633
## 2 EXCESSIVE HEAT       1903
## 3    FLASH FLOOD        978
## 4           HEAT        937
## 5      LIGHTNING        816
## 6      TSTM WIND        504
```

```r
Injuries<-arrange(Injuries, desc(INJURIES))
Injuries10<-Injuries[1:10, ]
head(Injuries10)
```

```
##           EVTYPE INJURIES
## 1        TORNADO    91346
## 2      TSTM WIND     6957
## 3          FLOOD     6789
## 4 EXCESSIVE HEAT     6525
## 5      LIGHTNING     5230
## 6           HEAT     2100
```

```r
Injuriesplot<- ggplot(Injuries10, aes(reorder(EVTYPE, -INJURIES), INJURIES))+ ggtitle("Injuries by Event Type")+
  geom_bar(stat="identity", fill="orange")+
  xlab("Event Type")+
  theme(axis.text.x = element_text(angle = 45, size=8, hjust = 1, vjust = 1))+
  ylab("Total Fatalities")
grid.arrange(Fatalplot, Injuriesplot, ncol = 2)
```

![](peer_assignment2_files/figure-html/unnamed-chunk-9-1.png) 
Plot our top 10 event types with the greatest econimic cost of property damage and of crop damage

```r
Propdmg<-aggregate(x=list(PROPDMGVAL=StormData$PROPDMGVAL), by=list(EVTYPE =StormData$EVTYPE), FUN= sum)
head(Propdmg)
```

```
##                  EVTYPE PROPDMGVAL
## 1    HIGH SURF ADVISORY     200000
## 2         COASTAL FLOOD          0
## 3           FLASH FLOOD      50000
## 4             LIGHTNING          0
## 5             TSTM WIND    8100000
## 6       TSTM WIND (G45)       8000
```

```r
Propdmg<-arrange(Propdmg, desc(PROPDMGVAL))
Propdmg10<-Propdmg[1:10, ]
head(Propdmg10)
```

```
##              EVTYPE   PROPDMGVAL
## 1             FLOOD 144657709870
## 2 HURRICANE/TYPHOON  69305840000
## 3           TORNADO  56947382445
## 4       STORM SURGE  43323536000
## 5       FLASH FLOOD  16822678195
## 6              HAIL  15735270147
```

```r
Propdmgplot<- ggplot(Propdmg10, aes(reorder(EVTYPE, -PROPDMGVAL), PROPDMGVAL))+ ggtitle("Top property damage by Event Type")+
  geom_bar(stat="identity", fill="red")+
  xlab("Event Type")+
  theme(axis.text.x = element_text(angle = 45, size=8, hjust = 1, vjust = 1))+
  ylab("Total cost")
Cropdmg<-aggregate(x=list(CROPDMGVAL=StormData$CROPDMGVAL), by=list(EVTYPE =StormData$EVTYPE), FUN= sum)
head(Cropdmg)
```

```
##                  EVTYPE CROPDMGVAL
## 1    HIGH SURF ADVISORY          0
## 2         COASTAL FLOOD          0
## 3           FLASH FLOOD          0
## 4             LIGHTNING          0
## 5             TSTM WIND          0
## 6       TSTM WIND (G45)          0
```

```r
Cropdmg<-arrange(Cropdmg, desc(CROPDMGVAL))
Cropdmg10<-Cropdmg[1:10, ]
head(Cropdmg10)
```

```
##        EVTYPE  CROPDMGVAL
## 1     DROUGHT 13972566000
## 2       FLOOD  5661968450
## 3 RIVER FLOOD  5029459000
## 4   ICE STORM  5022113500
## 5        HAIL  3025579173
## 6   HURRICANE  2741910000
```

```r
Cropdmgplot<- ggplot(Cropdmg10, aes(reorder(EVTYPE, -CROPDMGVAL), CROPDMGVAL))+ ggtitle("Top Crop damage by Event Type")+
  geom_bar(stat="identity", fill="blue")+
  xlab("Event Type")+
  theme(axis.text.x = element_text(angle = 45, size=8, hjust = 1, vjust = 1))+
  ylab("Total cost")
grid.arrange(Propdmgplot, Cropdmgplot, ncol = 2)
```

![](peer_assignment2_files/figure-html/unnamed-chunk-10-1.png) 


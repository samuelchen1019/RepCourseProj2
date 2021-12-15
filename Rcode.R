library(tidyverse)
library(lubridate)
library(knitr)

##Locate, download and unzip the data file.
fileName<-"repdata_data_StormData.csv.bz2"
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!file.exists(fileName)){
  download.file(fileURL,destfile=fileName,method="curl")
}

##Read the data.
df<-read.csv(fileName)
dim(df)
names(df)
str(df)
summary(df)

##Q1: Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
#Subsetting the raw data set
df_selected<-select(df,BGN_DATE,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
gsub(" 0:00:00", "",df_selected$BGN_DATE)
df_selected$BGN_DATE<-mdy(df_selected$BGN_DATE)
head(df_selected)
df_selected<-subset(df_selected,year(BGN_DATE)>"1995"& year(BGN_DATE)<=2011)

##Format transformations
df_selected$FATALITIES<-as.numeric(df_selected$FATALITIES)
df_selected$INJURIES<-as.numeric(df_selected$INJURIES)
df_selected$PROPDMG<-as.numeric(df_selected$PROPDMG)
df_selected$CROPDMG<-as.numeric(df_selected$CROPDMG)
#Draw the data set just for the number of fatalities and injuries.
PHDMG<-aggregate(data=df_selected,FATALITIES+INJURIES~EVTYPE, FUN="sum")
#Manipulate the data set.
str(PHDMG)
summary(PHDMG)
#Rename the variables.
names(PHDMG)<-c("EVTYPE","DMG")
PHDMG$EVTYPE<-str_trim(PHDMG$EVTYPE)
#Filter the records in which the damage (fatalities + injuries) are not 0.
PHDMG_NoZero<-subset(PHDMG,DMG>0)
PHDMG_NoZero<-PHDMG_NoZero[order(-PHDMG_NoZero$DMG),]
PHDMG_NoZero[1,]
#Making a plot.
ggplot(data=PHDMG_NoZero[1:10,],aes(x=reorder(EVTYPE,-DMG),y=DMG,fill=EVTYPE,label=DMG))+geom_col()+
  geom_text(vjust=0)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Events") + ylab("Fatalities and Injuries")+
  ggtitle("1996~2011, The most harmful event type to population health")

##Q2: Across the United States, which types of events have the greatest economic consequences?
ev_econ<-subset(df_selected,PROPDMG>0&CROPDMG>0)
ev_econ<-select(ev_econ,EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
table(ev_econ$PROPDMGEXP)
table(ev_econ$CROPDMGEXP)

ev_econ$PROPDMGfct[ev_econ$PROPDMGEXP=="K"]<-10^3
ev_econ$PROPDMGfct[ev_econ$PROPDMGEXP=="M"]<-10^6
ev_econ$PROPDMGfct[ev_econ$PROPDMGEXP=="B"]<-10^9

ev_econ$CROPDMGfct[ev_econ$CROPDMGEXP=="K"]<-10^3
ev_econ$CROPDMGfct[ev_econ$CROPDMGEXP=="M"]<-10^6
ev_econ$CROPDMGfct[ev_econ$CROPDMGEXP=="B"]<-10^9

ev_econ<-mutate(ev_econ,ECODMG=PROPDMG*PROPDMGfct+CROPDMG*CROPDMGfct)
ev_econ_dmg<-aggregate(data=ev_econ,ECODMG~EVTYPE,FUN="sum")
max(ev_econ_dmg$ECODMG)
ev_econ_dmg<-ev_econ_dmg[order(-ev_econ_dmg$ECODMG),]

ggplot(data=ev_econ_dmg[1:5,],aes(x=reorder(EVTYPE,-ECODMG),y=ECODMG,fill=EVTYPE,label=ECODMG))+geom_col()+
  geom_text(vjust=0)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Events") + ylab("Economic Consequences")+
  ggtitle("1996~2011, The Top 5 events that have Economic Consequences")
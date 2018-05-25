setwd("~/Desktop/tidytuesday/data/week8_honey_production")
require(dplyr)

#### raw 1 ###
raw1=read.csv("honeyraw_1998to2002.csv",skip=9,header=F,na.strings="",stringsAsFactors=F) ## pick skip by looking manually
raw1=raw1[,3:ncol(raw1)] ## skip first two columns
names(raw1)=c("state","numColonies","yieldPerColony","production","stocks","avgPricePerLb","valProd") ## manually
## pounds, 1000 pounds, 1000 pounds, cents, 1000 dollars


tryThis=raw1[complete.cases(raw1),] ## lots of the weird rows have misisng values
niceData=tryThis[-which(nchar(tryThis$state)>2),] ## just want states 


## get year ready
test=niceData %>% group_by(state)%>% summarise(count=n())
test[which(test$count!=5),]


(1998:2002)[ceiling(which(niceData$state=="SC")/nrow(test))]


year=c(rep(1998:2000,each=nrow(test)-1),rep(2001:2002,each=nrow(test)))
length(year)
nrow(niceData)

niceData$year=year
niceData1=niceData

#### raw 2 ####

# skipping other info at top for now

raw2=read.csv("honeyraw_2003to2007.csv",skip=82,header=F,na.strings="",stringsAsFactors=F)
raw2=raw2[,3:ncol(raw2)]
names(raw2)=c("state","numColonies","yieldPerColony","production","stocks","avgPricePerLb","valProd")

tryThis=raw2[complete.cases(raw2),]
niceData=tryThis[-which(nchar(tryThis$state)>2),]

test=niceData %>% group_by(state)%>% summarise(count=n())

test[which(test$count!=5),]

(2003:2007)[ceiling(which(niceData$state=="AL")/nrow(test))]
(2003:2007)[ceiling(which(niceData$state=="MD")/nrow(test))]
(2003:2007)[ceiling(which(niceData$state=="OK")/nrow(test))]
(2003:2007)[ceiling(which(niceData$state=="SC")/nrow(test))]


year=c(rep(2003,nrow(test)),rep(2004:2006,each=nrow(test)-3),rep(2007,nrow(test)-4))
           
niceData$year=year

niceData2=niceData

#### raw 3 ####

## skip some additional info at top for now

raw3=read.csv("honeyraw_2008to2012.csv",skip=73,header=F,na.strings="",stringsAsFactors=F)

raw3=raw3[,3:ncol(raw3)]
names(raw3)=c("state","numColonies","yieldPerColony","production","stocks","avgPricePerLb","valProd")
## pounds, 1000 pounds, 1000 pounds, cents, 1000 dollars


tryThis=raw3[complete.cases(raw3),]
row.names(tryThis)=NULL

require(readr)
niceData=tryThis[-which(is.na(parse_number(tryThis$numColonies))),]
niceData[which(unlist(lapply(strsplit(niceData$state," "),length))>2),]
niceData=niceData[-which(unlist(lapply(strsplit(niceData$state," "),length))>2),]



test=niceData %>% group_by(state)%>% summarise(count=n())

test[which(test$count!=5),]

which(niceData$state=="Alabama")/nrow(test)
which(niceData$state=="Nevada")/nrow(test)


(2008:2012)[ceiling(which(niceData$state=="Alabama")/nrow(test))]
(2008:2012)[ceiling(which(niceData$state=="Nevada")/nrow(test))]

year=c(rep(2008,nrow(test)),rep(2009:2011,each=nrow(test)-1),rep(2012,nrow(test)-2))

niceData$year=year
niceData3=niceData

## make consistent with other two dataframes
toMatch=cbind.data.frame(abb=state.abb,name=state.name)
niceData3=inner_join(niceData3,toMatch,by=c("state"="name")) 

niceData3$state=niceData3$abb
niceData3=niceData3[,-ncol(niceData3)]


head(niceData1)
head(niceData2)
head(niceData3)

niceDataFull=rbind.data.frame(niceData1,niceData2,niceData3)
write.csv(niceDataFull,"honeyDataNice.csv",row.names=F)

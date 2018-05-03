

require(ggplot2)
require(maps)
require(dplyr)
require(plotly)

counties= map_data("county")
state=map_data("state")

county_plot <-function(x){
  ## adapted from
  ## function approach
  #<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">For <a href="https://twitter.com/hashtag/TidyTuesday?src=hash&amp;ref_src=twsrc%5Etfw">#TidyTuesday</a> I created simple function which allows you to plot any continuous variable in the data on a map <a href="https://twitter.com/hashtag/rstats?src=hash&amp;ref_src=twsrc%5Etfw">#rstats</a> <a href="https://twitter.com/hashtag/r4ds?src=hash&amp;ref_src=twsrc%5Etfw">#r4ds</a> <a href="https://t.co/6Q1I121VqI">pic.twitter.com/6Q1I121VqI</a></p>&mdash; Aidan Boland (@AidoBo) <a href="https://twitter.com/AidoBo/status/991338257391804416?ref_src=twsrc%5Etfw">May 1, 2018</a></blockquote>
  #  <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  #https://twitter.com/AidoBo/status/991338257391804416
  
  all_county$x<-all_county[,x] ## a different fix for this? something like aes_string?
  
  ggplot(data=counties,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_county, aes(fill=x),color="grey")+labs(fill=x)+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
  
}

setwd("~/Desktop/tidytuesday/data")

acs<-read.csv("acs2015_county_data.csv")

head(acs)
names(acs)

all_county<-inner_join(counties,acs %>% mutate(County=tolower(County),State=tolower(State)),by=c("subregion"="County","region"="State"))


#### Commuting ####

p=county_plot("MeanCommute") ## what are those hot spots?
p

## where to put text =paste(County,State,sep="-")
#ggplotly(p)

#plot(acs$MeanCommute,acs$IncomePerCap) 

## where can commuting make you make more money?
p <- ggplot(acs, aes(x = MeanCommute, y = IncomePerCap, text =paste(County,State,sep="-"))) +
  geom_point() +xlab("mean commute")+
  ylab("income per cap")+ggtitle("Where Does/Doesn't Commuting Pay Off?")
p
p <- ggplotly(p)
p

## rough idea on a map instead of relying on hover
## above average income in below average commute time per state
averagesByState=group_by(acs,State)%>% summarize(avgMeanCommute=mean(MeanCommute),avgIncomePerCap=mean(IncomePerCap))


acsM=merge(acs,averagesByState,by.x="State",by.y="State",all.x=T)

acs$goodCommuteIncomeLevels=rep(0, nrow(acs))
acs$goodCommuteIncomeLevels[which(acsM$IncomePerCap>acsM$avgIncomePerCap & acsM$MeanCommute < acsM$avgMeanCommute)]=1

all_county<-inner_join(counties,acs %>% mutate(County=tolower(County),State=tolower(State)),by=c("subregion"="County","region"="State"))

county_plot("goodCommuteIncomeLevels") ## need to make this discrete

#### Work at Home ####
p=county_plot("WorkAtHome") ## what are these hot spots?
p
## where to put text =paste(County,State,sep="-")
#ggplotly(p)

plot(acs$WorkAtHome,acs$IncomePerCap)

## transit
county_plot("Transit") ## boring, although what is going on in Nevada?



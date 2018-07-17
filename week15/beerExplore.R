setwd("~/Desktop/tidytuesday/data")

require(readxl)
require(dplyr)

beers=read_excel("week15_beers.xlsx",sheet=1)
brewer=read_excel("week15_beers.xlsx",sheet=2)

head(beers)
head(brewer)


beer=inner_join(beers,brewer,by =c("brewery_id"="id"))


byState=beer %>% group_by(state) %>% summarise(numBrewer=length(unique(brewery_id)),count=n(),mabv=mean(abv,na.rm=T))

beer %>% group_by(state,style) %>% summarise(numBrewer=length(unique(brewery_id)),count=n(),mabv=mean(abv,na.rm=T))

unique(beer$style)

## IPA
## Brown
## Blonde
## Red
## Black
## Ale
## American
## Stout

require(stringr)

str_locate(beer$style,"Stout")
stout=beer[str_detect(beer$style,"Stout"),]
american=beer[str_detect(beer$style,"American"),]
ipa=beer[str_detect(beer$style,"IPA"),]

require(ggplot2)
counties= map_data("county")
state=map_data("state")

stateInfo=cbind.data.frame(abb=state.abb,name=tolower(state.name))

state=inner_join(state,stateInfo,by=c("region"="name"))

all_state=inner_join(state,byState,by=c("abb"="state"))

  
  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=mabv),color="grey")+labs(fill="mabv")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
  ## haha Nevada, and Utah
  
  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=numBrewer),color="grey")+labs(fill="numBrewer")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
  ## Colorado
  
  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=count/numBrewer),color="grey")+labs(fill="beerPerBrewer")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
## Kansas is lit  
  
  
  byStateA=american %>% group_by(state) %>% summarise(numBrewer=length(unique(brewery_id)),count=n(),mabv=mean(abv,na.rm=T))

  all_state=inner_join(state,byStateA,by=c("abb"="state"))
  
  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=numBrewer),color="grey")+labs(fill="numBrewer")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries

  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=count/numBrewer),color="grey")+labs(fill="beerPerBrewer")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
  ## Mississippi
  
  
  
  byStateS=stout %>% group_by(state) %>% summarise(numBrewer=length(unique(brewery_id)),count=n(),mabv=mean(abv,na.rm=T))
  
  all_state=inner_join(state,byStateS,by=c("abb"="state"))
  
  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=numBrewer),color="grey")+labs(fill="numBrewer")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
  ## wow, some states have no stouts, rude
  
  
  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=count/numBrewer),color="grey")+labs(fill="beerPerBrewer")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
  ## Kansas
  
  byStateI=ipa %>% group_by(state) %>% summarise(numBrewer=length(unique(brewery_id)),count=n(),mabv=mean(abv,na.rm=T))
  
  all_state=inner_join(state,byStateI,by=c("abb"="state"))
  
  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=numBrewer),color="grey")+labs(fill="numBrewer")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
  ## WV ha
  
  
  ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+
    geom_polygon(data=all_state, aes(fill=count/numBrewer),color="grey")+labs(fill="beerPerBrewer")+scale_fill_distiller(palette="Spectral")+theme_void()+
    geom_path(data=state, aes(x=long,y=lat,group=group),color="black") ## add state boundaries
  ## Utah?
  ut=beer[which(beer$state == "UT"),]
  ut[str_detect(ut$style, "IPA"),] ## double counting
  
  
  ## beer palette?
  
 beer %>% group_by(city,state) %>% summarise(count=n(),numBrewer=length(unique(brewery_id))) %>% arrange(desc(count))
 beer %>% group_by(city,state) %>% summarise(count=n(),numBrewer=length(unique(brewery_id))) %>% arrange(desc(numBrewer))
 ## ok what is going on in grand rapids?
 
 
 beer %>% group_by(style)%>% summarise(count=n(),coeffVarabv=mean(abv,na.rm=T)/sd(abv,na.rm=T)) %>% arrange(desc(count))  %>% head(20) %>% arrange(desc(coeffVarabv))
 
 
 
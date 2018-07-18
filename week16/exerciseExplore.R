require(readxl)
require(dplyr)
require(ggplot2)
require(stringr)
require(tidyr)
setwd("~/Desktop/tidytuesday/data")
exercise=read_excel("week16_exercise.xlsx",sheet=1)

head(exercise)


exercise=exercise[,-1] ## remove count

dim(exercise)

exercise=exercise[-1,] ## remove all states

exerciseT=exercise %>% gather(type, value,-state)

unique(exerciseT$type)


require(geofacet)

exerciseT$value=as.numeric(exerciseT$value)

ggplot(exerciseT,aes(x=type,y=value,fill=type))+ geom_bar(stat="identity",position = position_dodge2(preserve = "total"))+facet_geo(~state) ## get rid of x labels
  
  
ggplot(subset(exerciseT,type %in% c("men_nonworking","men_working","women_nonworking","women_working")),aes(x=type,y=value,fill=type))+ geom_bar(stat="identity",position = position_dodge2(preserve = "total"))+facet_geo(~state) 

## those who working exercise more? I would have thought it was the other way around



## where are the largest/smallest? disparities?

exercise[2:ncol(exercise)]=apply(exercise[2:ncol(exercise)],2,function(x){as.numeric(x)})

exercise[which.max(abs(exercise$men-exercise$women)),] ## DC
exercise[which.min(abs(exercise$men-exercise$women)),] ## Montana

exercise[which.max(abs(exercise$men_working-exercise$men_nonworking)),] ## Vermont
exercise[which.min(abs(exercise$men_working-exercise$men_nonworking)),] ## Iowa

exercise[which.max(abs(exercise$women_working-exercise$women_nonworking)),] ## Wyoming
exercise[which.min(abs(exercise$women_working-exercise$women_nonworking)),] ## Oklahoma


counties= map_data("county")
state=map_data("state")

#stateInfo=cbind.data.frame(abb=state.abb,name=tolower(state.name))

#state=inner_join(state,stateInfo,by=c("region"="name"))

exercise$state=tolower(exercise$state)

all_state=inner_join(state,exercise,by=c("region"="state"))

ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+geom_polygon(data=all_state, aes(fill=men-women),color="grey")+labs(fill="men - women")+scale_fill_distiller(palette="Spectral")+theme_void()+geom_path(data=state, aes(x=long,y=lat,group=group),color="black")

ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+geom_polygon(data=all_state, aes(fill=men_working-men_nonworking),color="grey")+labs(fill="menWork-menNonwork")+scale_fill_distiller(palette="Spectral")+theme_void()+geom_path(data=state, aes(x=long,y=lat,group=group),color="black")


ggplot(data=state,mapping=aes(x=long,y=lat,group=group))+geom_polygon(data=all_state, aes(fill=women_working-women_nonworking),color="grey")+labs(fill="womenWork-womenNonwork")+scale_fill_distiller(palette="Spectral")+theme_void()+geom_path(data=state, aes(x=long,y=lat,group=group),color="black")


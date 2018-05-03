### Disparities in STEM ###

Take-aways

1. About equal number of indivuals in scientist jobs.
2. Many more males in engineering jobs.
3. Rough OLS: For every dollar a woman makes in science, a man makes $1.52.
4. Rough OLS: For every dollar a woman makes in engineering, a man makes $1.26.

#### setup ####
setwd("~/Desktop/tidytuesday/data")
aus<-read.csv("week4_australian_salary.csv")

require(ggplot2)
require(plotly) ## use to hover and see the job names


#### look at STEM ####
aus[grep("stat",aus$occupation),] ## looking for statistics
aus[grep("math",aus$occupation),] ## nope

scientist=aus[grep("scien",aus$occupation),] ## bingo
engineer=aus[grep("engineer",aus$occupation),]

scientistG=split(scientist,scientist$gender)
engineerG=split(engineer,engineer$gender)

names(scientistG[[1]])=paste("F",names(scientistG[[1]]),sep="")
names(scientistG[[2]])=paste("M",names(scientistG[[2]]),sep="")

names(engineerG[[1]])=paste("F",names(engineerG[[1]]),sep="")
names(engineerG[[2]])=paste("M",names(engineerG[[2]]),sep="")

scientistFull=cbind(scientistG[[1]],scientistG[[2]])
engineerFull=cbind(engineerG[[1]],engineerG[[2]])

#### make number of job plots ####

p <- ggplot(scientistFull, aes(x = Findividuals, y = Mindividuals, text =Moccupation)) +
  geom_point() +geom_abline(intercept = 0, slope = 1)+xlab("number of individuals")+
  ylab("average taxable income for males ($)")+ggtitle("Science Jobs")
p <- ggplotly(p)
p
## around parity

p <- ggplot(engineerFull, aes(x = Findividuals, y = Mindividuals, text =Moccupation)) +
  geom_point() +geom_abline(intercept = 0, slope = 1)+xlab("number of individuals")+
  ylab("average taxable income for males ($)")+ggtitle("Engineer Jobs")
p <- ggplotly(p)
p
## many more men



#### make salary plots ####

p <- ggplot(scientistFull, aes(x = Faverage_taxable_income, y = Maverage_taxable_income, text =Moccupation)) +
  geom_point() +geom_abline(intercept = 0, slope = 1)+xlab("average taxable income for females ($)")+
  ylab("average taxable income for males ($)")+ggtitle("Science Jobs")
p <- ggplotly(p)
p
## more for men

p <- ggplot(engineerFull, aes(x = Faverage_taxable_income, y = Maverage_taxable_income, text =Moccupation)) +
  geom_point() +geom_abline(intercept = 0, slope = 1)+xlab("average taxable income for females ($)")+
  ylab("average taxable income for males ($)")+ggtitle("Engineer Jobs")
p <- ggplotly(p)
p
## more for men

lm(scientistG[[2]]$Maverage_taxable_income~scientistG[[1]]$Faverage_taxable_income)
# Call:
#   lm(formula = scientistG[[2]]$Maverage_taxable_income ~ scientistG[[1]]$Faverage_taxable_income)
# 
# Coefficients:
#   (Intercept)  scientistG[[1]]$Faverage_taxable_income  
# -14063.862                                    1.521  

lm(engineerG[[2]]$Maverage_taxable_income~engineerG[[1]]$Faverage_taxable_income)

# Call:
#   lm(formula = engineerG[[2]]$Maverage_taxable_income ~ engineerG[[1]]$Faverage_taxable_income)
# 
# Coefficients:
#   (Intercept)  engineerG[[1]]$Faverage_taxable_income  
# 6543.508                                   1.261  
# 
# 

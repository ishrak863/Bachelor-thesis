#importing data from CSV file)
library(ggplot2)
data<- read.csv('Farhan_graph_table.csv')

data$Time.limit<- factor(data$Time.limit)
data$Situation<- factor(data$Situation,level=c('Before','After'))


#Analysing Data through Boxplot
#Three different time period boxplot (30, 45, 60 minutes)
ggplot(data=data)+
  geom_boxplot(aes(x=Time.limit,y=Accessibility.Index,
                   color=Situation))+
  ggtitle('Accessibility Index before and after the introduction of Circular Bus')+
  xlab('Time limit (minutes)')+
  ylab('Accessiblity Index')+
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size=12),
    plot.title = element_text(hjust= 0.5,size=16),
  )


#T-test (for 3 different time period)
time<- c(30, 45, 60) 
for (item in time){
  x<- data$Accessibility.Index[data$Situation=='Before'& data$Time.limit== item] 
  y<- data$Accessibility.Index[data$Situation=='After' & data$Time.limit== item] 
  print(shapiro.test(x))
  print(shapiro.test(y))
  print(t.test(x,y,mu=0,alt='two.sided',paired=T,conf.level = .95))
}


  

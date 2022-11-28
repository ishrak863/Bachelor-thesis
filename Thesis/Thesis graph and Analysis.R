library(ggplot2)
data<- read.csv('Farhan_graph_table.csv')

data$Time.limit<- factor(data$Time.limit)
data$Situation<- factor(data$Situation,level=c('Before','After'))


#png('capture1.png',width=1000,height=800,res=140)
ggplot(data=data)+
  geom_boxplot(aes(x=Time.limit,y=Accessibility.Index,
                   color=Situation))+
  ggtitle('Accessibility Index before and after the introduction of Circular Bus')+
  xlab('Time limit (minutes)')+
  ylab('Accessiblity Index')+
  #scale_y_continuous(breaks = a)+
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size=12),
    plot.title = element_text(hjust= 0.5,size=16),
  )


#T-test
time<- c(30, 45, 60) 
for (item in time){
  x<- data$Accessibility.Index[data$Situation=='Before'& data$Time.limit== item] 
  y<- data$Accessibility.Index[data$Situation=='After' & data$Time.limit== item] 
  print(shapiro.test(x))
  print(shapiro.test(y))
  print(t.test(x,y,mu=0,alt='two.sided',paired=T,conf.level = .95))
}


  
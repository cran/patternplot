library(patternplot)
library(png)
library(ggplot2)

#Example 1
data <- read.csv(system.file("extdata", "fruits.csv", package="patternplot"))
group<-data$Fruit
y<-data$Weight
x<-data$Store

pattern.type<-c('nwlines', 'blank', 'waves')
pattern.color=c('black','black', 'black')
background.color=c('white','gray80', 'white')
frame.color=c('black', 'black', 'black')
pattern.line.size<-c(6, 1,6)
density<-c(6, 1, 8)
patternboxplot(data,x, y,group,pattern.type=pattern.type,pattern.line.size=pattern.line.size,
pattern.color=pattern.color,background.color=background.color,frame.color=frame.color, 
density=density,legend.h=2,legend.x.pos=1.1,legend.y.pos=0.49,legend.pixel=10, legend.w=0.2)

#Example 2
pattern.color=c('black','white', 'grey20')
background.color=c('gold','lightpink', 'lightgreen')
patternboxplot(data,x, y,group=group,pattern.type=pattern.type,pattern.line.size=pattern.line.size, 
pattern.color=pattern.color, background.color=background.color,frame.color=frame.color, 
density=density,legend.h=2,legend.x.pos=1.1,legend.y.pos=0.49,legend.pixel=10,legend.w=0.2)


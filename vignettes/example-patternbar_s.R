#Example 1
library(patternplot)
library(png)
library(ggplot2)
data <- read.csv(system.file("extdata", "monthlyexp.csv", package="patternplot"))
x<-data$Location
y<-data$Amount
group<-data$Type

patternbar_s(data,x, y, group,xlab='', ylab='Monthly Expenses, Dollar', label.size=3.5,
pattern.type=c('Unicode_\u266B', 'nwlines', 'bricks'),pattern.line.size=c(10, 5, 5),frame.size=1,
pattern.color=c('blue', 'green', 'white'),background.color=c('white', 'white', 'orange'),pixel=20, 
density=c(10, 10, 10),frame.color='black', legend.type='h', legend.h=10, legend.y.pos=0.49,
legend.pixel=10, legend.w=0.25, legend.x.pos=1.1, 
legend.label=c("Childcare", "Food", "Housing" ))+scale_y_continuous(limits = c(0, 6800))


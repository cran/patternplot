library(patternplot)
library(png)

#Example 1
data <- read.csv(system.file("extdata", "fruits.csv", package="patternplot"))
x<-data$Fruit
y<-data$Weight
group<-data$Store

pattern.type<-c('nwlines', 'blank', 'crosshatch')
pattern.color=c('black','black', 'black')
background.color=c('white','gray80', 'white')
frame.color=c('black', 'black', 'black')
density<-c(6, 1, 6)
patternboxplot(data,x, y,group=NULL,pattern.type=pattern.type,
               pattern.color=pattern.color, background.color=background.color,
               frame.color=frame.color, density=density)

#Example 2
pattern.color=c('black','white', 'grey20')
background.color=c('lightgreen','lightgreen', 'lightgreen')
patternboxplot(data,x, y,group=NULL,pattern.type=pattern.type,
               pattern.color=pattern.color, background.color=background.color,
               frame.color=frame.color, density=density)


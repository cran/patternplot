#Example 1
library(patternplot)
library(png)
library(ggplot2)
data <- read.csv(system.file("extdata", "vegetables.csv", package="patternplot"))
pattern.type<-c('hdashes', 'vdashes', 'bricks')
patternpie(group=data$group,pct=data$pct,label=data$label, label.size=4,label.color='black',
           label.distance=1.2,pattern.type=pattern.type,pattern.line.size=c(5, 5, 2), 
           frame.color='black',frame.size=1.5, pixel=12, density=c(12, 12, 10))

#Example 2
pattern.color<-c('red3','green3', 'white' )
background.color<-c('dodgerblue', 'lightpink', 'orange')
patternpie(group=data$group,pct=data$pct,label=data$label, pattern.type=pattern.type,
pattern.color=pattern.color,background.color=background.color, pattern.line.size=c(5, 5, 2), 
           frame.color='grey40',frame.size=1.5, pixel=12, density=c(12, 12, 10))

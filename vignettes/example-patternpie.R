##Piecharts in black and white
#A simple example to start with. 
library(patternplot)
group<-c('A', 'B')
pct<-c(25, 75)
label<-paste(group, ' ',pct,'%', sep='' )
pattern.type<-c('hlines', 'grid')
patternpie(group=group,pct=pct,label =label, pattern.type=pattern.type,
           pixel=0.3, pattern.line.size=0.2, frame.size=1.5)

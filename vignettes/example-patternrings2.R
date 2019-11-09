library(patternplot)
library(png)
library(ggplot2)

group1<-c("Wind", "Hydro", "Solar", "Coal", "Natural Gas", "Oil")
pct1<-c(12, 15, 8, 22, 18, 25)
label1<-paste(group1, " \n ", pct1 , "%", sep="")

group2<-c("Renewable", "Non-Renewable")
pct2<-c(35, 65)
label2<-paste(group2, " \n ", pct2 , "%", sep="")

pattern.type1<-rep(c( "blank"), times=6)
pattern.type2<-c('grid', 'blank')
pattern.type.inner<-"blank"
pattern.color1<-rep('white', length(group1))
pattern.color2<-rep('white', length(group2))

background.color1<-c("darkolivegreen1", "white", "indianred",
                     "gray81",  "white", "sandybrown" )
background.color2<-c("seagreen", "deepskyblue")

density1<-rep(10, length(group1))
density2<-rep(10, length(group2))

pattern.line.size1=rep(6, length(group1))
pattern.line.size2=rep(2, length(group2))
pattern.line.size.inner=1

#Example 1: Two rings
g<-patternrings2(group1, group2, pct1,pct2, label1, label2, 
label.size1=3, label.size2=3.5, label.color1='black', label.color2='black',
label.distance1=0.75, label.distance2=1.4, pattern.type1, pattern.type2,  
pattern.color1,pattern.color2,pattern.line.size1, pattern.line.size2, 
background.color1, background.color2,density1=rep(10, length(group1)), 
density2=rep(15, length(group2)),pixel=10, pattern.type.inner, pattern.color.inner="black",
pattern.line.size.inner,  background.color.inner="white",  pixel.inner=6,  
density.inner=5, frame.color='black',frame.size=1,r1=2.45, r2=4.25, r3=5)
g<-g+annotate(geom="text", x=0, y=0, label="Earth's Energy",color="black",size=5)
g<-g+scale_x_continuous(limits=c(-6, 6))+scale_y_continuous(limits=c(-6, 6))
g

#Example 2: Pie in a ring
g<-patternrings2(group1, group2, pct1,pct2, label1, label2, label.size1=3, label.size2=3.5,
label.color1='black', label.color2='black', label.distance1=0.7, label.distance2=1.4, 
pattern.type1, pattern.type2,  pattern.color1,pattern.color2,pattern.line.size1, 
pattern.line.size2, background.color1, background.color2,density1=rep(10, length(group1)), 
density2=rep(15, length(group2)),pixel=10, pattern.type.inner, pattern.color.inner="black",
pattern.line.size.inner,  background.color.inner="white",  pixel.inner=1,  density.inner=2, 
frame.color='black',frame.size=1, r1=0.005, r2=4, r3=4.75)
g<-g+scale_x_continuous(limits=c(-6, 6))+scale_y_continuous(limits=c(-6, 6))
g
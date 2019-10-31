#Example 1
library(patternplot)
library(png)
library(ggplot2)
group1<-c("Wind", "Hydro", "Solar", "Coal", "Natural Gas", "Oil")
pct1<-c(12, 15, 8, 22, 18, 25)
label1<-paste(group1, " \n ", pct1 , "%", sep="")
location<-gsub('\\','/',tempdir(), fixed=TRUE)
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="darkolivegreen1",  pixel=20, res=15)
Wind<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="white", pixel=20, res=15)
Hydro<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="indianred",  pixel=20, res=15)
Solar<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="gray81",  pixel=20, res=15)
Coal<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="white",  pixel=20, res=15)
NaturalGas<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="sandybrown",  pixel=20, res=15)
Oil<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern.type1<-list(Wind, Hydro, Solar, Coal, NaturalGas, Oil)

group2<-c("Renewable", "Non-Renewable")
pct2<-c(35, 65)
label2<-paste(group2, " \n ", pct2 , "%", sep="")
pattern(type="grid", density=12, color='white', pattern.line.size=5, 
background.color="seagreen", pixel=20, res=15)
Renewable<-readPNG(paste(location,'/',"grid",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="deepskyblue",  pixel=20, res=15)
NonRenewable<-readPNG(paste(location,'/',"blank",".png", sep=''))


pattern.type2<-list(Renewable, NonRenewable)
pattern.type.inner<-readPNG(system.file("img", "earth.png", package="patternplot"))

g<-imagerings2(group1, group2,pct1,pct2, label1, label2, label.size1=3, label.size2=3.5, 
label.color1='black', label.color2='black', label.distance1=0.7, label.distance2=1.3, 
pattern.type1, pattern.type2, pattern.type.inner, frame.color='skyblue',frame.size=1, 
r1=2.2, r2=4.2, r3=5)
g<-g+scale_x_continuous(limits=c(-7, 7))+scale_y_continuous(limits=c(-7, 7))
suppressWarnings(print(g))









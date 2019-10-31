#Example 1
library(patternplot)
library(png)
library(ggplot2)
location<-gsub('\\','/',tempdir(), fixed=TRUE)
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="darkgreen", pixel=8, res=8)
FarWest<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="darkcyan", pixel=8, res=8)
GreatLakes<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="chocolate", pixel=8, res=8)
Mideast<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="cadetblue1", pixel=8, res=8)
NewEngland<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="darkorchid", pixel=8, res=8)
Plains<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="yellowgreen", pixel=8, res=8)
RockyMountain<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="hotpink", pixel=8, res=8)
Southeast<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="lightslateblue", pixel=8, res=8)
Southwest <-readPNG(paste(location,'/',"blank",".png", sep=''))


group1<-c('New_England','Great_Lakes','Plains','Rocky_Mountain', 'Far_West',
'Southwest', 'Southeast', 'Mideast')
pct1<-c( 12, 11, 17, 15, 8, 11,  16,  10)
label1<-paste(group1, " \n ", pct1, "%", sep="")

pattern.type1<-list(NewEngland, GreatLakes,Plains,  RockyMountain, FarWest,
Southwest, Southeast,  Mideast)
pattern.type.inner<-readPNG(system.file("img", "USmap.png", package="patternplot"))

g<-imagering1(group1, pct1,  pattern.type1, pattern.type.inner, frame.color='black',
frame.size=1.2, r1=3, r2=4,label1, label.size1=4,label.color1='black', label.distance1=1.2)
g<-g+annotate(geom="text", x=0, y=-2, label="2019 Number of Cases \n N=1000",color="black", size=4)
g<-g+scale_x_continuous(limits=c(-5.5, 5.5))+scale_y_continuous(limits=c(-5.5, 5.5))
suppressWarnings(print(g))






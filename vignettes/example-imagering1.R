#Example 1
library(patternplot)
library(png)
library(ggplot2)
location<-gsub('\\','/',tempdir(), fixed=TRUE)
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="darkgreen",pixel=6, res=4)
FarWest<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="darkcyan",pixel=6, res=4)
GreatLakes<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="chocolate",pixel=6, res=4)
Mideast<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="cadetblue1",pixel=6, res=4)
NewEngland<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="darkorchid",pixel=6, res=4)
Plains<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="yellowgreen",pixel=6, res=4)
RockyMountain<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="hotpink",pixel=6, res=4)
Southeast<-readPNG(paste(location,'/',"blank",".png", sep=''))
pattern(type="blank", density=1, color='white', pattern.line.size=1, 
background.color="lightslateblue",pixel=6, res=4)
Southwest <-readPNG(paste(location,'/',"blank",".png", sep=''))


group1<-c('New_England','Great_Lakes','Plains','Rocky_Mountain', 'Far_West',
'Southwest', 'Southeast', 'Mideast')
pct1<-c( 12, 11, 17, 15, 8, 11,  16,  10)
label1<-paste(group1, " \n ", pct1, "%", sep="")

pattern.type1<-list(NewEngland, GreatLakes,Plains,  RockyMountain, FarWest,
Southwest, Southeast,  Mideast)
pattern.type.inner<-readPNG(system.file("img", "USmap.png", package="patternplot"))

imagering1(group1, pct1,  pattern.type1, pattern.type.inner, frame.color='black',
frame.size=1.5, r1=3, r2=4,label1, label.size1=4,label.color1='black', label.distance1=1.2)




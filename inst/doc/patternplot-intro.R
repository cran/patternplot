## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(Cairo)
opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center", dev='CairoPNG')

## ----e1------------------------------------------------------------------
library(patternplot)
group<-c('A', 'B', 'C', 'D', 'E')
pct<-c(20, 20, 20, 20, 20)
label<-paste(group, ' ',pct,'%', sep='' )
pattern.type<-c('hlines','nelines', 'grid', 'dots', 'nwlines')
p<-patternpie(group=group,pct=pct,label =label, pattern.type=pattern.type, pattern.line.size=0.5, pixel=1)
p

## ----e2------------------------------------------------------------------
pattern.type<-rep('blank',5)
background.color<-c('gray100','gray75', 'gray50', 'gray25', 'gray0')
p<-patternpie(group=group,pct=pct,label =label, pattern.type=pattern.type, background.color=background.color, pixel=1)
p

## ----e3------------------------------------------------------------------
pattern.type<-c('hlines','nelines', 'grid', 'dots', 'nwlines')
background.color<-c('yellow', 'orange','blue', 'violet', 'green')
pattern.color<-c('white', 'white','white', 'red', 'white')
p<-patternpie(group,pct,label,label.size=4, label.color='black',label.distance=1.2,pattern.type=pattern.type, pattern.color=pattern.color,
                  pattern.line.size=1, background.color=background.color, frame.color='black',frame.size=1, pixel=1)
p

## ----e4------------------------------------------------------------------
frame.color<-'white'
p<-patternpie(group,pct,label,label.size=4, label.color='black',label.distance=1.2,pattern.type=pattern.type, pattern.color=pattern.color,
                  pattern.line.size=1, background.color=background.color, frame.color=frame.color,frame.size=1, pixel=1)
p

## ----e5------------------------------------------------------------------
pattern.type<-rep('blank',5)
background.color<-c('yellow', 'orange','blue', 'violet', 'green')
p<-patternpie(group=group,pct=pct,label =label, pattern.type=pattern.type, background.color=background.color, frame.color='white', pixel=1)
p

## ----e6------------------------------------------------------------------
#Fill slices with png images.
library(png)
Lines <-  readPNG(system.file("img", "lines.png", package="patternplot"))
Chessboard <-  readPNG(system.file("img", "chessboard.png", package="patternplot"))
Dots <-  readPNG(system.file("img", "dots.png", package="patternplot"))

pattern.type<-list(Chessboard, Lines,Dots)
group<-c('chessboard','Lines',  'Dots')
pct<-c(25, 40,  35)
label<-c("Chessboard 25% ", "Lines 40%",  "Dots 35%")

p<-imagepie(group=group,pct=pct,label=label,pattern.type=pattern.type,label.distance=1.35, 
              frame.color='black')
p


## ----e7------------------------------------------------------------------
library(jpeg)
Tomatoes <-  readJPEG(system.file("img", "tomatoes.jpg", package="patternplot"))
Peas <- readJPEG(system.file("img", "peas.jpg", package="patternplot"))
Peppers <-  readJPEG(system.file("img", "peppers.jpg", package="patternplot"))

pattern.type<-list(Tomatoes,Peas,Peppers)
group<-c('Tomatoes', 'Peas', 'Peppers')
pct<-c(40, 25, 35)
label<-c("Tomatoes \n 40%", "Peas \n 25% ", "Peppers \n 35%")

p<-imagepie(group=group,pct=pct,label=label,pattern.type=pattern.type,label.distance=1.35, 
              frame.color='burlywood4', frame.size=0.8, label.size=6, label.color='forestgreen')
p



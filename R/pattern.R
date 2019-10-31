#' Generate a pattern in png format.
#'
#' The \code{pattern} function is a function for generating a pattern in png format. 
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot 
#' @param type pattern types include: 'blank', 'bricks', 'vdashes', 'hdashes', 'crosshatch','dots',
#' 'grid','hlines','nelines','nwlines','vlines','waves','Rsymbol_0' to 'Rsymbol_25', and unicode symbols.  
#' @param density a numeric number, the density for the lines/dots of a pattern.
#' @param pattern.line.size a numeric value, the line size for the lines/dots of a pattern.
#' @param color color for the lines/dots of pattern.
#' @param background.color color to be filled in the background.
#' @param pixel a numeric value, the pixel resolution of the pattern. 
#' @param res a numeric value, the pixel resolution of the pattern. 
#' @return  A ggplot object.
#'
#' @details \code{pattern} function generates a pattern in png format.
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)

pattern<-function(type='bricks', density=8, pattern.line.size=10,color='black', background.color='white', pixel=5, res=30){

outputlocation<-gsub('\\\\\\\\','/',tempdir())

if(type=='blank'){
  png(paste(outputlocation,'/', 'blank.png', sep=""), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot.new()
  dev.off()
}

if (sub( "_.*", "", type)=='Unicode'){
var1<-rep(seq(1, density, by=1), density/2)
var1<-sort(var1)
var2<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
#http://www.alanwood.net/demos/wgl4.html#w25A0 #use Hex column

png(paste(outputlocation,'/', "Unicode",'.png', sep=''), width = pixel, height =pixel, units="in", res=res)
par(mar=rep(0, 4), bg=background.color)
plot(var1, var2, pch =gsub( "Unicode_", "", type), frame.plot=F, axes=FALSE,  cex = pattern.line.size, col=color, lwd=pattern.line.size) 
dev.off()

}


if (type=='hdashes'){
  var1<-rep(seq(1, density, by=1), density/2)
  var1<-sort(var1)
  var2<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
  
  png(paste(outputlocation,'/','hdashes.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(var1, var2, pch = sprintf("\u2015"), frame.plot=F, axes=FALSE,  cex = pattern.line.size, col=color) 
  dev.off()
}


if (type=='vdashes'){
  var1<-rep(seq(1, density, by=1), density/2)
  var1<-sort(var1)
  var2<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
  
  png(paste(outputlocation,'/','vdashes.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  #plot(var1, var2, pch = sprintf("\u266a"), frame.plot=F, axes=FALSE,  cex = pattern.line.size, col=color) 
  plot(var1, var2, pch = sprintf("\u2502"), frame.plot=F, axes=FALSE,  cex = pattern.line.size, col=color) 
  dev.off()
}

if(type=='grid'){
  png(paste(outputlocation,'/','grid.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(c(0:density), c(0:density), col = rgb(red = 1, green = 0, blue = 0, alpha = 0))
  abline(h = 0:density, v =0:density, col = color, lty = 1, lwd=pattern.line.size)
  dev.off()
  }


if(type=='vlines'){
  png(paste(outputlocation,'/','vlines.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(c(0, density), c(0, density), col = rgb(red = 1, green = 0, blue = 0, alpha = 0))
  abline(v =0:density, col = color, lty = 1, lwd = pattern.line.size)
  dev.off()
}

if(type=='hlines'){
  png(paste(outputlocation,'/','hlines.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(c(0, density), c(0, density), col = rgb(red = 1, green = 0, blue = 0, alpha = 0))
  abline(h =0:density, col = color, lty = 1, lwd = pattern.line.size)
  dev.off()
}

#http://media.clemson.edu/economics/faculty/wilson/R-tutorial/graphics.html
if(type=='waves'){
  x <- seq(0, density*pi,length.out=1000)
  y <- sin(x)

  png(paste(outputlocation,'/','waves.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(c(min(x)+pi, max(x)-pi), c(min(y)+pi, max(y+(density-1)*pi)), col = rgb(red = 1, green = 0, blue = 0, alpha = 0))
  for (i in 0:(density)){
    lines(x, y+i*pi, col=color, lwd = pattern.line.size)
  }
  dev.off()
}

if(type=='bricks'){
  x<-rep(seq(1, density, by=1), density/2)
  x<-sort(x)
  y<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
  
  png(paste(outputlocation,'/','bricks.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(c(min(x), max(x)), c(min(y), max(y)-1), col = rgb(red = 1, green = 0, blue = 0, alpha = 0))
  segments(x, y-1, x1 = x, y1 = y,  col=color, lwd = pattern.line.size)
  abline(h =unique(x), col = color, lty = 1, lwd = pattern.line.size)
  
  dev.off()
}



if(type=='nelines'){
  png(paste(outputlocation,'/','nelines.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(c(0, density), c(0, density), col = rgb(red = 1, green = 0, blue = 0, alpha = 0))
  for (i in -density:density){
    abline(i, 1 , col = color, lty = 1, lwd = pattern.line.size) 
  }
  dev.off()
}

if(type=='nwlines'){
  png(paste(outputlocation,'/','nwlines.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(c(0, density), c(0, density), col = rgb(red = 1, green = 0, blue = 0, alpha = 0))
  for (i in seq(0, 2*density, by=1)){
    abline(i, -1 , col = color, lty = 1, lwd = pattern.line.size) 
  }
  dev.off()
}

if(type=='crosshatch'){
  png(paste(outputlocation,'/','crosshatch.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(c(0, density), c(0, density), col = rgb(red = 1, green = 0, blue = 0, alpha = 0))
  for (i in -density:density){
    abline(i, 1 , col = color, lty = 1, lwd = pattern.line.size) 
  }
  for (i in seq(0, 2*density, by=1)){
    abline(i, -1 , col = color, lty = 1, lwd = pattern.line.size) 
  }
  dev.off()
}


if(sub( "_.*", "", type)=="Rsymbol"){
  var1<-rep(seq(1, density, by=1), density/2)
  var1<-sort(var1)
  var2<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
  png(paste(outputlocation,'/',type,'.png', sep=''), width = pixel, height =pixel, units="in", res=res)
  par(mar=rep(0, 4), bg=background.color)
  plot(var1, var2, pch =as.numeric(gsub( "Rsymbol_", "", type)), frame.plot=F, axes=FALSE,  cex = pattern.line.size, col=color, lwd=pattern.line.size) 
  dev.off()
}
}

pattern(type='Unicode_\u266A', density=8, pattern.line.size=10,color='red', background.color='green', pixel=10, res=30)








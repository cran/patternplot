#' Plot a ring chart using patterns and colors to fill the rings.
#'
#' The \code{patternrings2} function is a tool for creating versatile ring charts 
#' by filling the rings with colors and patterns.
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot
#' @param group1 a vector of strings, containing the names of each slice for the inner ring. 
#' @param group2 a vector of strings, containing the names of each slice for the outer ring. 
#' @param pct1 a vector of non-negative numbers, containing percentages of each group for the inner ring.
#' The numbers must sum up to 100.
#' @param pct2 a vector of non-negative numbers, containing percentages of each group for the outer ring.
#' The numbers must sum up to 100.
#' @param label1 a vector of strings, giving the names for the slices shown in the ring chart for the inner ring. 
#' @param label2 a vector of strings, giving the names for the slices shown in the ring chart for the outer ring.   
#' @param label.size1 the font size of labels shown in the ring chart for the inner ring. 
#' @param label.size2 the font size of labels shown in the ring chart for the outer ring.   
#' @param label.color1 the color of labels shown in the ring chart for the inner ring.  
#' @param label.color2 the color of labels shown in the ring chart for the outer ring.  
#' @param label.distance1 the distance of labels from the border of the ring chart for the inner ring. 
#' @param label.distance2 the distance of labels from the border of the ring chart for the outer ring. 
#' @param pattern.type1 a vector of patterns to be filled in the slices for the inner ring. 
#' The pattern types include: 'blank', 'bricks', 'vdashes', 'hdashes', 'crosshatch','dots', 
#' 'grid','hlines','nelines', 'nwlines', 'vlines', 'waves' and more. 
#' @param pattern.type2 a vector of patterns to be filled in the slices for the outer ring. 
#' The pattern types include: 'blank', 'bricks', 'vdashes', 'hdashes', 'crosshatch','dots', 
#' 'grid','hlines','nelines', 'nwlines', 'vlines', 'waves' and more. 
#' @param pattern.color1 a vector of colors for the lines/dots of patterns for the inner ring.
#' @param pattern.color2 a vector of colors for the lines/dots of patterns for the outer ring.
#' @param pattern.line.size1 a vector of numeric values, the line size for the lines/dots of patterns for the inner ring.
#' @param pattern.line.size2 a vector of numeric values, the line size for the lines/dots of patterns for the outer ring.
#' @param background.color1 a vector of colors to be filled in the slices for the inner ring.
#' @param background.color2 a vector of colors to be filled in the slices for the outer ring.
#' @param density1 a numeric vector, the density for the lines/dots of patterns for the inner ring.
#' @param density2 a numeric vector, the density for the lines/dots of patterns for the outer ring.
#' @param pixel a numeric value, the pixel resolution of ring chart. 
#' @param pattern.type.inner a pattern to be filled in the inner circle.
#' @param pattern.color.inner the color for the lines/dots of the pattern.
#' @param pattern.line.size.inner the line size for the lines/dots of the pattern.
#' @param background.color.inner the color to be filled in the inner circle.
#' @param pixel.inner a numeric value, the pixel resolution of the inner circle.
#' @param density.inner a numeric vector, the density for the lines/dots of patterns of the inner circle.
#' @param frame.color the color for the borders of slices.
#' @param frame.size a numeric value, the line size for the borders of slices.
#' @param r1 a numeric value, the inner radius of the inner ring. 
#' @param r2 a numeric value, the outer radius of the inner ring. 
#' @param r3 a numeric value, the outer radius of the outer ring. 
#' 
#' @return  A ggplot object.
#'
#' @details \code{patternrings2} function offers flexible ways of doing ring charts. 
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)
#'
#' @seealso Function \code{imagerings2}  
#'
#' @example vignettes/example-patternrings2.R

patternrings2<-function(group1, group2, pct1,pct2, label1, label2, label.size1=4, label.size2=4, label.color1='black', label.color2='black',label.distance1=1.2, label.distance2=1.2,
                        pattern.type1, pattern.type2, pattern.color1,pattern.color2, pattern.line.size1=rep(10, length(group1)), pattern.line.size2=rep(10, length(group2)), background.color1, background.color2,density1=rep(10, length(group1)), density2=rep(10, length(group2)),pixel=20,
                        pattern.type.inner="blank", pattern.color.inner="white", pattern.line.size.inner=1, background.color.inner="white", pixel.inner=10,  density.inner=1, frame.color='black',frame.size=1,  r1, r2, r3){
  
  #pie from data
  start1<-0.02*c(0, head(cumsum(pct1), -1))
  end1<-0.02*cumsum(pct1)
  start2<-0.02*c(0, head(cumsum(pct2), -1))
  end2<-0.02*cumsum(pct2)
 
  #label position
  ringlabel1<- labelposition(r=r2, start=start1, end=end1, distance=label.distance1)
  ringlabel2<- labelposition(r=r2, start=start2, end=end2, distance=label.distance2)
  
  
  #draw and read slice patterns
  location<-gsub('\\','/',tempdir(), fixed=T)
  
  ringdata1<-list()
  piedata1<-list()
  picdata1<-list()
  picdf1<-list()
  for (i in 1:length(group1)){
    ringdata1[[i]] <-ringpos(r1=r2,r2=r1,start=start1[i], end=end1[i])
    piedata1[[i]] <-slicepos(r=r2,start=start1[i], end=end1[i])
    suppressWarnings(pattern(type=pattern.type1[i], density=density1[i], color=pattern.color1[i], pattern.line.size=pattern.line.size1[i], background.color=background.color1[i], pixel=pixel, res=pixel))
    if (sub( "_.*", "", pattern.type1[i])=='Unicode'){
      picdata1[[i]]<-imagetodf3(readPNG(paste(location,'/','Unicode',".png", sep='')), as.matrix(piedata1[[i]]),r2,r1,left = min(piedata1[[i]]$x), right = max(piedata1[[i]]$x),bottom = min(piedata1[[i]]$y),top = max(piedata1[[i]]$y))
    }else{
      picdata1[[i]]<-imagetodf3(readPNG(paste(location,'/',pattern.type1[i],".png", sep='')), as.matrix(piedata1[[i]]),r2,r1,left = min(piedata1[[i]]$x), right = max(piedata1[[i]]$x),bottom = min(piedata1[[i]]$y),top = max(piedata1[[i]]$y))
    }
    
    picdf1[[i]] <- filter(picdata1[[i]], pos==1)
    picdata1[[i]]<-NULL
  }
  
  ringdata2<-list()
  piedata2<-list()
  picdata2<-list()
  picdf2<-list()
  for (i in 1:length(group2)){
    ringdata2[[i]] <-ringpos(r1=r3,r2=r2,start=start2[i], end=end2[i])
    piedata2[[i]] <-slicepos(r=r3,start=start2[i], end=end2[i])
    suppressWarnings(pattern(type=pattern.type2[i], density=density2[i], color=pattern.color2[i], pattern.line.size=pattern.line.size2[i], background.color=background.color2[i], pixel=pixel, res=pixel))
    if (sub( "_.*", "", pattern.type2[i])=='Unicode'){
      picdata2[[i]]<-imagetodf3(readPNG(paste(location,'/','Unicode',".png", sep='')), as.matrix(piedata2[[i]]),r3,r2,left = min(piedata2[[i]]$x), right = max(piedata2[[i]]$x),bottom = min(piedata2[[i]]$y),top = max(piedata2[[i]]$y))
    }else{
      picdata2[[i]]<-imagetodf3(readPNG(paste(location,'/',pattern.type2[i],".png", sep='')), as.matrix(piedata2[[i]]),r3,r2,left = min(piedata2[[i]]$x), right = max(piedata2[[i]]$x),bottom = min(piedata2[[i]]$y),top = max(piedata2[[i]]$y))
    }
   
    picdf2[[i]] <- filter(picdata2[[i]], pos==1)
    picdata2[[i]]<-NULL
  }
  
  #Draw inner circle
  innercdf<-innercircle(r3=r1)
  suppressWarnings(pattern(type=pattern.type.inner, density=density.inner, color=pattern.color.inner, pattern.line.size=pattern.line.size.inner, background.color=background.color.inner, pixel=pixel.inner, res=pixel.inner))
  if (sub( "_.*", "", pattern.type.inner)=='Unicode'){
    innercdata<-imagetodf4(readPNG(paste(location,'/','Unicode',".png", sep='')), as.matrix(innercdf),r1, left = min(innercdf$x), right = max(innercdf$x),bottom = min(innercdf$y),top = max(innercdf$y))
  }else{
    innercdata<-imagetodf4(readPNG(paste(location,'/',pattern.type.inner,".png", sep='')), as.matrix(innercdf),r1, left = min(innercdf$x), right = max(innercdf$x),bottom = min(innercdf$y),top = max(innercdf$y))
  }
  innercdata <- filter(innercdata, pos==1)
  innercdf<-NULL

  X<-Y<-r<-g<-b<-a<-pos<-x<-y<-NULL
  
  #generate black background
  blank_theme <- theme_minimal()+theme(axis.title = element_blank(),
                                       axis.text = element_blank(),
                                       panel.border = element_blank(),
                                       panel.grid=element_blank(),
                                       axis.ticks = element_blank()
  )
  
  ggplot() + mapply(function(i) geom_tile(data = picdf1[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))), i=1:length(group1)) + mapply(function(i) geom_tile(data = picdf2[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))), i=1:length(group2))+geom_tile(data = innercdata, aes(x = X, y = Y, fill = rgb(r,g, b,a)))+scale_fill_identity()+mapply(function(i) geom_polygon(data=ringdata1[[i]], aes(x=x,y=y), color=frame.color, size=frame.size, fill=NA), i=1:length(group1)) +mapply(function(i) geom_polygon(data=ringdata2[[i]], aes(x=x,y=y), color=frame.color, size=frame.size, fill=NA), i=1:length(group2)) +coord_equal()+blank_theme +annotate("text", label =label2, x = ringlabel2$x, y = ringlabel2$y, size = label.size2, colour = label.color2)+annotate("text", label =label1, x = ringlabel1$x, y = ringlabel1$y, size = label.size1, colour = label.color1)
 
}







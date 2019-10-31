#' Plot a ring chart using patterns and colors to fill the ring.
#'
#' The \code{patternring1} function is a tool for creating versatile ring charts 
#' by filling the ring with colors and patterns.
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot
#' @param group1 a vector of strings, containing the names of each slice. 
#' @param pct1 a vector of non-negative numbers, containing percentages of each group.
#' The numbers must sum up to 100.
#' @param label1 a vector of strings, giving the names for the slices shown in the ring chart.   
#' @param label.size1 the font size of labels shown in the ring chart.   
#' @param label.color1 the color of labels shown in the ring chart.   
#' @param label.distance1 the distance of labels from the border of the ring chart. 
#' @param pattern.type1 a vector of patterns to be filled in the ring. 
#' The pattern types include: 'blank', 'bricks', 'vdashes', 'hdashes', 'crosshatch','dots', 
#' 'grid','hlines','nelines', 'nwlines', 'vlines', 'waves' and more. 
#' @param pattern.color1 a vector of colors for the lines/dots of patterns filled in the ring.
#' @param pattern.line.size1 a vector of numeric values, the line size for the lines/dots of patterns filled in the ring.
#' @param background.color1 a vector of colors to be filled in the ring.
#' @param frame.color the color for the borders of the ring.
#' @param frame.size a numeric value, the line size for the borders of the ring.
#' @param density1 a numeric vector, the density for the lines/dots of patterns of the ring.
#' @param pixel a numeric value, the pixel resolution of the ring.  
#' @param pattern.type.inner a pattern to be filled in the inner circle.
#' @param pattern.color.inner the color for the lines/dots of the pattern of the inner circle.
#' @param pattern.line.size.inner the line size for the lines/dots of the pattern of the inner circle.
#' @param background.color.inner the color to be filled in the inner circle.
#' @param pixel.inner a numeric value, the pixel resolution of the inner circle.
#' @param density.inner a numeric vector, the density for the lines/dots of patterns of the inner circle.
#' @param r1 a numeric value, the inner radius of the ring. 
#' @param r2 a numeric value, the outer radius of the ring. 
#' 
#' @return  A ggplot object.
#'
#' @details \code{patternring1} function offers flexible ways of doing ring charts. 
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)
#'
#' @seealso Function \code{imagering1}  
#'
#' @example vignettes/example-patternring1.R

patternring1<-function(group1, pct1, label1, label.size1=4,label.color1='black', label.distance1=1.2, pattern.type1, pattern.color1, pattern.line.size1=rep(10, length(group1)),background.color1, 
                       frame.color='black',frame.size=1, density1=rep(10, length(group1)), pixel=20,
                       pattern.type.inner="blank",pattern.color.inner="white", pattern.line.size.inner=1,  background.color.inner="white", pixel.inner=10, density.inner=1, r1, r2){
  
  #ring from data
  start1<-0.02*c(0, head(cumsum(pct1), -1))
  end1<-0.02*cumsum(pct1)

  #label position
  pielabel<- labelposition(r=r2, start=start1, end=end1, distance=label.distance1)
  
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
  ggplot() + mapply(function(i) geom_raster(data = picdf1[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))), i=1:length(group1))+geom_raster(data = innercdata, aes(x = X, y = Y, fill = rgb(r,g, b,a)))+scale_fill_identity()+mapply(function(i) geom_polygon(data=ringdata1[[i]], aes(x=x,y=y), color=frame.color, size=frame.size, fill=NA), i=1:length(group1)) +coord_equal()+blank_theme +annotate("text", label =label1, x = pielabel$x, y = pielabel$y, size = label.size1, colour = label.color1) 
}




#' Plot a ring chart using images to fill the ring.
#'
#' The \code{imagering1} function is a tool for creating versatile ring charts 
#' by filling the ring with images.
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot
#' @param group1 a vector of strings, containing the names of each slice. 
#' @param pct1 a vector of non-negative numbers, containing percentages of each group.
#' The numbers must sum up to 100.
#' @param pattern.type1 a list of objects returned by \code{readPNG} and \code{readJPEG} used to fill the ring. 
#' @param pattern.type.inner an object returned by \code{readPNG} and \code{readJPEG} used to fill the inner circle. 
#' @param frame.color the color for the borders of the ring.
#' @param frame.size a numeric value, the line size for the borders of the ring.
#' @param label1 a vector of strings, giving the names for the slices shown in the ring chart.   
#' @param label.size1 the font size of labels shown in the ring chart.   
#' @param label.color1 the color of labels shown in the ring chart.   
#' @param label.distance1 the distance of labels from the border of the ring chart. 
#' @param r1 a numeric value, the inner radius of the ring. 
#' @param r2 a numeric value, the outer radius of the ring. 
#' 
#' @return  A ggplot object.
#'
#' @details \code{imagering1} function offers flexible ways of doing ring charts. 
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)
#'
#' @seealso Function \code{patternring1}  
#'
#' @example vignettes/example-imagering1.R

imagering1<-function(group1, pct1, pattern.type1, pattern.type.inner, frame.color='black',frame.size=1,label1, label.size1=4,label.color1='black', label.distance1=1.2, r1=3, r2=4){
  
  #pie from data
  start1<-0.02*c(0, head(cumsum(pct1), -1))
  end1<-0.02*cumsum(pct1)
 
  #label position
  ringlabel1<- labelposition(r=r2, start=start1, end=end1, distance=label.distance1)
  
  imagedata1<-list()
  piedata1<-list()
  picdata1<-list()
  picdf1<-list()
  for (i in 1:length(group1)){
    imagedata1[[i]] <-ringpos(r1=r2,r2=r1,start=start1[i], end=end1[i])
    piedata1[[i]] <-slicepos(r=r2,start=start1[i], end=end1[i])
    picdata1[[i]]<-imagetodf3(pattern.type1[[i]], as.matrix(piedata1[[i]]),r2,r1,left = min(piedata1[[i]]$x), right = max(piedata1[[i]]$x),bottom = min(piedata1[[i]]$y),top = max(piedata1[[i]]$y))
    picdf1[[i]] <- filter(picdata1[[i]], pos==1)
    picdata1[[i]]<-NULL
  }
  
  #Draw inner circle
  innercdf<-innercircle(r3=r1)
  innercdata<-imagetodf4(pattern.type.inner, as.matrix(innercdf),r1, left = min(innercdf$x), right = max(innercdf$x),bottom = min(innercdf$y),top = max(innercdf$y))
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
  ggplot() + mapply(function(i) suppressWarnings(geom_raster(data = picdf1[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a)))), i=1:length(group1))+suppressWarnings(geom_raster(data = innercdata, aes(x = X, y = Y, fill = rgb(r,g, b,a))))+scale_fill_identity()+mapply(function(i) geom_polygon(data=imagedata1[[i]], aes(x=x,y=y), color=frame.color, size=frame.size, fill=NA), i=1:length(group1)) +coord_equal()+blank_theme +annotate("text", label =label1, x = ringlabel1$x, y = ringlabel1$y, size = label.size1, colour = label.color1)
}



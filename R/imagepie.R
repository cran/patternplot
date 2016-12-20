#' Plot a pie chart with slices filled with png and jpeg images.
#'
#' The \code{imagepie} function is a tool for creating versatile pie charts 
#' by filling the slices with external png and jpeg images. It can create 
#' either black and white pie charts which are useful for scientific publications, 
#' or colorful pie charts which are good for presentations. 
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot 
#' @param group a vector of strings, containing the names of each slice. 
#' @param pct a vector of non-negative numbers, containing percentages of each group.
#' The numbers must sum up to 100.
#' @param label a vector of strings, giving the names for the slices shown in the pie chart.   
#' @param label.size the font size of labels shown in the pie chart.   
#' @param label.color the color of labels shown in the pie chart.   
#' @param label.distance the distance of labels from the border of the pie chart. 
#' @param pattern.type a list of objects returned by \code{readPNG} and \code{readJPEG} used to fill slices. 
#' @param frame.color the color for the borders of slices.
#' @param frame.size a numeric value, the line size for the borders of slices.
#' 
#' @return  A ggplot object.
#'
#' @details \code{imagepie} function offers flexible ways in doing pie charts.
#'   
#' @author Chunqiao Luo (cluo@@uams.edu)
#'
#' @seealso Function \code{patternpie}  
#'
#' @example vignettes/example-imagepie.R



imagepie<-function(group,pct,label,label.size=4,label.color='black', label.distance=1.35,pattern.type, frame.color='black',frame.size=1){

  #pie frame data
  start<-0.02*c(0, head(cumsum(pct), -1))
  end<-0.02*cumsum(pct)
  
  #label position
  pielabel<- labelpos(r=5, start=start, end=end, distance=label.distance)
  
  #slice position
  piedata<-list()
  picdata<-list()
  picdf<-list()

  for (i in 1:length(group)){
  piedata[[i]]<-slicepos(r=5,start=start[i], end=end[i])
  picdata[[i]]<-imagetodf(pattern.type[[i]], as.matrix(piedata[[i]]),left = min(piedata[[i]]$x), right = max(piedata[[i]]$x),bottom = min(piedata[[i]]$y),top = max(piedata[[i]]$y))
  picdf[[i]]<-filter(picdata[[i]], pos==1)
  picdata[[i]]<-NULL
  }
  pattern.type<-NULL
  X<-Y<-r<-g<-b<-a<-pos<-x<-y<-NULL
  #generate black background
  blank_theme <- theme_minimal()+
    theme( axis.title = element_blank(),
           axis.text = element_blank(),
           panel.border = element_blank(),
           panel.grid=element_blank(),
           axis.ticks = element_blank()
    )
  
  ggplot() + mapply(function(i) geom_raster(data = picdf[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:length(group))+scale_fill_identity()+ mapply(function(i) geom_polygon(data=piedata[[i]], aes(x=x,y=y), color=frame.color, size=frame.size, fill=NA),1:length(group)) +coord_equal() +annotate("text", label =label, x = pielabel$x, y = pielabel$y, size = label.size, colour = label.color)+scale_x_continuous(limits=c(-7.0, 7.0))+scale_y_continuous(limits=c(-7.0, 7.0))+blank_theme
}


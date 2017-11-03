#' Plot a pie chart using patterns and colors to fill the slices.
#'
#' The \code{patternpie} function is a tool for creating versatile pie charts 
#' by filling the slices with colors and patterns.
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot
#' @param group a vector of strings, containing the names of each slice. 
#' @param pct a vector of non-negative numbers, containing percentages of each group.
#' The numbers must sum up to 100.
#' @param label a vector of strings, giving the names for the slices shown in the pie chart.   
#' @param label.size the font size of labels shown in the pie chart.   
#' @param label.color the color of labels shown in the pie chart.   
#' @param label.distance the distance of labels from the border of the pie chart. 
#' @param pattern.type a vector of patterns to be filled in the slices. 
#' There are 15 pattern types: 'blank', 'bricks', 'circles1','circles2', 'vdashes', 'hdashes',
#' 'crosshatch','dots', 'grid','hlines','nelines', 'nwlines', 'shells', 'vlines', 'waves'. 
#' @param pattern.color a vector of colors for the lines/dots of patterns.
#' @param pattern.line.size a numeric value, the line size for the lines/dots of patterns.
#' @param background.color a vector of colors to be filled in the slices.
#' @param frame.color the color for the borders of slices.
#' @param frame.size a numeric value, the line size for the borders of slices.
#' @param pixel a numeric value, the pixel resolution of pie chart. 
#' @param density a numeric vector, the density for the lines/dots of patterns. 
#' 
#' @return  A ggplot object.
#'
#' @details \code{patternpie} function offers flexible ways in doing pie charts. 
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)
#'
#' @seealso Function \code{imagepie}  
#'
#' @example vignettes/example-patternpie.R

patternpie<-function(group,pct,label,label.size=4, label.color='black',label.distance=1.2,pattern.type, pattern.color=rep('black', length(group)),
                    pattern.line.size=1, background.color=rep('white', length(group)), frame.color='black',frame.size=1, pixel=0.3, density=rep(10, length(group))){
  
  #pie frame data
  start<-0.02*c(0, head(cumsum(pct), -1))
  end<-0.02*cumsum(pct)
  
  #label position
  pielabel<- labelpos(r=5, start=start, end=end, distance=label.distance)
  
  #slice position
  #draw and read slice patterns
  location<-gsub('\\','/',tempdir(), fixed=T)
  
  piedata<-list()
  picdata<-list()
  picdf<-list()
    for (i in 1:length(group)){
      piedata[[i]] <- slicepos(r=5,start=start[i], end=end[i])
      suppressWarnings(pattern(type=pattern.type[i], density=density[i], color=pattern.color[i], pattern.line.size=pattern.line.size, background.color=background.color[i], pixel=pixel))
      picdata[[i]]<-imagetodf(readPNG(paste(location,'/',pattern.type[i],".png", sep='')), as.matrix(piedata[[i]]),left = min(piedata[[i]]$x), right = max(piedata[[i]]$x),bottom = min(piedata[[i]]$y),top = max(piedata[[i]]$y))
      picdf[[i]] <- filter(picdata[[i]], pos==1)
      picdata[[i]]<-NULL
    }
  X<-Y<-r<-g<-b<-a<-pos<-x<-y<-NULL
  #generate black background
  blank_theme <- theme_minimal()+theme(axis.title = element_blank(),
           axis.text = element_blank(),
           panel.border = element_blank(),
           panel.grid=element_blank(),
           axis.ticks = element_blank()
    )
  ggplot() + mapply(function(i) geom_raster(data = picdf[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))), i=1:length(group))+scale_fill_identity()+mapply(function(i) geom_polygon(data=piedata[[i]], aes(x=x,y=y), color=frame.color, size=frame.size, fill=NA), i=1:length(group)) +coord_equal()+annotate("text", label =label, x = pielabel$x, y = pielabel$y, size = label.size, colour = label.color)+scale_x_continuous(limits=c(-7.0, 7.0))+scale_y_continuous(limits=c(-7.0, 7.0))+blank_theme 
}


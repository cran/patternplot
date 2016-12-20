
labelpos <- function(r=0.5,start=start, end=end, distance=1.2){
  label.position <- pi*rowMeans(cbind(start, end))
  df <- data.frame(
    x = distance*r* cos(label.position),
    y = distance*r* sin(label.position)
  )
  df
}
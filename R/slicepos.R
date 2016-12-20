
slicepos <- function(r=0.5,start=0, end=2, distance=1.2, length=800){
  slice.position <- seq(start*pi, end*pi, length.out=length) 
  df <- data.frame(
    x = r*cos(slice.position),
    y = r*sin(slice.position)
  )
  df <- do.call(rbind, list(c(0,0),df, c(0, 0)))
  df
}


# convert a data to approx desity function
getDensityApproxfun = function(x){
  dd = density(as.vector(x))
  ff = approxfun(dd$x,dd$y)
  ff
}

plotContour = function(x,...){
  UseMethod("plotContour",x)
}

plotContour.Variable = function(var.tdata, 
                                    #cut.nticks = 100, 
                                    value.range = c(0,10),
                                    value.breaks = 1000,
                                    levels = "auto",
                                    levels.breaks = 10,
                                    add.mean = F,
                                    add.median = F,
                                    round.digit = 3,
                                    #main = NULL,
                                    ...){
 
  
  data = var.tdata
  ticks = getTicks(var.tdata)
  
  value = seq(value.range[1],value.range[2],(value.range[2] - value.range[1]) / value.breaks)
  # the plot 
  x = 1:length(ticks)
  y = 1:length(value)
  z = matrix(nrow=length(x),ncol = length(y))
  
  for(i in x){
    ff = getDensityApproxfun(t(data[i,]))
    density.value = ff(value)
    z[i,] = density.value
  }
  
  z.max = max(na.omit(as.vector(z)))
  z.min = min(na.omit(as.vector(z)))  
  inter = (z.max - z.min) / levels.breaks
  
  
  if(tolower(levels) == "auto"){
    mylevels <- seq(z.min,z.max,inter)
  }else{
    mylevels = levels
  } 

  mylevels = round(mylevels,round.digit)
  
  #if (is.null(main)) {main = paste0("Contour Line of Distrubution of ", attr(var.tdata,"var.name"))}
  contour(ticks,value,z,levels=mylevels, labcex =1, drawlabels = T, ...) 

  if(add.mean) lines(ticks,rowMeans(data),col="red")
  if(add.median) lines(ticks,apply(data,1,median),col="blue")
}

# 每一次NetLogo的运行，就生成一个TickData
library(dplyr)

TickData = function(x, ticks = NULL){
  
  if(!is.null(ticks)){
    attr(x,"ticks") = ticks
  }else if("ticks" %in% colnames(x)){
    ticks = x$ticks
    x = dplyr::select(x,-ticks)
    attr(x,"ticks") = ticks
  }else{
    stop("x must has a \"ticks\" col or ticks should be set")
  }
  
  rownames(x) = NULL
  class(x) = unique(c("TickData",class(x)))
  x
}

print.TickData = function(x, print.all = F, n = 5){
  #x = data.frame()
  cat("TickData:", ncol(x) ,"x", nrow(x),"\n")
  cat("Last tick:",max(getTicks(x)),"\n")
  if(print.all | nrow(x) <= n){
    print(data.frame(x))
  }else{
    print(data.frame(head(x,n)))
    cat("...\n")
  }
}

getTicks = function(x){
  UseMethod("getTicks", x)
}

getTicks.TickData = function(x){
  attr(x,"ticks")
}

filterByTicks = function(x, ticks){
  ii = getTicks(x) %in% ticks 
  TickData(x[ii,], getTicks(x)[ii])
}

cut.TickData = filterByTicks
cut.Variable = function(x,...){
  Variable(cut.TickData(x,...))
}



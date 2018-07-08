# 每一次（大型）实验，就生成一个MultiverseData
library(dplyr)

# plot MultiverseData =============



MultiverseData = function(x, name = ""){
  class(x) = unique(c("MultiverseData",class(x)))
  if(name!=""){attr(x,"name") = name}
  x
}


# print expt oject =============
print.MultiverseData = function(x) {
  
  last.tick = (sapply(x,function(xx){
    #print(class(xx))
    max(getTicks(xx))
  }))
  
  all.length = (sapply(x,function(xx){
    nrow(xx)
  }))
  
  if(length(attr(x,"set.id"))>0){
    cat("\nExperiments Set ID: ",attr(x,"set.id"),"\n")
    cat("Average time used: ", attr(x,"Avg.time.used"), "\n")
    cat("Total time used: ", attr(x,"Time.used"), "\n")

  }

  cat("\nMultiverse name: ",getName(x),"\n")
  
  cat("Number of worlds: ", length(x), "\n")
  cat("Data length: ", max(all.length), "\n")
  
  if(max(all.length) > min(all.length)){
    cat(crayon::inverse("\nWarning: Data have different size!\n"))
  }
  

  if(max(last.tick) > min(last.tick)){
    cat("Max last tick: ", max(last.tick),"\n")
    cat("Min last tick: ", min(last.tick),"\n")
    
    cat(crayon::inverse("\nWarning: Models end at different ticks! \n"))
  }else{
    cat("Last tick: ", max(last.tick),"\n")
  }
    
    
  
  cat("\nVariables: \n")
  print(colnames(x[[1]]))
}



# subseting a experiment by parames ====
subset.MultiverseData= function(x,subset,env=parent.frame()){
  e = substitute(subset)
  res = sapply(x,function(y){
    params = getParamsSet(y)
    eval(e,params,env)
  })
  
  
  if(all(!res)){return(NULL)}
  
  res2 = x[res]
  #class(res2) = c("MultiverseData",class(res2))
  MultiverseData(res2,getName(x))
}


# inner function of subset.MultiverseData
subset_.MultiverseData= function(x,e,env=parent.frame()){
  #e = substitute(subset)
  res = sapply(x,function(y){
    params = getParamsSet(y)
    eval(e,params,env)
  })
  
  if(all(!res)){return(NULL)}
  
  res2 = x[res]
  #class(res2) = c("MultiverseData",class(res2))
  MultiverseData(res2,getName(x))
}

# cut by ticks
cut.MultiverseData = function(x,ticks){
  expt.cutted = lapply(x, function(xx){
    xx[getTicks(xx) %in% ticks,]
  }) %>% MultiverseData(getName(x))
  
  attr(expt.cutted,"cut") = T
  expt.cutted
}

# ===========
getName = function(x,...){
  UseMethod("getName",x)
}

getName.MultiverseData = function(x){
  attr(x,"name")
}



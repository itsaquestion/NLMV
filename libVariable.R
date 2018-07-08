# gether a var from the result ====

Variable = function(x, ...){
  UseMethod("Variable",x)
}
 
Variable.TickData = function(x, var.name = NULL){
  class(x) = unique(c("Variable",class(x)))
  if(!is.null(var.name)){
    attr(x,"var.name") = var.name
    colnames(x) = paste0(var.name, "_", 1:ncol(x))
  }
  x
}


# get a cross section data (of ticks) from a Variable =====================
# getCrossSection = filterByTicks


print.Variable = function(x){
  cat("Var.name:",attr(x,"var.name"),"\n")
  print.TickData(x)
}

getherVariable = function(x, ...) {
  UseMethod("getherVariable", x)
}
## 中文中文
## abababab

getherVariable.MultiverseData = function(x, var.name, condition) {

  var.name.list = lapply(x, function(y) {
    colnames(y)
  }) %>% unique()

  if (!var.name %in% var.name.list[[1]]) {
    stop(paste0("No such variable : ", var.name))
  }

  if (missing(condition)){
    data = x
  }else{
    data = subset_.MultiverseData(x,substitute(condition))
  }

  ret = lapply(data, function(y) {
    y[var.name]
  })

  ret2 = TickData(Reduce(cbind, ret),getTicks(data[[1]]))
  

  #colnames(ret2) = paste0(var.name, "_", 1:ncol(ret2))
  #ret2 = tibble::as_tibble(ret2)
  #colnames(ret2) = paste(var.name, (1:ncol(ret2)), sep = "_")
  #ret3 = data.frame(ticks = x[[1]][, 1], ret2)

  #attr(ret2,"ticks") = x[[1]]$ticks
  attr(ret2,"mean") = rowMeans(ret2)
  attr(ret2,"sd") = apply(ret2, 1, sd)
  #attr(ret2,"var.name") = var.name
  #Variable(ret2,var.name = var.name)
  Variable(ret2,var.name)
}




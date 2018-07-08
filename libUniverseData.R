# 每一次NetLogo的运行，就生成一个UniverseData
# 其实就是TickData + paramsSet


UniverseData = function(x, ...){
  UseMethod("UniverseData", x)
}

UniverseData.TickData = function(x, paramsSet){
  attr(x,"paramsSet") = ParamsSet(paramsSet)
  class(x) = unique(c("UniverseData",class(x)))
  x
}


# get one expt from result ==============
getUniverse = function(x, ...) {
  UseMethod("getUniverse", x)
}

# expt.id = 1 ,2, 3 ...
getUniverse.MultiverseData = function(x, expt.id, ...) {
  ret = x[[expt.id]]
  #class(ret) = c(class(ret), "AnExptData")
  ret
}

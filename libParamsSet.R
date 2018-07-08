ParamsSet = function(x){
  class(x) = unique(c("ParamsSet",class(x)))
  x
}

# get and print param set===========================
print.ParamsSet = function(x) {
  ret = data.frame(parameter = names(x), value = unlist(x))
  rownames(ret) = NULL
  # print(filter(ret,var!='random-seed'))
  print(ret)
}

getParamsSet = function(x, ...) {
  UseMethod("getParamsSet", x)
}
getParamsSet.UniverseData = function(x) {
  attr(x, "paramsSet")
}

# gether all value of a params ====
getherParam = function(x,...){
  UseMethod("getherParam",x)
}

getherParam.MultiverseData = function(x, param.name){
  res = lapply(x,function(y){
    ps = getParamsSet(y)
    unlist(ps[param.name])
  }) %>% unlist() 
  attributes(res) = NULL
  res
}

# 模型参数测试

checkParams <- function(nl.path, model.path, exptsList) {
  getNLjarname = function(nl.path){
    all.files = list.files(nl.path)
    all.files[startsWith(all.files,"netlogo") & endsWith(all.files,".jar")]
  }
  
  
  nl.jarname = getNLjarname(nl.path)
  NLStart(nl.path, gui=F, nl.obj=NULL, is3d=FALSE, nl.jarname=nl.jarname)
  
  NLLoadModel(model.path)
  # NLCommand("setup")
  # NLCommand("one-step")
  
  
  a = exptsList[[1]]
  #length(a)
  
  for(i in 2:length(a)){
    var.name = names(a[i])
    value = a[i][[1]]
    NLCommand(paste0("set ",var.name," ",value))
  }
  
  NLQuit()
}


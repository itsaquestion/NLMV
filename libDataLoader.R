# load and recover data from disk

library(dplyr)

# load one expt data
loadResult = function(set.id,output.dir){
  #root.path = getwd()
  output.path = paste0(output.dir,"/",set.id)
  readRDS(paste0(output.path,"/","result.Rds")) %>% 
    MultiverseData
}

# load all expts data with the same expt.name
loadExpt= function(expt.name,output.dir){
  #root.path = getwd()
  results.list = listResults(output.dir)
  
  results.list = results.list[grepl("@",results.list)]
  
  ii = unlist(lapply(str_split(results.list,"@"),function(x){
    x[[1]][1] == expt.name
  }))
  
  
  results.list = results.list[ii]

  result = list()
  
  for(i in 1:length(results.list)){
    expt.path = paste0(output.dir,"/",results.list[i])
   
    if("result.Rds" %in% list.files(expt.path)){
      result = append(result,readRDS(paste0(expt.path,"/result.Rds")))
      #aBlock = readRDS(paste0(expt.path,"/result.Rds"))
      
    }else{
      result = append(result,recoverTmpData(expt.path))
      #aBlock = recoverTmpData(expt.path)
    }
    #result = append(result,aBlock)
  }

  MultiverseData(result,expt.name)
}


# temp file recovery ====

listResults = function(output.dir){
  #output.path = paste0(root.path,"/output/")
  list.dirs(output.dir,full.names = F, recursive = F)
}

recoverTmpData = function(expt.path){
  #expt.path = paste0(output.dir,"/",set.id)
  
  if("result.Rds" %in% list.files(expt.path)){
    warning("\"result.Rds\" exists. Nothing to do.")
    result = readRDS(paste0(expt.path,"/result.Rds"))
    result = addClass(result, "MultiverseData")
    return(result)
  }
  
  tmp.path = paste0(expt.path,"/tmp")
  tmp.files.path = list.files(tmp.path,full.names = T)
  
  result = lapply(tmp.files.path,function(x){
    tmp.file = readRDS(x)
    tmp.file
  })
  result = MultiverseData(result)
  saveRDS(result,file=paste0(expt.path,"/result.Rds"))
  result
}



addClass = function(x,class.name){
  class(x) = unique(c(class.name,class(x)))
  x
}







library(parallel)
library(doSNOW)
#library(tcltk)
library(lubridate)
library(RNetLogo)
library(stringr)



# make param set =============
getNLNewSeed = function() {
  floor(runif(1, -2147483648, 2147483647))
}

makeBaseParamsSet = function(newSeed = getNLNewSeed()) {
  paramsSet = list(`random-seed` = newSeed)
  ParamsSet(paramsSet)
}


# do ====
doExperiments = function(
  
  model.path, exptsList,goFun,
  nl.path ,# = Sys.getenv("NETLOGO_PATH", "/opt/NetLogo_6.0.2/app/"),
  cores = detectCores(),
  expt.name = "unnamed",
  output.dir = "output",
  vars,play.rounds,
  r.source.dir = ""){
  
  # expt.name check
  if(grepl("@",expt.name)){
    stop("expt.name must avoid \"@\".")
  }
  
  
  if(Sys.info()['sysname'] != "Windows"){
    # kill all R process
    system("killall R",ignore.stdout = T,ignore.stderr = T)
  }
  
  
  if(!file.exists(model.path)){stop("Model doesn't exists!")}
  # time as set id, and the folder name for tmp data. 
  set.id = paste0(expt.name,"@",format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  
  # dir for data ==========

  #root.dir = getwd()
  
  
  if (!file.exists(output.dir)){
    dir.create(file.path(output.dir))
  }
  
  data.dir = paste0(output.dir,"/",set.id)
  if (!file.exists(data.dir)){
    dir.create(file.path(data.dir))
  }
  
  tmp.dir = paste0(data.dir,"/tmp")
  if (!file.exists(tmp.dir)){
    dir.create(file.path(tmp.dir))
  }
  
  

  # how many experiments to run
  n.expts = length(exptsList)
  
  
  # ==========================
  prepro <- function(dummy, nl.path, model.path) {
    getNLjarname = function(nl.path){
      all.files = list.files(nl.path)
      all.files[startsWith(all.files,"netlogo") & endsWith(all.files,".jar")]
    }
    # 
    # sourceDir = function(path, trace = TRUE, recursive = T, ...) {
    #   for (nm in list.files(path, pattern = "\\.[RrSsQq]$",recursive = recursive)) {
    #     if(trace) cat(nm)           
    #     source(file.path(path, nm), ...)
    #     if(trace) cat("\n")
    #   }
    # }
    
    
    cat("prepro ...")
    #library(MyUtils)
    library(RNetLogo)
    
    if(r.source.dir!=""){
      MyUtils::sourceDir(r.source.dir)
    }
    
    NLStart(nl.path, gui=F, nl.jarname = getNLjarname(nl.path))
    NLLoadModel(model.path)

  }
  
  postpro <- function(x) {
    NLQuit()
  }
  
  #functions for every cores =====
  setNLParams = function(x, ...){
    UseMethod("setNLParams",x)
  }
  
  setNLParams = function(x){
    NLCommand(paste0("random-seed ",x$`random-seed`))
    vars = names(x)
    vars = vars[! vars %in% "random-seed"]
    if(length(vars) != 0 ){
      sapply(vars,function(var.name){
        value = x[[var.name]]
        NLCommand(paste0("set ",var.name," ",value))
      })
      
    }
  }
  
  # funtions end
 
  # if(Sys.info()['sysname'] == "Windows"){
  #   pb <- tkProgressBar(max=n.expts)
  # }
  
  diff2Second = function(start.time,end.time){
    (as.numeric(end.time - start.time,units="secs"))
  }
  
  formatPeriod= function(sec){
    ds = floor(sec / ((3600 * 24)))
    result = ""
    if(ds > 0){
      result = paste0(ds,"d ")
    }
    result = paste0(result,format(.POSIXct(sec,tz="GMT"), "%H:%M:%S"))
    result
  }
  
  progress <- function(n) {
    # if(Sys.info()['sysname'] == "Windows"){
    #   setTkProgressBar(pb, n)
    # }

    time.passed = formatPeriod(diff2Second(start.time,Sys.time()))
    eta = formatPeriod((n.expts - n) * diff2Second(start.time,Sys.time()) / n)
    
    #setTkProgressBar(pb, n)
    cat(n, "/", n.expts,"=" ,format(round(n/n.expts*100,2),nsmall = 2),"%","| Time used: " ,time.passed,"| eta: ",eta,"\n")
  }
  
  
  cat("Making clusters ... ")
  cl <- makeCluster(cores)
  cat("done \n")
  
  cat("Preparing cores ... ")
  invisible(parLapply(cl, 1:cores, prepro, nl.path=nl.path, model.path=model.path))
  cat("done \n")
  
  cat("Start!\n")
  registerDoSNOW(cl)
  opts <- list(progress=progress)
  
  start.time = Sys.time()
  
  tt = system.time({
    result.par <- foreach(i=1:n.expts, .options.snow=opts) %dopar% {
      setNLParams(exptsList[[i]])
      
      ret = goFun(i,vars,play.rounds) %>% 
        TickData() %>% 
        UniverseData(exptsList[[i]])
      
      saveRDS(ret,paste0(tmp.dir,"/",sprintf("%04d", i),".Rds"))
      
      ret
    }
  })
  
  # save some info
  attr(result.par,"set.id") = set.id
  attr(result.par,"Time.used") = formatPeriod(tt[3])
  attr(result.par,"Avg.time.used") = formatPeriod(round(tt[3]/n.expts,2))
  result.par = MultiverseData(result.par,expt.name)

  
  # print info
  cat("All done!\n\n")
  cat("Total: ",formatPeriod(tt[3]),"\n")
  cat("Avg :" ,formatPeriod(round(tt[3]/n.expts,2)),"\n")
  if(Sys.info()['sysname'] == "Windows"){
    #close(pb)
  }

  invisible(parLapply(cl, 1:cores, postpro))
  stopCluster(cl)
  
 
  
  # save result  
  saveRDS(result.par,paste0(data.dir,"/","result.Rds"))
    
  result.par
}


# x = path of nlogo file
getGlobalsFromFile = function(x){
  txt = readr::read_file(x)
  
  txt.lower = tolower(txt)
  
  # 找globals
  tmpstr = str_sub(txt,str_locate(txt.lower,"globals")[2])
  
  i = str_locate(tmpstr,"\\[")[1]
  j = str_locate(tmpstr,"\\]")[1]
  
  tmpvars = str_sub(tmpstr,i+1,j-1)
  
  # 断行，去掉首尾空格
  a = str_trim(str_split(tmpvars,"\n")[[1]])
  
  # 切掉注释
  pp = ";.*"
  a = sub(pp,"",str_split(a,"\n")) 
  
  # 清理空行
  a = (a[a != ""]) %>% str_trim()
  
  # 中间用空格分开的变量名
  Reduce(c,str_split(a," ")) %>% str_trim()
  
}



nbytes <- function(drname,filelist,arg){
  dirname = drname
  for (f in filelist){
    arg = arg + file.info(f)$size
  }
  
  return (arg)
  
}

rmemptydirs <- function(drname,filelist){
  for (f in 1:length(filelist)){
    folder_size=sum(file.info(list.files(filelist[f], all.files = TRUE, recursive = TRUE))$size)
     if (folder_size == 0 ){
        unlink(filelist[f],recursive = T)
     }
  }
  
}

walk <- function(currdir,f,first){
  if (first ==T) {
    droot <- currdir
  }
  dirlist <- dir(currdir,full.names = T)[file.info(dir(currdir,all.files = FALSE,full.names=T))$isdir]
  f(currdir,dirlist)
  dirlist <- dir(currdir,full.names = T)[file.info(dir(currdir,all.files = FALSE,full.names=T))$isdir]
  for (i in 1:length(dirlist))
  {
    walk(dirlist[i],f,FALSE)
    
  }
  
  setwd(droot)
}

walk(getwd(),rmemptydirs,first = T)
getwd()

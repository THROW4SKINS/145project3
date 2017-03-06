nbytes <- function(drname,filelist,arg){
  
  for (f in filelist){
    arg = arg + file.info(f)$size
  }
  
  return (arg)
  
}

rmemptydirs <- function(drname,filelist,arg){
  
  empty_flst <- ff[file.info(filelist)[["size"]]==0]
  unlink(empty_flst)
}

walk <- function(currdir,f,arg){
  droot <- currdir
  dirlist <- list.dirs(droot,full.names=TRUE,recursive = TRUE)
  for (i in dirlist)
  {
    setwd(i)
    sub_flst <- list.files(path = ".",full.names=TRUE)
    f(sub_flst,arg)
  }
  setwd(droot)
}




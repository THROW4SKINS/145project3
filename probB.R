nbytes <- function(drname,filelist,arg,firstcall){
  if (firstcall == TRUE){
    dirlist<-list.dirs(drname, full.names = TRUE)
    flst<-list.files(drname, full.names = TRUE)
    flst<-flst[!is.element(flst, dirlist)]
    arg = sum(file.info(flst)$size)
  }
  for (f in filelist){
    dirlist<-list.dirs(f, full.names = TRUE)
    flst<-list.files(f, full.names = TRUE)
    flst<-flst[!is.element(flst, dirlist)]
    arg = arg +  sum(file.info(flst)$size)
  }
  
  return (arg)
  
  
  
}

rmemptydirs <- function(drname,filelist,arg,firstcall){
  for (i in filelist){
    print (i)
    fs=sum(file.info(list.files(i, full.names = TRUE))$size)
    print (fs)
    if (fs == 0 ){
      unlink(i,recursive = T)
    }
  }
  return (NA)
  
}

walk <- function(currdir,f,arg,firstcall = TRUE){
  if (firstcall ==T) {
    droot <<- currdir
  }
  dirlist <- list.dirs(path = currdir, full.names = TRUE)
  f(currdir,dirlist,arg,firstcall)
  for (i in dirlist)
  {
    walk(i,f,arg,first = FALSE)
    
  }
  setwd(droot)
  
}
path <- " "
walk(path,rmemptydirs,NA,TRUE)
sms <<- 0
walk(path,nbytes,sms,TRUE)
print (sms)
getwd()


nbytes <- function(drname,filelist,arg,firstcall){
  if (firstcall == TRUE){
    arg <<- sum(file.info(list.files(drname, full.names = TRUE))$size)
  }
  for (f in filelist){
    arg = arg +  sum(file.info(list.files(drname, full.names = TRUE))$size)
  }
  
    print (arg)
  
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

walk(getwd(),rmemptydirs,NA,TRUE)
getwd()

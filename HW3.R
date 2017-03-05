library(pixmap)

con_check <- function(pos, m_check, consec){
  m_check[pos] = m_check[pos] + 1
  if(any(rowSums(m_check)>=consec || colSums(m_check)>=consec)){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

ifnotprime <- function(num){
  if(num == 2){
    return(FALSE)
  }
  else if(any(num %% 2:(num-1) == 0)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

secretencoder <- function(imgfilename, msg, startpix, stride, consec = NULL){
  if(ifnotprime(stride)){
    warning("Stride is not a prime number. Recommended prime number")
  }
  #tryCatch(read.pnm(imgfilename),error={print("File does not exist")})
  org_img <- read.pnm(imgfilename)
  grey_img <- org_img@grey
  img_len = length(grey_img)
  msg_code = utf8ToInt(msg)
  msg_code = append(msg_code, 0)
  msg_code = msg_code/128
  msg_code_len = length(msg_code)
  if(is.null(consec)){
    grey_pos = seq(startpix, msg_code_len*stride + startpix - 1, stride)
    while(any(grey_pos > img_len)){
      too_large = which(grey_pos > img_len)
      grey_pos[too_large] = grey_pos[too_large] - img_len
    }
    grey_img[grey_pos] = msg_code
  }
  else{
    consec_check = matrix(0L, nrow = dim(m1)[1], ncol = dim(m1)[2])
    grey_pos <- vector(length=msg_code_len)
    counter = 1
    posit = startpix
    while(counter <= msg_code_len){
      if(con_check(posit, consec_check, consec)){
        consec_check[posit] = consec_check[posit] + 1
        grey_pos[counter] = posit
        posit = posit + stride
        counter = counter + 1
      }
      else{
        posit = posit + stride
      }
      if((posit-img_len) >= startpix){
        stop("Insufficient room for the message.")
      }
    }
    if(any(grey_pos > img_len)){
      too_large = which(grey_pos > img_len)
      grey_pos[too_large] = grey_pos[too_large] - img_len
    }
    grey_img[grey_pos] = msg_code
  }
  alt_img <- org_img
  alt_img@grey <- grey_img
  print(grey_pos)
  return(alt_img)
}


secretdecoder <- function(imgfilename,startpix,stride,consec=NULL){
  msg_img = read.pnm(imgfilename)
  msg = vector()
  grey_img <- msg_img@grey
  img_len = length(grey_img)
  posit = startpix
  counter = 0
  if(is.null(consec)){
    while(counter == 0){
      if(posit > img_len){
        posit = posit - img_len
      }
      num = grey_img[posit]
      if(num == 0){
        counter = 1
        break
      }
      msg = append(msg,num)
      posit = posit + stride
    }
  }
  else{
    consec_check = matrix(0L, nrow = dim(m1)[1], ncol = dim(m1)[2])
    while(counter == 0){
      if(posit > img_len){
        posit = posit - img_len
      }
      if(con_check(posit, consec_check, consec)){
        consec_check[posit] = consec_check[posit] + 1
        num = grey_img[posit]
        if(num == 0){
          counter = 1
          break
        }
        msg = append(msg,num)
        posit = posit + stride
      }
      else{
        posit = posit + stride
      }
    }
  }
  #msg_str = intToUtf8((msg*128))
  return(msg)
}


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

c_check <- function(pos, r_check, c_check, dims, cons){
  rc = floor(pos/dims[2]) + 1
  cc = floor(pos/dims[1]) + 1
  if(r_check[rc]+1 > cons){
    return(FALSE)
  }
  else if(c_check[cc]+1 > cons){
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
  d_img = dim(grey_img)
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
    #consec_check = matrix(0L, nrow = dim(grey_img)[1], ncol = dim(grey_img)[2])
    #consec_check = array(0L, dim(grey_img))
    looper = 0
    row_check = rep(0,d_img[1])
    col_check = rep(0,d_img[2])
    grey_pos <- vector(length=msg_code_len)
    counter = 1
    posit = startpix
    while(counter <= msg_code_len){
      if(c_check(posit, row_check, col_check, d_img, consec)){
      #if(con_check(posit, consec_check, consec)){
        #consec_check[posit] = consec_check[posit,drop=FALSE] + 1
        #consec_check = replace(consec_check, posit, consec_check[posit]+1)
        row_check[floor(posit/d_img[2])] = row_check[floor(posit/d_img[2]) + 1] + 1
        col_check[floor(posit/d_img[1])] = col_check[floor(posit/d_img[1]) + 1] + 1
        #row_check = replace(row_check, row_check[floor(posit/d_img[2])]+1)
        #col_check = replace(col_check, col_check[floor(posit/d_img[1])]+1)
        grey_pos[counter] = posit
        posit = posit + stride
        counter = counter + 1
      }
      else{
        posit = posit + stride
      }
      if(posit > img_len){
        posit = posit - img_len
        looper = looper + 1
      }
      if(looper == 2){
        stop("Insufficient room for the message.")
      }
    }
    #if(any(grey_pos > img_len)){
    #  too_large = which(grey_pos > img_len)
    #  grey_pos[too_large] = grey_pos[too_large] - img_len
    #}
    grey_img[grey_pos] = msg_code
  }
  alt_img <- org_img
  alt_img@grey <- grey_img
  return(alt_img)
}


secretdecoder <- function(imgfilename,startpix,stride,consec=NULL){
  #msg_img = read.pnm(imgfilename)
  msg_img = imgfilename
  msg = vector()
  grey_img <- msg_img@grey
  d_img = dim(grey_img)
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
    row_check = rep(0,d_img[1])
    col_check = rep(0,d_img[2])
    #consec_check = matrix(0L, nrow = dim(grey_img)[1], ncol = dim(grey_img)[2])
    while(counter == 0){
      if(posit > img_len){
        posit = posit - img_len
      }
      if(c_check(posit, row_check, col_check, d_img, consec)){
      #if(con_check(posit, consec_check, consec)){
        #consec_check[posit] = consec_check[posit] + 1
        row_check[floor(posit/d_img[2])] = row_check[floor(posit/d_img[2]) + 1] + 1
        col_check[floor(posit/d_img[1])] = col_check[floor(posit/d_img[1]) + 1] + 1
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
  msg_str = intToUtf8((msg*128))
  return(msg_str)
}


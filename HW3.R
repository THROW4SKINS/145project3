library(pixmap)

#c_check will be responsible for checking if the total row and columns within consec.
#c_check takes the position being added in. r_check will be the vector for rows. c_check the vector for columns
#dims is the dimension of the picture matrix. cons will be consec.
c_check <- function(pos, r_check, c_check, dims, cons){

  #rc will be the position for rows and cc will be position for columns
  #rc divides the checking position against number of columns and cc is the reverse.
  #Add one because unlike python, we start at 1 instead of 0.
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

#Check for NOT prime number
ifnotprime <- function(num){
  if(num == 2){
    return(FALSE)
  }
  #Num will mod all the numbers from 2 to num-1. If any is 0, then it is not a prime
  else if(any(num %% 2:(num-1) == 0)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

secretencoder <- function(imgfilename, msg, startpix, stride, consec = NULL){
  #Give warning if stride is not prime
  if(ifnotprime(stride)){
    warning("Stride is not a prime number. Recommended prime number")
  }
  #Check if file exist. Stop if it does
  tryCatch(org_img <- read.pnm(imgfilename),error=function(e){stop("File does not exist")})
  
  #Set up the image matrix, and message number, and stuff
  grey_img <- org_img@grey
  img_len = length(grey_img)
  d_img = dim(grey_img)
  msg_code = utf8ToInt(msg)
  msg_code = append(msg_code, 0)
  msg_code = msg_code/128
  msg_code_len = length(msg_code)
  
  #If the message length is greater then the amount of pixels we have, then throw error for not enough room
  if(msg_code_len > img_len){
    stop("Insufficient room for the message.")
  }
  
  #If consec is null, we do this loop.
  if(is.null(consec)){
    #seq will give the numbers from the starting pixel, up by stride, till the number of message code
    grey_pos = seq(startpix, msg_code_len*stride + startpix - 1, stride)
    
    #If any position is greater than the maximum image length, that means we loop around, so we subtract by max img length
    while(any(grey_pos > img_len)){
      too_large = which(grey_pos > img_len)
      grey_pos[too_large] = grey_pos[too_large] - img_len
    }
    #Set all the position in that image with our code
    grey_img[grey_pos] = msg_code
  }
  #If we have consec
  else{
    #Row vector and column vector for consec
    row_check = rep(0,d_img[1])
    col_check = rep(0,d_img[2])
    #Create empty vector for our positions in the pictures to change
    grey_pos <- vector(length=msg_code_len)
    counter = 1
    posit = startpix
    #While we did not finish getting the positions for our messages
    while(counter <= msg_code_len){
      #Check if the rows and columns are less than consec. If it is, we pass, if not, we add in!
      if(c_check(posit, row_check, col_check, d_img, consec)){
        row_check[floor(posit/d_img[2])] = row_check[floor(posit/d_img[2]) + 1] + 1
        col_check[floor(posit/d_img[1])] = col_check[floor(posit/d_img[1]) + 1] + 1
        grey_pos[counter] = posit
        posit = posit + stride
        counter = counter + 1
      }
      #Here we pass, so next position
      else{
        posit = posit + stride
      }
      #Loop around
      if(posit > img_len){
        posit = posit - img_len
      }
      #Now if we don't have enough pixel within consec requirement, throw insuff error.
      if(sum(row_check) > (length(row_check)*consec) || sum(col_check) > (length(col_check)*consec)){
        stop("Insufficient room for the message.")
      }
    }
    grey_img[grey_pos] = msg_code
  }
  alt_img <- org_img
  alt_img@grey <- grey_img
  return(alt_img)
}


secretdecoder <- function(imgfilename,startpix,stride,consec=NULL){
  #Read in file and set up stuff.
  msg_img = read.pnm(imgfilename)
  msg = vector()
  grey_img <- msg_img@grey
  d_img = dim(grey_img)
  img_len = length(grey_img)
  posit = startpix
  counter = 0
  #Consec = Null
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
  #Consec is not null
  else{
    #Set up our checker
    row_check = rep(0,d_img[1])
    col_check = rep(0,d_img[2])
    while(counter == 0){
      if(posit > img_len){
        posit = posit - img_len
      }
      #Get our position. Same idea as the encoder, our positions will have to meet up consec requirement
      if(c_check(posit, row_check, col_check, d_img, consec)){
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
  #Use round because of stupid bits problem that makes the pixels different after reading and writing
  msg_str = intToUtf8(round(msg*128))
  return(msg_str)
}


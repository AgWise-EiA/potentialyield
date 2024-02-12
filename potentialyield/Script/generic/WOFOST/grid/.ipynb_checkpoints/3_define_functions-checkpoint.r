#create a new folder if it does not exist
wdif<-function(x){if(!dir.exists(x))dir.create(x)}

#easy viewing of corners of a data.frame
topleft<-function(x, rows=6, cols=rows){
  x[1:rows, 1:cols]
  }
topright<-function(x, rows=6, cols=rows){
  m<-ncol(x)
  x[1:rows, (m-cols+1):m]
  }
bottomleft<-function(x, rows=6, cols=rows){
  n<-nrow(x)
  x[(n-rows+1):n, 1:cols]
}
bottomright<-function(x, rows=6, cols=rows){
  n<-nrow(x)
  m<-ncol(x)
  x[(n-rows+1):n, (m-cols+1):m]
}

#check if x is even
is.even<-function(x){x %% 2 == 0}

#souse specific lines in r script
##author: https://gist.github.com/christophergandrud/1eb4e095974204b12af9
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))}




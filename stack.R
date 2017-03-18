newstack <- function() {
  st <- new.env(parent=globalenv())  
  st$data <- c(NA)
  class(st) <- append(class(st), "stack")
  return(st)
}

push <- function(obj, val) UseMethod("push") 
pop <- function(obj, val) UseMethod("pop") 

push.stack <- function(obj,val) { 
  if(is.na(val)) stop("Invalid type")
  obj$data <- c(obj$data,val) 
}

pop.stack <- function(obj) {
  if(length(obj$data) == 1)
    stop("Stack is empty")
  end <- length(obj$data)
  top <- obj$data[end]
  obj$data <- obj$data[-end]
  return(top)
}

print.stack <- function(obj){
  if(length(obj$data) == 1) return()
  print(obj$data)
}

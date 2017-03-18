
# make to take input
newqueue <- function() {
  q <- new.env(parent=globalenv()) 
  q$data <- c(NA)
  class(q) <- append(class(q), "queue")
  return(q)
}

push <- function(obj, val) UseMethod("push") 
pop <- function(obj, val) UseMethod("pop") 

push.queue <- function(obj,val) { 
  if(is.na(val)) stop("Invalid type")
  obj$data <- c(obj$data,val) 
}

pop.queue <- function(obj) {
  if(length(obj$data) == 1)
    stop("Queue is empty")
  front <- obj$data[2]
  obj$data <- obj$data[-2]
  return(front)
}


print.queue <- function(obj){
  if(length(obj$data) == 1) return()
  print(obj$data)
}

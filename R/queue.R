#' Constructor for queue class object
#'
#' \code{newqueue} returns the instantiated newqueue object
#'
#' This constructor implementation involves instantiating a new
#' Environment with each new instance of a queue, along with the
#' class attribute appended with the name of the structure.
#' 
#' @examples
#' q <- newqueue()
#'
#' @export
newqueue <- function() {
  q <- new.env(parent=globalenv()) 
  q$data <- c(NA)
  class(q) <- append(class(q), "queue")
  return(q)
}


#' Inserts value after the current last element in the vector
#'
#' \code{push} returns the modified vector
#'
#' This is a generic function: methods can be defined for it
#' directly. For this to work properly, the arguments
#' \code{obj} and \code{val} should be a newqueue (Environment)
#' object and a character/numeric vector, respectively.
#' 
#' @param obj An Environment object.
#' @param val Numeric or character vectors.
#' 
#' @return If all inputs are integer, then the returned vector
#' will be a vector of integer values. If all inputs are integer and
#' character values, then the integers will be converted to
#' characters and the returned vector will be a vector of characters.
#'
#' @examples
#' push(q,5)
#' push(q,18)
#' push(q,"Hello")
#' push(q,100)
#' push(q,"Give me 100 cookies!")
#'
#' @export
push <- function(obj, val) UseMethod("push")


#' Removes first element from the vector and returns that value
#'
#' \code{pop} returns the popped value
#'
#' This is a generic function: methods can be defined for it
#' directly. For this to work properly, the arguments
#' \code{obj} should be a newqueue (Environment) object.
#' 
#' @param obj An Environment class object.
#' 
#' @return If all previously pushed inputs were integer, then the
#' returned value will be a numeric vector. If all previously
#' pushed inputs were integer and character values, then the
#' returned value will be a character vector.
#'
#' @examples
#' pop(q)
#'
#' @export
pop <- function(obj) UseMethod("pop") 


#' Inserts value after the current last element in the vector
#'
#' \code{push} returns the modified vector
#'
#' This is the function that the generic function \code{push} will
#' dispatch to when called.
#' 
#' @param obj An Environment object.
#' @param val Numeric or character vectors.
#' 
#' @return If all inputs are integer, then the returned vector
#' will be a vector of integer values. If all inputs are integer and
#' character values, then the integers will be converted to
#' characters and the returned vector will be a vector of characters.
#'
#' @examples
#' push(q,5)
#' push(q,18)
#' push(q,"Hello")
#' push(q,100)
#' push(q,"Give me 100 cookies!")
#'
#' @export
push.queue <- function(obj,val) { 
  if(is.na(val)) stop("Invalid type")
  obj$data <- c(obj$data,val) 
}


#' Removes first element from the vector and returns that value
#'
#' \code{pop} returns the popped value
#'
#' This is the function that the generic function \code{pop} will
#' dispatch to when called.
#' 
#' @param obj An Environment class object.
#' 
#' @return If all previously pushed inputs were integer, then the
#' returned value will be a numeric vector. If all previously
#' pushed inputs were integer and character values, then the
#' returned value will be a character vector.
#'
#' @examples
#' pop(q)
#'
#' @export
pop.queue <- function(obj) {
  if(length(obj$data) == 1)
    stop("Queue is empty")
  front <- obj$data[2]
  obj$data <- obj$data[-2]
  return(front)
}


#' Prints out the queue
#'
#' \code{print} will return nothing if the queue is empty (if the
#' queue only has the value NA in it).
#' 
#' @param obj An Environment class object.
#' 
#' @return If all previously pushed inputs were integer, then the
#' printed value will be a numeric vector. If all previously
#' pushed inputs were integer and character values, then the
#' printed value will be a character vector.
#'
#' @examples
#' print(q)
#'
#' @export
print.queue <- function(obj){
  if(length(obj$data) == 1) return()
  print(obj$data[-1])
}

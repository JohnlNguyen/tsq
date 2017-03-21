#' Constructor for stack class object
#'
#' \code{newstack} returns the instantiated stack object
#'
#' This constructor implementation involves instantiating a new
#' Environment with each new instance of a stack, along with the
#' class attribute appended with the name of the structure.
#' 
#' @examples
#' s <- newstack()
#'
#' @export
newstack <- function() {
  st <- new.env(parent=globalenv())  
  st$data <- c(NA)
  class(st) <- append(class(st), "stack")
  return(st)
}


#' Inserts value after the current last element in the vector
#'
#' \code{push} returns the modified vector
#'
#' This is a generic function: methods can be defined for it
#' directly. For this to work properly, the arguments
#' \code{obj} and \code{val} should be a stack (Environment)
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
#' push(s,5)
#' push(s,18)
#' push(s,"Hello")
#' push(s,100)
#' push(s,"Give me 100 cookies!")
#'
#' @export
push <- function(obj, val) UseMethod("push") 


#' Removes last element from the vector and returns that value
#'
#' \code{pop} returns the popped value
#'
#' This is a generic function: methods can be defined for it
#' directly. For this to work properly, the argument
#' \code{obj} should be a stack (Environment) object.
#' 
#' @param obj An Environment class object.
#' 
#' @return If all previously pushed inputs were integer, then the
#' returned value will be a numeric vector. If all previously
#' pushed inputs were integer and character values, then the
#' returned value will be a character vector.
#'
#' @examples
#' pop(s)
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
push.stack <- function(obj,val) { 
  if(is.na(val)) stop("Invalid type")
  obj$data <- c(obj$data,val) 
}


#' Removes last element from the vector and returns that value
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
#' pop(s)
#'
#' @export
pop.stack <- function(obj) {
  if(length(obj$data) == 1)
    stop("Stack is empty")
  end <- length(obj$data)
  top <- obj$data[end]
  obj$data <- obj$data[-end]
  return(top)
}


#' Prints out the stack
#'
#' \code{print} will return nothing if the queue is empty (if the
#' stack only has the value NA in it).
#' 
#' @param obj An Environment class object.
#' 
#' @return If all previously pushed inputs were integer, then the
#' printed value will be a numeric vector. If all previously
#' pushed inputs were integer and character values, then the
#' printed value will be a character vector.
#'
#' @examples
#' print(s)
#'
#' @export
print.stack <- function(obj){
  if(length(obj$data) == 1) return()
  print(obj$data[-1])
}

\name{newbintree,push,pop,helper,push.bintree,print.bintree}
\alias{newbintree}
\alias{push}
\alias{pop}
\alias{helper}
\alias{push.bintree}
\alias{print.bintree}

\title{S3 Class Binary Tree Implementation}

\description{
	Binary Tree Class with print, push, pop methods
}

\usage{
	newbintree()
	push(obj,val)
	pop(obj,val)
	helper(root,inVal,graph)
	push.bintree(obj,val)
	print.bintree(tree)
}

\arguments{
	\item{obj}{A newbintree class object to be modified.}
	\item{val}{The primitive value to be pushed or popped.}
	\item{root}{The row of the node.}
	\item{inVal}{The primitive value to be pushed or popped.
		Same as \code{val} but only used in helper function.}
	\item{graph}{The newbintree class object.
		Same as \code{obj} but only used in helper function to check for existing children.}
	\item{tree}{The newbintree class object to be printed.}
}

\details{
	Implements a S3 class version of a Binary Tree in R with print,
	push and pop methods.

	This implementation involves instantiating a new Environment with each
	new instance of a binary tree, along with the class attribute appended
	with the name of the structure.

	In our implementation the binary tree is stored as a matrix, where each row
	is represented as a node with the value, row number of the its left child,
	and row number of its right child as its column values, respectively.

	Note the comments above \code{helper} that explain what how the value, left
	child row, and right child row are accessed in the helper function.

	A note on NA values: The value of NA in the binary tree represents:
	1. if \code{val} = NA, there is no value in that row (this will only be the case
	when the tree is instantiated)
	2. if \code{left} = NA, then the node in that row has no left child
	3. if \code{right} = NA, then the node in that row has no right child

	Note that in \code{push.bintree}, when the tree is empty the first value
	pushed has its left and right children sent to NA. The function \code{helper}
	is always called as long as the tree is not empty, and performs a recursive
	search to find the correct spot to insert in the binary tree.
}

\value{The following items are returned by the methods of our binary tree:
	
	\itemize{
		
		\item \code{newbintree}, the instantiated newbintree object

		\item \code{helper}, the matrix in which our binary tree is stored

		\item \code{push.bintree}, the new version of matrix in which our
		binary tree is stored

		\item \code{pop.bintree}, the smallest element that was removed

	}
}

\examples{
	
	# Initialize and set up our binary tree
	newbintree()

	# attempt to pop empty tree, will result in warning
	pop(tree)

	# push values into the tree
	push(tree,7)
	push(tree,5)
	push(tree,6)
	push(tree,9)

	# pop the smallest value, which returns 5 and removes it from the tree
	pop(tree)

	# push more values into the tree
	push(tree,4)
	push(tree,1)
	push(tree,30)
	push(tree,8)
	push(tree,10)

	# print out the binary tree, should return this:
	# 
	print(tree)
}

######## TEST CASE #################
newbintree()
push(tree,7)
push(tree,5)
push(tree,6)
push(tree,9)
push(tree,4)
push(tree,1)
push(tree,30)
push(tree,8)
push(tree,10)

newbintree <- function(){
  tree <- new.env(parent=globalenv()) 
  tree$vals <- matrix(cbind(NA,NA,NA),nrow=1,ncol=3, dimnames = list(c(NULL),
                                                      c("Value","Left", "Right")) ) 
  colnames(tree, do.NULL = FALSE)
  class(tree) <- append(class(tree),"bintree")
  return(tree)
}

push <- function(obj, val) UseMethod("push")
pop <- function(obj, val) UseMethod("pop")

# val <- graph[root,1]
# left <- graph[root,2]
# right <- graph[root,3]
# root <- the row of the node
helper <- function(root, inVal ,graph){
  if(inVal < graph[root,1] & is.na(graph[root,2])){ #BASE CASE
    graph <- rbind(graph,c(inVal,NA,NA))
    graph[root,2] <- nrow(graph)
    return(graph)
  }
  if(inVal > graph[root,1] & is.na(graph[root,3])){ #BASE CASE
    graph <- rbind(graph,c(inVal,NA,NA))
    graph[root,3] <- nrow(graph)
    return(graph)
  }
  if(inVal < graph[root,1]) helper(graph[root,2],inVal,graph)

  else helper(graph[root,3],inVal,graph)
}

push.bintree <- function(obj,val) {
     if(nrow(obj$vals) == 1){ # Empty Tree
        obj$vals <- rbind(obj$vals,c(val,NA,NA))
     }
     else obj$vals <- helper(2,val,obj$vals)
     return(obj$vals)
}

pop.bintree <- function(obj) {
	if(nrow(obj$vals) == 1){ # Empty Tree
		stop("Error: attempt to pop empty tree")
	}
	else value <- pop_helper(2,obj$vals)
	return(value)
}

pop_helper <- function(root, graph){
	if(graph[root,1] & is.na(graph[root,2])) { #BASE CASE
		graph[root,1] <- 
	}
	else pop_helper(graph[root,2])
}

print.bintree <- function(tree) {
  obj <- tree$vals
  printhelper <- function(obj, row) {
    if(!is.na(obj[as.numeric(row),2])) {
      printhelper(obj,obj[as.numeric(row),2])
    }
    print(obj[as.numeric(row),1])
    if(!is.na(obj[as.numeric(row),3])) {
      printhelper(obj,obj[as.numeric(row),3])
    }
  }
  if(nrow(obj) > 1) printhelper(obj,2)
}

######## TEST CASE #################
newbintree()
push(tree,7)
push(tree,5)
push(tree,6)
push(tree,9)
push(tree,4)
push(tree,1)
push(tree,30)
push(tree,8)
push(tree,10)


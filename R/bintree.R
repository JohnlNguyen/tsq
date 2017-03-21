#' Constructor for binary tree class object
#'
#' \code{newbintree} returns the instantiated newbintree object
#'
#' This constructor implementation involves instantiating a new
#' Environment with each new instance of a binary tree, along with the
#' class attribute appended with the name of the structure.
#' 
#' @return The instantiated newbintree object.
#'
#' @examples
#' q <- newbintree()
#'
#' @export
newbintree <- function(){
  tree <- new.env(parent=globalenv()) 
  tree$vals <- matrix(cbind(NA,NA,NA,NA),nrow=1,ncol=4, dimnames = list(c(NULL),
                                                      c("Value","Left", "Right", "Count")) ) 
  colnames(tree, do.NULL = FALSE)
  class(tree) <- append(class(tree),"bintree")
  return(tree)
}


#' Inserts value into the tree following binary tree order
#'
#' \code{push} returns the modified matrix
#'
#' This is a generic function: methods can be defined for it
#' directly. For this to work properly, the arguments
#' \code{tree} and \code{val} should be a newbintree (Environment)
#' object and a character/numeric vector, respectively.
#' 
#' @param tree An Environment object.
#' @param val Numeric or character vectors.
#' 
#' @return If all inputs are integer, then the returned vector
#' will be a matrix of numeric values. If all inputs are integer and
#' character values, then the integers will be converted to
#' characters and the returned matrix will be a matrix of characters.
#'
#' @examples
#' push(tree,5)
#' push(tree,"hi there")
#'
#' @export
push <- function(tree, val) UseMethod("push")

#' Removes the smallest value from the matrix and returns that value
#'
#' \code{push} returns the popped value
#'
#' This is a generic function: methods can be defined for it
#' directly. For this to work properly, the arguments
#' \code{tree} should be a newbintree (Environment) object.
#' 
#' @param tree An Environment object.
#' 
#' @return If all inputs are integer, then the returned vector
#' will be a matrix of numeric values. If all inputs are integer and
#' character values, then the integers will be converted to
#' characters and the returned matrix will be a matrix of characters.
#'
#' @examples
#' pop(tree)
#'
#' @export
pop <- function(tree) UseMethod("pop")


#' Inserts value into the tree following binary tree order
#'
#' \code{push} returns the modified matrix
#'
#' This is the function that the generic function \code{push} will
#' dispatch to when called.
#' 
#' @param tree An Environment object.
#' @param val Numeric or character vectors.
#' 
#' @return If all inputs are integer, then the returned vector
#' will be a matrix of numeric values. If all inputs are integer and
#' character values, then the integers will be converted to
#' characters and the returned matrix will be a matrix of characters.
#'
#' @examples
#' push(tree,5)
#' push(tree,"hi there")
#'
#' @export
push.bintree <- function(tree,val) {
  if(nrow(tree$vals) == 1){ # Empty Tree
    tree$vals <- rbind(tree$vals,c(val,NA,NA,1))
  }
  if(nrow(tree$vals) > 1) { # Check for null root (also an empty tree)
    if(is.na(tree$vals[2,1])){
      tree$vals[2,] <- c(val,NA,NA,1)
    }
  }
  # check if converting from numeric tree to char tree
  if(is.numeric(tree$vals[2,1]) && is.character(val)) {
    warning("Inserting char will cause previous values to convert to char.")
  }
  tree$vals <- push_helper(2,val,tree$vals)
}


#' Inserts value into the tree following binary tree order
#'
#' \code{push_helper} returns the modified matrix
#'
#' This is the helper function that the function \code{push} will call
#' to insert the value into the binary tree.
#'
#' @param root The row to begin the tree traversal.
#' @param inVal Numeric or character vectors.
#' @param graph The matrix to be modified.
#' 
#' @return The modified matrix after the insertion.
#'
#' @examples
#' push_helper(2,val,tree$vals)
#'
#' @export
push_helper <- function(root, inVal, graph){
  root <- as.numeric(root)
  # base case, insert as left child
  if(inVal < graph[root,1] & is.na(graph[root,2])){
    rowToInsert <- push_firstNArow(graph) # find avail row
    if(rowToInsert == -1) { # create new row
      graph <- rbind(graph,c(inVal,NA,NA,1))
      graph[root,2] <- nrow(graph) # update parent node's left child value
    } else {
      graph[rowToInsert,] <- c(inVal,NA,NA,1) # put val in found NA row
      graph[root,2] <- rowToInsert
    }
    return(graph)
  }
  # base case, insert as right child
  if(inVal > graph[root,1] & is.na(graph[root,3])){ # base case, insert to right
    rowToInsert <- push_firstNArow(graph) # find avail row
    if(rowToInsert == -1) { # create new row
      graph <- rbind(graph,c(inVal,NA,NA,1))
      graph[root,3] <- nrow(graph) # update parent node's right child value
    } else {
      graph[rowToInsert,] <- c(inVal,NA,NA,1) # put val in found NA row
      graph[root,3] <- rowToInsert
    }
    return(graph)
  }
  # base case, duplicate item
  if(inVal == graph[root,1]) {
    graph[root,4] <- as.numeric(graph[root,4]) + 1 # increment count
    return(graph)
  }
  # search left tree
  if(inVal < graph[root,1]) push_helper(graph[root,2],inVal,graph)
  # search right tree
  else push_helper(graph[root,3],inVal,graph)
}


#' Finds and returns the first row of NA's in the matrix
#'
#' \code{push_firstNArow} returns the first row of NA's
#'
#' This is the helper function that the function \code{push_helper}
#' will call to find the first available row to insert the value
#' for either base case.
#'
#' @param graph The matrix to be searched.
#' 
#' @return The row in the matrix containing the first row of NA's
#'
#' @examples
#' push_firstNArow(graph)
#'
#' @export
push_firstNArow <- function(graph) {
  for(i in 2:nrow(graph)) {
    if(all(is.na(graph[i,]))) return(i)
  }
  return(-1) # all rows taken, return -1
}


#' Removes the smallest value from the matrix and returns that value
#'
#' \code{push} returns the popped value
#'
#' This is the function that the generic function \code{pop} will
#' dispatch to when called.
#' 
#' @param tree An Environment object.
#' 
#' @return The popped value.
#'
#' @examples
#' pop(tree)
#'
#' @export
pop.bintree <- function(tree) {
  if(nrow(tree$vals) < 2 || is.na(tree$vals[2,1])) stop("Tree is empty")
  if(is.na(tree$vals[2,2])) { # checks if root is smallest item (no left subtree)
    if(is.na(tree$vals[2,3])) { # checks if there is only one item in tree (no right subtree)
      top <- tree$vals[2,1]
      if(tree$vals[2,4] == 1) tree$vals[2,] <- NA # check if duplicates remain
      else tree$vals[2,4] <- as.numeric(tree$vals[2,4]) - 1
      names(top) <- NULL
      return(top)
    }
    if(tree$vals[2,4] == 1) { # move right subtree to root (row 2 in matrix)
      newRoot <- as.numeric(tree$vals[2,3])
      top <- tree$vals[2,1]
      tree$vals[2,] <- tree$vals[newRoot,]
      tree$vals[newRoot,] <- NA
      names(top) <- NULL
      return(top)
    } else { # duplicates remain, decrement count
      top <- tree$vals[2,1]
      tree$vals[2,4] <- as.numeric(tree$vals[2,4]) - 1
      names(top) <- NULL
      return(top)
    }
  }
  pop_helper(tree, tree$vals[2,2], 2)

}


#' Removes the smallest value in the tree and returns it
#'
#' \code{pop_helper} returns the modified matrix
#'
#' This is the helper function that the function \code{pop} will call
#' to remove the smallest value from the binary tree.
#'
#' @param tree The matrix to be modified.
#' @param row The row to searching for the smallest element.
#' @param parent The parent row.
#' 
#' @return The popped value.
#'
#' @examples
#' push_helper(tree,tree$vals[2,2],2)
#'
#' @export
pop_helper <- function(tree, row, parent) {
  row <- as.numeric(row)
  parent <- as.numeric(parent)
  # base case, found smallest element
  if(is.na(tree$vals[row,2])) { 
    top <- tree$vals[row,1] # value to return
    # next we check for duplicates and whether to move right subtree up
    if(tree$vals[row, 4] == 1) { # no duplicates remaining
      rTree <- tree$vals[row,3]
      if(!is.na(rTree)) { # right subtree exists
        tree$vals[parent, 2] <- rTree # update parent to new subtree
        tree$vals[row,] <- NA
      } else {
        tree$vals[parent,2] <- NA
        tree$vals[row,] <- NA
      }
      return(top)
    } else { # duplicate remain, decrement count
      tree$vals[row, 4] <- as.numeric(tree$vals[row, 4]) - 1
    }
    return(top)
  }
  pop_helper(tree, tree$vals[as.numeric(row),2], row)
}

#' Prints out the binary tree
#'
#' \code{print} will print nothing if the binary tree is empty.
#' 
#' @param tree An Environment class object.
#' 
#' @return If all inputs are integer, then the returned vector
#' will be a matrix of numeric values. If all inputs are integer and
#' character values, then the integers will be converted to
#' characters and the returned matrix will be a matrix of characters.
#'
#' @examples
#' print(tree)
#'
#' @export
print.bintree <- function(tree) {
  obj <- tree$vals
  colnames(obj) <- NULL
  printhelper <- function(obj, row) {
    if(!is.na(obj[as.numeric(row),2])) {  # print left tree
      printhelper(obj,obj[as.numeric(row),2])
    }
    for(i in 1:obj[as.numeric(row),4]) print(obj[as.numeric(row),1]) # print root
    if(!is.na(obj[as.numeric(row),3])) {  # print right subtree
      printhelper(obj,obj[as.numeric(row),3])
    }
  }
  if(nrow(obj) > 1) {
    if(!is.na(obj[2,1])) printhelper(obj,2)
  }
}


